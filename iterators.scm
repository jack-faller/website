(define-module (iterators)
  #:export ( ;; Primitive creation.
			iterator iterator?
			iter-null iter-null?
			iterate iterate*
			iterator-defer! iterator-replace!

			;; Access.
			iter-next iter-peek

			;; Creation.
			list->iter repeat-list->iter pairs->iter
			string->iter repeat-string->iter port->char-iter
			vector->iter repeat-vector->iter
			iter-forever iter-repeat
			tree->iter

			;; Manipulation.
			iter-map iter-filter iter-delete-if
			iter-delete iter-delq iter-delv
			iter-take iter-drop iter-flatten iter-recursive-flatten
			iter-cons

			;; Collection.
			iter->string iter->list iter->vector
			iter-for-each iter-run
			iter-fold))

(import (srfi 6)) ;; String ports.
(import (srfi 9)) ;; Records.
(import (srfi 16)) ;; Case lambda.
;; Can't import in guile.
;; (import (srfi 46)) ;; Syntax rules using different ellipsis.

(define-record-type <iterator>
  (make-iterator next-function)
  iterator?
  (next-function iterator-next-function set-iterator-next-function!))

(define-syntax iterator
  (syntax-rules ()
	((_ () body ...)
	 (iterator ignored-1 (ignored-2) body ...))
	((_ (this) body ...)
	 (iterator ignored-1 (this) body ...))
	((_ name () body ...)
	 (iterator name (ignored-2) body ...))
	((_ recur (this) body ...)
	 (make-iterator
	  (letrec ((function (lambda (this) (let recur () body ...))))
		function)))))
(define-syntax define-syntax*
  (syntax-rules ()
	((_ (name rules ...) ...)
	 (begin (define-syntax name (syntax-rules () rules ...)) ...))
	;; SRFI-46 custom ellipses.
	((_ ident (name rules ...) ...)
	 (begin (define-syntax name (syntax-rules ident () rules ...)) ...))))
(define-syntax*
  (apply-to-iterators
   ((_ f ... ()) (f ...))
   ((_ f ... (first rest ...))
	(let ((it (iter-next first)))
	  (if (iter-null? it)
		  iter-null
		  (apply-to-iterators f ... it (rest ...))))))
  (iter-let
   ((iter-let-name ((name val) ...) body ...)
	(apply-to-iterators (lambda (name ...) body ...) (val ...))))
  (setter-impl
   ((_ () ((name name-name) ...))
	(lambda (return name-name ...) (set! name name-name) ... return))
   ((_ (first name ...) (pairs ...))
	(setter-impl (name ...) (pairs ... (first name-name)))))
  ;; For use in iterate.
  (setter ((_ names ...) (setter-impl (names ...) ())))
  (iterate-impl-impl
   ((_ let null recur ((name value) ...) body ...)
	(let ((name value) ...)
	  (iterator loop ()
		(call-with-values
			(lambda ()
			  (letrec ((s (setter name ...))
					   (null (lambda () (values iter-null name ...)))
					   (recur (lambda (name ...) (s #f name ...) (do-body)))
					   (do-body (lambda () body ...)))
				(do-body)))
		  (setter name ...))))))
  (iterate-impl
   ((_ (let-name let-type) ...)
	(begin
	  (define-syntax*
		:::
		(let-name
		 ((let-name ((name value) :::) body :::)
		  (iterate-impl-impl let-type ignore-1 ignore-2 ((name value) :::) body :::))
		 ((let-name null ((name value) :::) body :::)
		  (iterate-impl-impl let-type null ignore ((name value) :::) body :::))
		 ((let-name null recur ((name value) :::) body :::)
		  (iterate-impl-impl let-type null recur ((name value) :::) body :::))))
	  ...)))
  (iter-zip-impl
   ((_ () (names ...))
	(iterator () (apply-to-iterators values (names ...))))
   ((_ (it its ...) (names ...))
	(let ((name it)) (iter-zip-impl (its ...) (names ... name)))))
  (iter-zip
   ((_ its ...)
	(iter-zip-impl (its ...) ()))))
(iterate-impl (iterate let) (iterate* let*))

(define (iterator-replace! old new)
  (set-iterator-next-function! old (iterator-next-function new)))
(define (iterator-defer! old new)
  (iterator-replace! old new) (iter-next old))

(define iter-null (iterator (this) iter-null))
(define (iter-null? obj) (eqv? obj iter-null))
;; Setting iter to iter-null on the EOS would make this more predictable but it
;; would also incur an overhead and preclude multi-valued iterators.
(define (iter-next iter) ((iterator-next-function iter) iter))
(define (iter-cons item iter)
  (iterator (this)
	(iterator-replace! this iter)
	item))
(define (iter-peek iter)
  (define item (iter-peek iter))
  (iterator-replace! iter (iter-cons item iter))
  item)

(define (list->iter list)
  (iterate null ((l list))
	(if (null? l)
		(null)
		(values (car l) (cdr l)))))
(define (port->char-iter port)
  (iterator ()
	(define out (read-char port))
	(if (eof-object? out) iter-null out)))
(define (string->iter string)
  (port->char-iter (open-input-string string)))

(define (repeat-string->iter string)
  (define port (open-input-string string))
  (iterator loop ()
	(define next (read-char port))
	(if (eof-object? next)
		(begin (set! port (open-input-string string))
			   (loop))
		next)))
(define (iter->string iter)
  (define out (open-output-string))
  (iter-for-each (lambda (c) (write-char c out)) iter)
  (get-output-string out))
(define (vector->iter vect)
  (iterate null ((i 0))
	(if (< i (vector-length vect))
		(values (vector-ref vect i) (+ i 1))
		(null))))
(define (repeat-vector->iter vect)
  (iterate null loop ((i 0))
	(if (< i (vector-length vect))
		(values (vector-ref vect i) (+ i 1))
		(loop 0))))

(define (repeat-list->iter list)
  (when (null? list)
	(error "Cannot repeat empty list."))
  (iterate null loop ((l list))
	(if (null? l)
		(loop list)
		(values (car l) (cdr l)))))
(define (pairs->iter list)
  (iterate null ((l list))
	(if (pair? l)
		(values (car l) (cdr l))
		(null))))
;; Call branch->iter on tree, then on the items from that iterator recursively.
;; Yield any results which aren't iterators.
;; Skip the initial call if tree-already-iter?.
;; For example:
;; (iter->list (tree->iter '(a (b (c)) d) (lambda (i) (if (pair? i) (list->iter i) i))))
;; => (a b c d)
(define* tree->iter
  (case-lambda
	((tree item->iter) (tree->iter tree item->iter #f))
	((tree item->iter tree-already-iter?)
	 (iterate null next
			  ((stack (list (if tree-already-iter? tree (item->iter tree)))))
	   (if (null? stack)
		   (null)
		   (let ((i (iter-next (car stack))))
			 (if (iter-null? i)
				 (next (cdr stack))
				 (let ((branch (item->iter i)))
				   (if (iterator? branch)
					   (next (cons branch stack))
					   (values branch stack))))))))))

(define (iter-recursive-flatten iter)
  (tree->iter iter identity))

(define (iter->list iter)
  (define first (iter-next iter))
  (if (iter-null? first)
	  '()
	  (let* ((output (list first))
			 (output-tail output))
		(iter-for-each
		 (lambda (i)
		   (set-cdr! output-tail (list i))
		   (set! output-tail (cdr output-tail)))
		 iter)
		output)))
(define (iter->vector iter)
  ;; TODO: Implement this by filling/reallocating a large vector and shrinking
  ;; it down afterwards.
  (list->vector (iter->list iter)))

(define (step-iterators its)
  (define gave-null? #f)
  (define result
	(iter-map
	 (lambda (i)
	   (define val (i))
	   (set! gave-null? (iter-null? val))
	   val)
	 (list->iter its)))
  (if gave-null? #f result))

(define-syntax*
  (lambda-cases
   ((_ macro () cases \...)
	(case-lambda cases \...))
   ((_ macro (macro-case \... (arg \...)) normal-case \...)
	(lambda-cases macro (macro-case \...)
				  ((arg \...) (macro arg \...)) normal-case \...)))
  (map-case
   ((_ f arg \...) (iterator () (apply-to-iterators f (arg \...)))))
  (for-each-case
   ((_ f arg \...)
	(iter-run (iter-map f arg \...))))
  (fold-case
   ((_ f seed arg \...)
	(iter-for-each (lambda (arg \...) (set! seed (f seed arg \...))) arg \...))))

(define (assert-1-arg args)
  (when (null? args)
	(error "Expected at least one rest argument.")))
(define iter-map
  (lambda-cases
   map-case
   ((f a) (f a b) (f a b c) (f a b c d))
   ((f . rest)
	(assert-1-arg rest)
	(iterator ()
	  (let ((its (step-iterators rest)))
		(if its (apply f its) iter-null))))))
(define iter-for-each
  (lambda-cases
   for-each-case
   ((f a) (f a b) (f a b c) (f a b c d))
   ((f . rest) (assert-1-arg rest) (iter-run (apply iter-map f rest)))))
(define iter-fold
  (lambda-cases
   fold-case
   ((f a) (f a b) (f a b c) (f a b c d))
   ((f seed . rest)
	(assert-1-arg rest)
	(apply iter-for-each (lambda args (set! seed (apply f seed args))) rest)
	seed)))

(define (iter-run it)
  (while (not (iter-null? (iter-next it)))))

(define (iter-filter f iter)
  (iterator next ()
	(iter-let ((item iter))
	  (if (f item) item (next)))))
(define (iter-delete-if f iter)
  (iterator next ()
	(iter-let ((item iter))
	  (if (f item) (next) item))))
(define iter-delete
  (case-lambda
	((item iter) (iter-delete equal? item iter))
	((compare? item iter)
	 (iter-delete-if (lambda (i) (compare? item i)) iter))))
(define (iter-delq item iter) (iter-delete eq? item iter))
(define (iter-delv item iter) (iter-delete eqv? item iter))

(define (iter-append . its)
  (iterator (this)
	(if (null? its)
		iter-null
		(let ((next (iter-next (car its))))
		  (if (iter-null? next)
			  (begin
				(set! its (cdr its))
				(when (and (not (null? its)) (null? (cdr its)))
				  (iterator-replace! this (car its)))
				(iter-next this))
			  next)))))

(define (iter-forever item) (iterator () item))
(define (iter-repeat times item) (iter-take times (iter-forever item)))
(define (iter-take n iter)
  (iterate null ((n n))
	(if (= n 0)
		(null)
		(values (iter-next iter) (- n 1)))))
(define (iter-drop n iter)
  (iterator (this)
	(while (< n 0) (iter-next (iter)) (set! n (- n 1)))
	(iterator-defer! this iter)))
(define (iter-take-while f iter)
  (iterator (this)
	(iter-let ((item iter))
	  (if (f item)
		  item
		  (iterator-defer! this iter-null)))))
(define (iter-drop-while f iter)
  (iterator next (this)
	(iter-let ((item iter))
	  (if (f item)
		  (next)
		  (iterator-defer! this iter)))))

(define (iter-flatten iter)
  (define sub-iter iter-null)
  (iterator next (this)
	(let ((item (iter-next sub-iter)))
	  (if (iter-null? item)
		  (let ((outer-next (iter-next iter)))
			(if (iter-null? outer-next)
				(iterator-defer! this iter-null)
				(begin
				  (set! sub-iter outer-next)
				  (next))))
		  item))))
