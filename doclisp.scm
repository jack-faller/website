(define-module (doclisp)
  #:use-module (iterators)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-13)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 hash-table)
  #:use-module (ice-9 textual-ports)
  #:export (set-reader! doclisp-reader sexp->html write-sexp->html))

(define-syntax set-reader!
  (syntax-rules ()
	((_ reader)
	 (eval-when (compile eval)
	   (fluid-set! current-reader reader)))))

(define (port-position port)
  (vector (port-filename port) (port-line port) (port-column port)))

(define (drop-space port)
  (let ((found-space #f))
	(while (let ((c (peek-char port)))
			 (and (not (eof-object? c))
				  (char-whitespace? c)))
	  (set! found-space #t)
	  (read-char port))
	found-space))

(define (read-until f port)
  (define (peek-escape port)
	(define out (peek-char port))
	(if (eq? out #\\)
		(begin
		  (read-char port)
		  (list (peek-char port)))
		out))
  (string-unfold
   (lambda (c)
	 (if (pair? c)
		 (eof-object? (car c))
		 (or (eof-object? c) (f c))))
   (lambda (c)
	 (if (pair? c) (car c) c))
   (lambda (ign)
	 (read-char port)
	 (peek-escape port))
   (peek-escape port)))

(define (delimited-read end port table join?)
  (iter->list
   (iterate null ((first? #t))
	 (let* ((dropped-space? (drop-space port))
			(peeked (peek-char port)))
	   (cond
		((eof-object? peeked) (error "Missing closing delimiter."))
		((eq? peeked end) (read-char port) (null))
		((or dropped-space? (not join?) first?)
		 (values (table-read port table) #f))
		(else (values 'join #t)))))))

(define (table-read port table)
  (drop-space port)
  (let* ((peeked (peek-char port))
		 (fun (hashq-ref table peeked)))
	(cond
	 ((eof-object? peeked) peeked)
	 (fun (read-char port) (fun port table))
	 (else ((hashq-ref table 'atom) port table)))))

(define comment (substring/copy "comment" 0))
(define (list-syntax-pair start end)
  (cons start
		(lambda (port table)
		  (fold-right
		   (lambda (i rest)
			 (cond
			  ((not (or (eq? (syntax->datum i) '#{.}#)))
			   (if (eq? comment i)
				   rest
				   (cons i rest)))
			  ((not (= (length rest) 1)) (error "Malformed dotted list."))
			  (else (car rest))))
		   '()
		   (delimited-read end port table #f)))))
(define (quote-syntax-pair char sym)
  (cons char
		(lambda (port table)
		  (define pos (port-position port))
		  (list (datum->syntax #f sym #:source pos) (table-read port table)))))
(define (unquote-reader port table)
  (let ((sym (if (eq? (peek-char port) #\@)
				 (begin (read-char port) 'unquote-splicing)
				 'unquote)))
	(list sym (table-read port table))))
(define (read-atom port table)
  (read-until (lambda (char) (or (char-whitespace? char)
								 (hashq-ref table char)))
			  port))
(define comment-reader-pair
  (cons #\; (lambda (port table)
			  (read-line port)
			  comment)))
(define (drop-block-comment port)
  (let ((got (cdr (read-delimited "|#" port 'split))))
	(cond
	 ((eof-object? got)
	  (error "Unterminated comment."))
	 ((and (eq? got #\|) (eq? (peek-char port) #\#))
	  (read-char port)
	  comment)
	 ((and (eq? got #\#) (eq? (peek-char port) #\|))
	  (read-char port)
	  (drop-block-comment port)
	  (drop-block-comment port))
	 (else
	  (drop-block-comment port)))))

(define curly-read-table
  (alist->hashq-table
   `((#\{ . ,(lambda (port table)
			   (delimited-read #\} port curly-read-table #t)))
	 ,comment-reader-pair
	 (#\} . ,(lambda _ (error "Unexpected closing delimiter" #\})))
	 (#\# . ,(lambda (port table)
			   (if (eq? (peek-char port) #\|)
				   (begin (read-char port)
						  (drop-block-comment port)
						  (table-read port table))
				   (unquote-reader port default-read-table))))
	 (atom . ,(lambda (port table)
				(define pos (port-position port))
				(datum->syntax #f (read-atom port table) #:source pos))))))
(define default-read-table
  (alist->hashq-table
   `((atom . ,(lambda (port table)
				(if (eq? (peek-char port) #\")
					(read-syntax port)
					(let ((pos (port-position port)))
					  (datum->syntax
					   #f
					   (call-with-input-string (read-atom port table) read)
					   #:source pos)))))
	 ,(list-syntax-pair #\( #\))
	 ,(list-syntax-pair #\[ #\])
	 ,(quote-syntax-pair #\' 'quote)
	 ,(quote-syntax-pair #\` 'quasiquote)
	 (#\, . ,unquote-reader)
	 (#\{ . ,(lambda (port table)
			   (cons 'quasiquote
					 (list (delimited-read #\} port curly-read-table #t)))))
	 ,comment-reader-pair
	 (#\# . ,(lambda (port table)
			   (case (peek-char port)
				 ((#\|)
				  (read-char port)
				  (drop-block-comment port))
				 ((#\()
				  (apply vector (table-read port table)))
				 (else
				  (unget-char port #\#)
				  (read port)))))
	 ,@(map (lambda (a)
			  (cons a (lambda _ (error "Unexpected closing delimiter" a))))
			'(#\) #\] #\})))))

(define (doclisp-reader port)
  (define val (table-read port default-read-table))
  (if (eq? val comment)
	  (doclisp-reader port)
	  val))

(define self-closing-html-tag?
  (let ((tags (alist->hash-table
			   (map (lambda (x) (cons x #t))
					'("area" "base" "br" "col" "embed" "hr" "img" "input" "link"
					  "meta" "param" "source" "track" "wbr" "!DOCTYPE")))))
	(lambda (t) (hash-ref tags t))))

;; TODO: Handle character escaping.
(define (sexp->html sexp)
  (call-with-output-string
	(lambda (port) (write-sexp->html sexp port))))
(define (write-sexp->html sexp port)
  (define (write-escaped string port)
	(define (map-match m)
	  (case (string-ref (match:string m) 0)
		((#\<) "&lt;")
		((#\>) "&gt;")
		((#\&) "&amp;")))
	(regexp-substitute/global port "[<>&]" string 'pre map-match 'post))
  (define (write-body sexp port)
	(unless (null? sexp)
	  (write-sexp->html (car sexp) port)
	  (let loop ((sexp (cdr sexp)))
		(cond
		 ((null? sexp))
		 ((eq? (car sexp) 'join)
		  (write-sexp->html (cadr sexp) port)
		  (loop (cddr sexp)))
		 (else
		  (display " " port)
		  (write-sexp->html (car sexp) port)
		  (loop (cdr sexp)))))))
  (cond
   ((or (not sexp) (null? sexp)) (display "" port))
   ((string? sexp) (write-escaped sexp port))
   ((pair? sexp)
	(cond
	 ((equal? (car sexp) "just") (write-body (cdr sexp) port))
	 ((equal? (car sexp) "raw") (display (cadr sexp) port))
	 ((equal? (car sexp) "join")
	  (for-each (lambda (x) (write-sexp->html x port)) (cdr sexp)))
	 (else
	  (display "<" port)
	  (let ((name
			 (if (string? (car sexp))
				 (begin (display (car sexp) port) (car sexp))
				 (let ((name (caar sexp)) (attributes (cdar sexp)))
				   (display name port)
				   (for-each
					(lambda (i)
					   (display " " port)
					  (if (string? i)
						  (display i port)
						  (begin
							(display (car i) port)
							(display "=\"" port)
							(write-body (cdr i) port)
							(display "\"" port))))
					attributes)
				   name))))
		(if (and (null? (cdr sexp)) (not (equal? name "script")))
			(display (if (self-closing-html-tag? name) ">" " />") port)
			(begin
			  (display ">" port)
			  (write-body (cdr sexp) port)
			  (display "</" port)
			  (display name port)
			  (display ">" port)))))))
   (else
	(error "Unexpected object in sexp->html." sexp))))
