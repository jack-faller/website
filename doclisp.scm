(define-module (doclisp)
  #:use-module (iterators)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-13)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 hash-table)
  #:use-module (ice-9 textual-ports)
  #:export (set-reader! doclisp-reader sexp->xml write-sexp->xml xml xslt html))

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

;; TODO: Handle character escaping.
(define (sexp->xml sexp language)
  (call-with-output-string
	(lambda (port) (write-sexp->xml sexp language port))))

(define (write-xml-tag-body body language port)
  (unless (null? body)
	(write-sexp->xml (car body) language port)
	(let loop ((body (cdr body)))
	  (cond
	   ((null? body))
	   ((eq? (car body) 'join)
		(write-sexp->xml (cadr body) language port)
		(loop (cddr body)))
	   (else
		(display " " port)
		(write-sexp->xml (car body) language port)
		(loop (cdr body)))))))
(define (xml-tag-writer should-self-close? can-short-close?)
  (lambda (name attributes body language port)
	(display "<" port)
	(display name port)
	(for-each
	 (lambda (i)
	   (display " " port)
	   (if (string? i)
		   (display i port)
		   (begin
			 (display (car i) port)
			 (display "=\"" port)
			 (regexp-substitute/global
			  port "\""
			  (call-with-output-string
				(lambda (port) (write-xml-tag-body (cdr i) language port)))
			  'pre "&quot;" 'post)
			 (display "\"" port))))
	 attributes)
	(if (and (null? body) can-short-close?)
		(display (if should-self-close? ">" " />") port)
		(begin
		  (display ">" port)
		  (write-xml-tag-body body language port)
		  (display "</" port)
		  (display name port)
		  (display ">" port)))))

(define (write-sexp->xml sexp language port)
  (define (write-escaped string port)
	(define (map-match m)
	  (case (string-ref (match:string m) 0)
		((#\<) "&lt;")
		((#\>) "&gt;")
		((#\&) "&amp;")))
	(regexp-substitute/global port "[<>&]" string 'pre map-match 'post))
  (cond
   ((or (not sexp) (null? sexp)) (display "" port))
   ((string? sexp) (write-escaped sexp port))
   ((pair? sexp)
	(receive (name attributes body)
		(if (string? (car sexp))
			(values (car sexp) '() (cdr sexp))
			(values (caar sexp) (cdar sexp) (cdr sexp)))
	  (cond
	   ((equal? name "just") (write-xml-tag-body body language port))
	   ((equal? name "raw") (display (car body) port))
	   ((equal? name "join")
		(for-each (lambda (x) (write-sexp->xml x language port)) body))
	   (else
		((or (language name) (xml-tag-writer #f #t))
		 name attributes body language port)))))
   (else
	(error "Unexpected object in sexp->xml." sexp))))

(define self-closing-html-tags
  '("area" "base" "br" "col" "embed" "hr" "img" "input" "link" "meta" "param"
	"source" "track" "wbr" "!DOCTYPE"))
(define html
  (let ((tags (alist->hash-table
			   (cons
				(cons "script" (xml-tag-writer #f #f))
				(map (lambda (x) (cons x (xml-tag-writer #t #t)))
					 self-closing-html-tags)))))
	(lambda (name) (hash-ref tags name))))

(define (xslt name)
  (if (equal? name "!DOCTYPE")
	  (lambda _ (values))
	  #f))
(define (xml name) #f)
