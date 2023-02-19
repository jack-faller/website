(use-modules (srfi srfi-1)
			 (srfi srfi-9)
			 (srfi srfi-13)
			 (ice-9 popen)
			 (ice-9 rdelim)
			 (ice-9 hash-table))

(define (drop-space port)
  (while (let ((c (peek-char port)))
		   (and (not (eof-object? c))
				(char-whitespace? c)))
	(read-char port)))

(define (read-until f port)
  (string-unfold
   (lambda (c) (or (eof-object? c) (f c)))
   identity
   (lambda (ign) (read-char port) (peek-char port))
   (peek-char port)))

(define stop-unfolding (gensym "stop-unfolding"))
(define (unfold-until f)
  (unfold (lambda (c) (eq? c stop-unfolding)) identity (lambda (_) (f)) (f)))

(define (delimited-read end port table)
  (unfold-until
   (lambda ()
	 (drop-space port)
	 (let ((peeked (peek-char port)))
	   (cond
		((eof-object? peeked) (error "Missing closing delimiter."))
		((eq? peeked end) (read-char port) stop-unfolding)
		(else (table-read port table)))))))

(define (table-read port table)
  (drop-space port)
  (let* ((peeked (peek-char port))
		 (fun (hashq-ref table peeked)))
	(cond
	 ((eof-object? peeked) peeked)
	 (fun (read-char port) (fun port table))
	 (else ((hashq-ref table 'atom) port table)))))

(define (list-syntax-pair start end)
  (cons start
		(lambda (port table)
		  (fold-right
		   (lambda (i rest)
			 (cond
			  ((not (eq? i '#{.}#)) (cons i rest))
			  ((not (= (length rest) 1)) (error "Malformed dotted list."))
			  (else (car rest))))
		   '()
		   (delimited-read end port table)))))
(define (quote-syntax-pair char sym)
  (cons char (lambda (port table) (list sym (table-read port table)))))
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
			  (table-read port table))))

(define curly-read-table
  (alist->hashq-table
   `((#\{ . ,(lambda (port table)
			   (delimited-read #\} port curly-read-table)))
	 ,comment-reader-pair
	 (#\} . ,(lambda _ (error "Unexpected closing delimiter" #\})))
	 (#\# . ,(lambda (port table) (unquote-reader port default-read-table)))
	 (atom . ,read-atom))))
(define default-read-table
  (alist->hashq-table
   `((atom . ,(lambda (port table)
				(if (eq? (peek-char port) #\")
					(read port)
					(read (open-input-string (read-atom port table))))))
	 ,(list-syntax-pair #\( #\))
	 ,(list-syntax-pair #\[ #\])
	 ,(quote-syntax-pair #\' 'quote)
	 ,(quote-syntax-pair #\` 'quasiquote)
	 (#\, . ,unquote-reader)
	 (#\{ . ,(lambda (port table)
			   (cons 'quasiquote
					 (list (delimited-read #\} port curly-read-table)))))
	 ,comment-reader-pair
	 ,@(map (lambda (a)
			  (cons a (lambda _ (error "Unexpected closing delimiter" a))))
			'(#\) #\) #\})))))

(define (doclisp-reader port)
  (table-read port default-read-table))

(define (sexp->html sexp)
  (define (tag first)
	(cond
	 ((string? first) (cons first first))
	 ((list? first)
	  (cons (string-join
			 (cons
			  (car first)
			  (map (lambda (x)
					 (if (list? x)
						 (string-append (car x) "=\"" (cadr x) "\"")
						 x))
				   (cdr first))))
			(car first)))))
  (if (string? sexp)
	  sexp
	  (let ((head (if (or (equal? (car sexp) "just") (equal? (car sexp) "join"))
					  #f
					  (tag (car sexp))))
			(body (string-join
				   (filter-map (lambda (e) (if e (sexp->html e) e))
							   (cdr sexp))
				   (if (equal? (car sexp) "join") "" " "))))
		(if head
			(string-append "<" (car head) ">" body "</" (cdr head) ">")
			body))))

(define (cmd prog . args)
  (let* ((pipe (apply open-pipe* OPEN_BOTH prog args))
		 (output (read-string pipe)))
	(close pipe)
	output))

(define current-dir (string-append (dirname (current-filename)) "/"))
(define (thisdir f) (string-append current-dir f))
(define (dl-load file) (load file doclisp-reader))
(dl-load (thisdir "make.scm"))
