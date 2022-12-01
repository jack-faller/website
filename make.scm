#!/usr/bin/env guile
!#

(use-modules (srfi srfi-1)
			 (srfi srfi-9)
			 (ice-9 rdelim)
			 (ice-9 hash-table)
			 (ice-9 match)
			 (ice-9 ftw)
			 (ice-9 regex)
			 (ice-9 textual-ports))
(define rx-global regexp-substitute/global)

(define (getfile file) (call-with-input-file file get-string-all))
(define (putfile file string) (call-with-output-file file (λ (port) (put-string port string))))

(define *rules* (make-hash-table))
(define-record-type <rule>
  (make-rule visited? products dependencies build-function)
  rule?
  (visited? visited? set-visited!)
  (products products set-products!)
  (dependencies dependencies set-dependencies!)
  (build-function build-function set-build-function!))
(define (rule product/s depends-on build-function)
  (define products (if (pair? product/s) product/s (list product/s)))
  (for-each (λ (x)
			  (when (hash-ref *rules* x)
				(error (string-append "duplicate rule for building " x)))
			  (hash-set! *rules* x (make-rule #f products depends-on build-function)))
			products))
(define (make)
  (define (process root rule)
	(cond
	 ((eq? (visited? rule) 'visiting)
	  (error (string-append "recursive dependency for " root
							" ending at " (products rule))))
	 ((not (visited? rule))
	  (set-visited! rule 'visiting)
	  (for-each
	   (λ (i) (let ((rule (hash-ref *rules* i))) (when rule (process root rule))))
	   (dependencies rule))
	  (let* ((mtimes (λ (files) (map (compose stat:mtime stat) files)))
			 (deps (if (current-filename)
					   (cons (current-filename) (dependencies rule))
					   (dependencies rule)))
			 (newest-dependancy (apply max (mtimes deps)))
			 (existent-products (filter file-exists? (products rule)))
			 (oldest-product (if (null? existent-products)
								 0
								 (apply min (mtimes existent-products)))))
		(when (< oldest-product newest-dependancy)
		  (for-each display (list "building " (products rule)
								  " from " (dependencies rule)))
		  (newline)
		  (apply (build-function rule) (append (products rule) (dependencies rule)))))
	  (set-visited! rule #t))))
  (hash-for-each process *rules*))

(define (filter-if-defs file defs)
  (let* ((defined '())
		 (return
		  ;; relies on filter going in order
		  (filter
		   (λ (x)
			 (let ((else/if-match (string-match "(ELSE-)?IF-DEF\\s*([^[:space:]]*)" x)))
			   (cond
				((and else/if-match (not (match:substring else/if-match 1)))
				 (set! defined
					   (cons (if (member (match:substring else/if-match 2) defs)
								 (cons #t #t)
								 (cons #f #f))
							 defined))
				 #f)
				(else/if-match
				 (set-car! defined
						   (cond
							((eq? (cdar defined) 'else) (error (string-append "else-if after else in " file)))
							((eq? (cdar defined) #t) '(#f #t))
							((member (match:substring else/if-match 2) defs) '(#t #t))
							(else '(#f #f))))
				 #f)
				((string-match "ELSE" x)
				 (set-car! defined
						   (match (cdar defined)
							 ('else (error (string-append "repeated else block in " file)))
							 (#t '(#f else))
							 (#f '(#t else))))
				 #f)
				((string-match "END-IF" x)
				 (if (null? defined)
					 (error (string-append "unmatched end-if in " file))
					 (set! defined (cdr defined)))
				 #f)
				(else (or (null? defined) (caar defined))))))
		   (string-split (getfile file) #\Newline))))
	(if (not (null? defined))
		(error (string-append "unmatched end-if in " file))
		(string-join return "\n" 'suffix))))

(define (subst-defs string defs)
  (if (null? defs)
	  string
	  (let ((ht (alist->hash-table defs))
			;; | should return longest match
			(regexp (string-join (map (compose regexp-quote car) defs) "|")))
		(rx-global #f regexp string
				   'pre (λ (x) (hash-ref ht (match:substring x))) 'post))))

(define (handle-defs file defs)
  (subst-defs (filter-if-defs file (map (λ (x) (if (pair? x) (car x) x)) defs))
			  (filter pair? defs)))

(define (dir-func dir) (lambda (name) (string-append dir name)))
(define generated (dir-func "generated/"))
(define page-src (dir-func "pages/"))
(define post-src (dir-func "posts/"))
(define page-out (dir-func "data/www/"))
(define post-out (dir-func "data/www/post/"))

(define post-infos-file "posts.txt")
(define post-infos
  (let* ((lines (string-split (getfile post-infos-file) #\Newline))
		 (lines (filter (λ (line) (not (string= line ""))) lines))
		 (lines (reverse lines)))
	(map (λ (line) (string-split line #\|)) lines)))
(define post-list (generated "post-list.html"))
(define page-template (generated "page.html"))
(define post-template (generated "post.html"))
(begin
  (define code-file (generated "code-output.html"))
  (define out-dir "data/www/post/")
  (define (not-dot file) (not (string-match "^\\.+$" file)))
  (define date-file (dir-func (generated "dates/")))
  (define (add-file file-name)
	(define match (string-match "(.*)\\.html$" file-name))
	(when match
	  (let* ((out-file (post-out file-name))
			 (post-name (match:substring match 1))
			 (date-file (date-file post-name))
			 (code (λ (file) (string-append (post-src post-name) "/" file)))
			 (code-files (map code (or (scandir (code "") not-dot) '()))))
		(define (instance output-file template-file input-file date-file . code-files)
		  (define content (getfile input-file))
		  (define (read-code match)
			(string-append
			 "<pre><code class=\"block\">"
			 (begin
			   (system* "highlight" "--fragment" "--inline-css" "--line-numbers"
						"--line-number-length" "3"
						(code (match:substring match 1))
						"-o" code-file "-O" "html")
			   (newline)
			   (getfile code-file))
			 "</code></pre>"))
		  (let* ((it (handle-defs template-file
								  `(("POSTNAME" . ,post-name)
									("CONTENT" . ,content)
									("DATE" . ,(getfile date-file)))))
				 (it (rx-global #f "CODE-BLOCK[^\n]*\"([^\n]*)\"" it
								'pre read-code 'post)))
			(putfile output-file it)))
		(unless (file-exists? date-file) (putfile date-file "DRAFT"))
		(rule out-file
			  (cons* post-template (post-src file-name) date-file code-files)
			  instance))))
  (for-each (λ (info)
			  (let ((date-file (date-file (car info))) (date (cadr info)))
				(when (not (and (file-exists? date-file)
								(string= date (getfile date-file))))
				  (putfile date-file date))))
			post-infos)
  (for-each add-file (scandir (post-src "") not-dot)))

(define base-template "template.html")

(define (page name dependencies fn)
  (rule (page-out name) (cons (page-src name) dependencies) fn))

(page "error404.html" (list page-template)
	  (λ (out-file in-file template)
		(putfile out-file (handle-defs template `(("CONTENT" . ,(getfile in-file)))))))

(define index-post-count 10)

(page "index.html" (list base-template post-list)
	  (λ (out-file in-file template post-list)
		(define file (open-input-file post-list))
		(define line "")
		(define recent-posts
		  (let loop ((output "") (times index-post-count))
			(if (or (= times 0)
					(begin (set! line (read-line file)) (eof-object? line)))
				output
				(loop (string-append output line) (- times 1)))))
		(define content (handle-defs in-file `(("RECENT-POSTS" . ,recent-posts))))
		(putfile out-file (handle-defs template `(("CONTENT" . ,content))))))

(page "posts.html" (list page-template post-list)
	  (λ (out-file in-file template post-list)
		(define content (handle-defs in-file `(("POSTS" . ,(getfile post-list)))))
		(putfile out-file (handle-defs template `(("CONTENT" . ,content))))))

(rule post-template (list base-template)
	  (λ (post template)
		(putfile post (handle-defs template '("BACK-ARROW" "COMMENTS" "DATE")))))

(rule page-template (list base-template)
	  (λ (page template)
		(putfile page (handle-defs template '("BACK-ARROW")))))

(rule post-list (list post-infos-file)
	  (λ (out-file in-file)
		(define lines
		  (map (λ (info) (string-append
						  "<li><a href=\"/post/" (car info) "\">"
						  (caddr info) " &ndash; " (cadr info)
						  "</a></li>"))
			   post-infos))
		(putfile out-file (string-join lines "\n"))))
(make)
