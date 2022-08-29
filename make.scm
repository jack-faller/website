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

(define (getfile file) (call-with-input-file file get-string-all))
(define (putfile file string) (call-with-output-file file (λ (port) (put-string port string))))

(define *rules* (make-hash-table))
(define *products* '())
(define-record-type <rule>
  (make-rule visited? products dependencies build-function)
  rule?
  (visited? visited? set-visited!)
  (products products set-products!)
  (dependencies dependencies set-dependencies!)
  (build-function build-function set-build-function!))
(define (rule products depends-on build-function)
  (for-each (λ (x) (if (hash-ref *rules* x)
					   (error (string-append "duplicate rule for building " x))
					   (hash-set! *rules* x (make-rule #f products depends-on build-function))))
			products)
  (set! *products* (append products *products*)))
(define (make)
  (define (process product)
	(let ((rule (hash-ref *rules* product)))
	  (cond
	   ((eq? (visited? rule) 'visiting) (error (string-append "recursive dependency on: " product)))
	   ((not (visited? rule))
		(set-visited! rule 'visiting)
		(for-each
		 (λ (i) (when (hash-ref *rules* i) (process i)))
		 (dependencies rule))
		(let* ((mtimes (λ (files) (map (compose stat:mtime stat) files)))
			   (newest-dependancy (apply max (mtimes (dependencies rule))))
			   (existent-products (filter file-exists? (products rule)))
			   (oldest-product (if (null? existent-products) 0 (apply min (mtimes existent-products)))))
		  (when (or (not (current-filename))
					(< oldest-product (max (stat:mtime (stat (current-filename))) newest-dependancy)))
			(for-each display (list "building " (products rule) " from " (dependencies rule)))
			(newline)
			(apply (build-function rule) (append (products rule) (dependencies rule)))))
		(set-visited! rule #t)))))
  (for-each process *products*))

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
		(regexp-substitute/global #f regexp string
								  'pre (λ (x) (hash-ref ht (match:substring x))) 'post))))

(define (handle-defs file defs)
  (subst-defs (filter-if-defs file (map (λ (x) (if (pair? x) (car x) x)) defs))
			  (filter pair? defs)))

(define (dir-func dir) (lambda (name) (string-append dir name)))
(define template (dir-func "templates/"))
(define generated (dir-func "generated/"))
(define page-src (dir-func "pages/"))
(define post-src (dir-func "posts/"))
(define page-out (dir-func "data/www/"))
(define post-out (dir-func "data/www/post/"))

(define page-template (generated "page.html"))
(define post-template (generated "post.html"))
(begin
  (define code-file (generated "code-output.html"))
  (define out-dir "data/www/post/")
  (define dates (make-hash-table))
  (define (not-dot file) (not (string-match "^\\.+$" file)))
  (define (add-file file-name)
	(define match (string-match "(.*)\\.html$" file-name))
	(when match
	  (let* ((out-file (post-out file-name))
			 (post-name (match:substring match 1))
			 (code (λ (file) (string-append (post-src post-name) "/" file)))
			 (code-files (map code (scandir (code "") not-dot))))
		(define (instance output-file template-file input-file . code-files)
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
									("CONTENT" . ,content))))
				 (it (regexp-substitute/global #f "CODE-BLOCK[^\n]*\"([^\n]*)\"" it
											   'pre read-code 'post)))
			(putfile output-file it)))
		(rule (list out-file) (cons* post-template (post-src file-name) code-files)
			  instance))))
  (for-each
   (λ (line)
	 (define split (string-split line #\|))
	 (when (not (string= line ""))
	   (hash-set! dates (car split) (cadr split))))
   (string-split (getfile "post-dates.txt") #\Newline))
  (for-each add-file (scandir (post-src "") not-dot)))

(define base-template (template "template.html"))

(rule (list (page-out "error404.html")) (list page-template (page-src "error404.html"))
	  (λ (out-file template in-file)
		(putfile out-file (handle-defs template `(("CONTENT" . ,(getfile in-file)))))))

(define *index-post-count** 10)

(rule (list (page-out "index.html")) (cons* base-template (map page-src '("index.html" "post-list.html")))
	  (λ (out-file template in-file post-list)
		(define file (open-input-file post-list))
		(define line "")
		(define recent-posts
		  (let loop ((output "") (times *index-post-count**))
			(if (or (= times 0) (eof-object?
								 (begin (set! line (read-line file))
										line)))
				output
				(loop (string-append output line) (- times 1)))))
		(define content (handle-defs in-file `(("RECENT-POSTS" . ,recent-posts))))
		(putfile out-file (handle-defs template `(("CONTENT" . ,content))))))

(rule (list (page-out "posts.html")) (cons* page-template (map page-src '("posts.html" "post-list.html")))
	  (λ (out-file template in-file post-list)
		(define content (handle-defs in-file `(("POSTS" . ,(getfile post-list)))))
		(putfile out-file (handle-defs template `(("CONTENT" . ,content))))))

(rule (list post-template) (list base-template)
	  (λ (post template)
		(putfile post (handle-defs template '("BACK-ARROW" "COMMENTS")))))

(rule (list page-template) (list base-template)
	  (λ (page template)
		(putfile page (handle-defs template '("BACK-ARROW")))))

(make)
