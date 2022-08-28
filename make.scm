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

(begin
  (define template-file "templates/generated/post.html")
  (define code-file "templates/generated/code-output.html")
  (define in-dir "posts/")
  (define code-dir "code/")
  (define out-dir "data/www/post/")
  (ftw in-dir
	   (λ (in-file _ flag)
		 (when (eq? flag 'regular)
		   (let* ((file-name (substring in-file (string-length in-dir)))
				  (out-file (string-append out-dir file-name))
				  (post-name (match:substring (string-match "posts/(.*)\\.html$" in-file) 1))
				  (code-dir (string-append code-dir post-name "/"))
				  (code-files (map (λ (f) (string-append code-dir f))
								   (scandir code-dir (λ (f) (not (string-match "^\\.+$" f)))))))
			 (define (instance output-file template-file input-file . code-files)
			   (define content (getfile input-file))
			   (define (read-code match)
				 (string-append
				  "<pre><code>"
				  (begin
					(system* "highlight" "--fragment" "--inline-css" "--line-numbers"
							 "--line-number-length" "3"
							 (string-append code-dir (match:substring match 1))
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
			 (rule (list out-file) (cons* template-file in-file code-files) instance)))
		 #t)))

(rule '("data/www/error404.html") '("templates/template.html" "pages/error404.html")
	  (λ (out-file template in-file)
		(putfile out-file (handle-defs template `(("CONTENT" . ,(getfile in-file)))))))

(define *index-post-count** 10)

(rule '("data/www/index.html") '("templates/template.html" "pages/index.html" "pages/post-list.html")
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

(rule '("data/www/posts.html") '("templates/template.html" "pages/posts.html" "pages/post-list.html")
	  (λ (out-file template in-file post-list)
		(define content (handle-defs in-file `(("POSTS" . ,(getfile post-list)))))
		(putfile out-file (handle-defs template `("BACK-ARROW" ("CONTENT" . ,content))))))

(rule '("templates/generated/post.html") '("templates/template.html")
	  (λ (post template)
		(putfile post (handle-defs template '("BACK-ARROW" "COMMENTS")))))

(filter-if-defs "templates/template.html" '("BACK-ARROW" "COMMENTS"))

(rule '("templates/generated/page.html") '("templates/template.html")
	  (λ (page template)
		(putfile page (handle-defs template '("BACK-ARROW")))))

(make)
