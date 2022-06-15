#!/bin/env guile
!#

(use-modules (ice-9 ftw) (ice-9 regex) (ice-9 textual-ports))
(define (getfile file) (call-with-input-file file get-string-all))
(define (putfile file string) (call-with-output-file file (lambda (port) (put-string port string))))
(define (replace string from to) (regexp-substitute/global #f from string 'pre to 'post))

(define (templatise template-file in-dir out-dir)
  (define (instance input-file output-file)
	(define post-name (regexp-substitute #f (string-match "\\.html$" input-file) 'pre))
	(define content (getfile input-file))
	(let* ((it (getfile template-file))
		   (it (replace it "[ \t]*POSTNAME" post-name))
		   (it (replace it "[ \t]*CONTENT" content)))
	  (putfile output-file it)
	  (display (string-append "instanced " input-file " to " output-file)) (newline)))
  (define template-mtime (stat:mtime (stat template-file)))
  (ftw in-dir
	   (lambda (in-file in-statinfo flag)
		 (when (eq? flag 'regular)
		   (let* ((file-name (substring in-file (string-length in-dir)))
				  (out-file (string-append out-dir file-name))
				  (out-mtime (stat:mtime (stat out-file))))
			 (when (or (not (file-exists? out-file))
					   (> (max template-mtime (stat:mtime in-statinfo))
						  out-mtime))
			   (instance in-file out-file))))
		 #t)))

(templatise "templates/post.html" "posts/" "data/www/post/")
(templatise "templates/page.html" "pages/" "data/www/")
