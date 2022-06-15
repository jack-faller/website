#!/usr/bin/env guile
!#

(use-modules (ice-9 ftw) (ice-9 regex) (ice-9 textual-ports))
(define (getfile file) (call-with-input-file file get-string-all))
(define (putfile file string) (call-with-output-file file (lambda (port) (put-string port string))))
(define (replace string from to) (regexp-substitute/global #f from string 'pre to 'post))

(define-macro (when-outdated results deps . body)
  "Run BODY when files any from RESULTS are older than those from DEPS.
RESULTS is a list of file-names (files need not exist), stat objects (return of
stat), or numbers from stat:mtime.
DEPS is of the same type as results, but files must exits.
BODY can be any number of forms, evaluated and returned given RESULTS are out of
date. These should then produce new versions of the files referenced by
RESULTS."
  (define (mtime-macro file) `(cond ((string? ,file) (stat:mtime (stat ,file)))
									((number? ,file) ,file)
									(else (stat:mtime ,file))))
  (let ((results-gensyms (map (lambda (_) (gensym)) results))
		(deps-gensyms (map (lambda (_) (gensym)) deps)))
	`(let (,@(map list results-gensyms results)
		   ,@(map list deps-gensyms deps))
	   (when (or ,@(map (lambda (sym)
						  `(or (not (string? ,sym))
							   (not (file-exists? ,sym))))
						results)
				 (> (max ,@(map mtime-macro deps-gensyms))
					(min ,@(map mtime-macro results-gensyms))))
		 (begin ,@body)))))

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
			 (when-outdated (out-file) (template-mtime in-statinfo)
							(instance in-file out-file))))
		 #t)))

(templatise "templates/post.html" "posts/" "data/www/post/")
(templatise "templates/page.html" "pages/" "data/www/")
