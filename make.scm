(use-modules (ice-9 ftw)
			 (ice-9 regex)
			 (srfi srfi-19))

(define (link-heading depth id text)
  (define (link body)
	{{a {class linkheading} {href #(string-append "#" id)}}
	 #body})
  {{div {id #id}}
   {#(string-append "h" (number->string depth))
	#(if (procedure? text)
		 (text link)
		 (link text))}})

(define (template blogname wants-back-arrow? date . body)
  {just
   <!DOCTYPE html>
   {head {title Jack Faller}
		 {{link {href /style.css} {rel stylesheet} {type text/css}}}
		 #@(if blogname
			   {{script const blogname =
						#(string-append "\"" blogname "\"")}
				{{script {src comment-script.js} defer}}}
			   '())}
   {body
	{main
	 #(and wants-back-arrow?
		   {{a {href /} {title home} {class backarrow}} #"&larr;"})
	 #(and date {{div {class date}} #date})
	 #@body}}})
(define (code-block file-name)
  {pre {{code {class block}}
		#(cmd "highlight" (thisdir (string-append "posts/" file-name))
			  "-O" "html" "--inline-css" "--fragment"
			  "--line-numbers" "--line-number-length" "3")}})
(define (calendar-date y m d) (make-date 0 0 0 0 d m y 0))
(define (date-format date)
  (string-append
   (let ((day (date-day date)))
	 (string-append (number->string day)
					(case day
					  ((1 21 31) "st")
					  ((2 22 32) "nd")
					  ((3 23) "rd")
					  (else "th"))))
   (date->string date " ~B ~Y")))

(define-record-type <post>
  (make-post name title time date written-date body)
  post?
  (name         post-name         post-set-name!)
  (time         post-time         post-set-time!)
  (title        post-title        post-set-title!)
  (date         post-date         post-set-date!)
  (written-date post-written-date post-set-written-date!)
  (body         post-body         post-set-body!))

(define (post title date . body)
  (let* ((sort (and date (date->time-tai date)))
		 (written-date (if date (date-format date) "DRAFT")))
	(make-post #f title sort (or date 'draft) written-date
			   (cons {h1 #title} body))))

(define (page . body) (apply template #f #t #f body))
(define (home-page . body) (apply template #f #f #f body))

(define (output-file name)
  (open-file (thisdir (string-append "data/www/" name)) "w"))
(define (write-sexp-to-html-file name sexp)
  (let ((out (output-file (string-append name ".html"))))
	(display (sexp->html sexp) out)
	(close-port out)))

(define (scheme-file-functor f)
  (lambda (file)
	(if (string-suffix? ".scm" file)
		(f (basename file ".scm") file)
		#f)))
(define (dirfiles dir)
  (map
   (lambda (x) (string-append (thisdir dir) "/" x))
   (scandir (thisdir dir))))

(define posts
  (filter-map
   (scheme-file-functor
	(lambda (name file)
	  (let ((p (dl-load file)))
		(post-set-name! p name)
		p)))
   (dirfiles "posts")))

(for-each
 (lambda (post)
   (write-sexp-to-html-file
	(string-append "post/" (post-name post))
	(apply template (post-name post) #t (post-written-date post)
		   (post-body post))))
 posts)

(define post-list
  (map
   (lambda (post)
	 {li {{a {href #(string-append "/post/" (post-name post))}}
		  #(post-title post) #"&ndash;" #(post-written-date post)}})
   (sort
	(filter post-time posts)
	(lambda (a b) (time>? (post-time a) (post-time b))))))
(define recent-posts
  {ul #@(list-head post-list (min 10 (length post-list)))})

(write-sexp-to-html-file "posts" (page {h1 All Posts} {ul #@post-list}))
(for-each
 (scheme-file-functor
  (lambda (name file)
	(write-sexp-to-html-file name (dl-load file))))
 (dirfiles "pages"))
