(use-modules (ice-9 ftw)
			 (ice-9 regex)
			 (ice-9 receive)
			 (srfi srfi-19))

(define (link-href prefix id)
  {href #(string-append "#" prefix "-" id)})

(define (link-heading depth id . text)
  (define (link . body)
	{{a {class headinglink} #(link-href "heading" id)}
	 #@body})
  {{div {id #id} }
   {{#(string-append "h" (number->string depth)) {class heading}}
	#@(map
	  (lambda (i) (if (procedure? i) (i link) (link i)))
	  text)
	{{span {class sectionmark}} #(link "&sect;")}}})

(define (footnotes)
  (define count 0)
  (define notes '())
  (define (format-notes)
	{just
	 {hr}
	 #@(map
		(lambda (vals)
		  (let ((id (car vals)) (count (cadr vals)) (text (cddr vals)))
			{{div {id {join footnote- #id}}}
			 {{a #(link-href "footnote" id)}
			  {join [ #(number->string count) ]}}
			 #@text
			 #"<br>"}))
		(reverse notes))})
  (define (note-ref id)
	(let ((count (cadr (assoc id notes))))
	  {sup {{a #(link-href "footnote" id)} #(number->string count)}}))
  (define (note id . text)
	(set! count (+ count 1))
	(set! notes (cons (cons* id count text) notes))
	(note-ref id))
  (values
   note
   note-ref
   format-notes))

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
(define date-string-format "~Y/~m/~d ~H:~M ~z")
(define (current-date-string) (date->string (current-date) date-string-format))
(define (read-date-string date) (string->date date date-string-format))
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
  (make-post name title time date written-date description body)
  post?
  (name         post-name         post-set-name!)
  (time         post-time         post-set-time!)
  (title        post-title        post-set-title!)
  (date         post-date         post-set-date!)
  (written-date post-written-date post-set-written-date!)
  (description  post-description  post-set-description!)
  (body         post-body         post-set-body!))

(define (post title date description . body)
  (let* ((date (and date (read-date-string date)))
		 (sort (and date (date->time-tai date)))
		 (written-date (if date (date-format date) "DRAFT")))
	(make-post #f title sort (or date 'draft) written-date description body)))

(define (page . body) (apply template #f #t #f body))
(define (home-page . body) (apply template #f #f #f body))

(system* "rm" "-rf" (thisdir "generated"))
(define (output-file name)
  (let ((fname (thisdir (string-append "generated/www/" name))))
	(system* "mkdir" "-p" (dirname fname))
	(open-file fname "w")))
(define (write-sexp-to-html-file name sexp)
  (let ((out (output-file name)))
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
   (scandir (thisdir dir)
			(lambda (file) (not (or (string= file ".") (string= file "..")))))))

(define posts
  (filter-map
   (scheme-file-functor
	(lambda (name file)
	  (let ((p (dl-load file)))
		(post-set-name! p name)
		p)))
   (dirfiles "posts")))
(define public-posts
  (sort (filter post-time posts)
		(lambda (a b) (time>? (post-time a) (post-time b)))))

(for-each
 (lambda (post)
   (write-sexp-to-html-file
	(string-append "post/" (post-name post) ".html")
	(apply template (post-name post) #t (post-written-date post)
		   (cons*
			{h1 #(post-title post)}
			{p #(post-description post)}
			(post-body post)))))
 posts)

(define post-list
  (map
   (lambda (post)
	 {li {{a {href #(string-append "/post/" (post-name post))}}
		  #(post-title post) #"&ndash;" #(post-written-date post)}})
   public-posts))
(define (at-most n list)
  (take list (min n (length list))))
(define recent-posts
  {ul #@(at-most 10 post-list)})
(list-head '(1 2 3) 2)

(write-sexp-to-html-file "posts.html" (page {h1 All Posts} {ul #@post-list}))
(for-each
 (lambda (dir)
   (define ext (basename dir))
   (for-each
	(scheme-file-functor
	 (lambda (name file)
	   (write-sexp-to-html-file (string-append name "." ext) (dl-load file))))
	(dirfiles (string-append "pages/" ext))))
 (dirfiles "pages"))
