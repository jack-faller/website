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
  {{div {id heading-#id} }
   {#(string-append "h" (number->string depth))
	#@(let loop ((result '()) (list text) (link? #t))
		(if (null? list)
			(reverse result)
			(loop (cons ((if link? link identity) (car list))
						result)
				  (cdr list)
				  (not link?))))
	{{span {class sectionmark}} #(link "&sect;")}}})

(define (footnotes)
  (define count 0)
  (define notes '())
  (define (format-notes)
	(if (= count 0)
		{just}
		{just
		 {hr}
		 {footer
		  #@(map
			 (lambda (vals)
			   (let ((id (car vals)) (count (cadr vals)) (text (cddr vals)))
				 {{div {id {join footnote- #id}}}
				  {{a #(link-href "footnote" id)}
				   [#(number->string count)]}
				  #@text
				  #"<br>"}))
			 (reverse notes))}}))
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
	 {header
	  #(and wants-back-arrow?
			{{a {href /} {title home} {class backarrow}} #"&larr;"})
	  #(and date {{div {class date}} #date})}
	 #@body}}})
;; Remember to put .codequote on inline code blocks to avoid word breaking.
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
  (name         post-name         post-name!)
  (time         post-time         post-time!)
  (title        post-title        post-title!)
  (date         post-date         post-date!)
  (written-date post-written-date post-written-date!)
  (description  post-description  post-description!)
  (body         post-body         post-body!))

(define (post title date description . body)
  (let* ((date (and date (read-date-string date)))
		 (sort (and date (date->time-tai date)))
		 (written-date (if date (date-format date) "DRAFT")))
	(make-post #f title sort (or date 'draft) written-date description body)))

(define (page . body) (apply template #f #t #f body))
(define (home-page . body) (apply template #f #f #f body))

(system* "rm" "-rf" (thisdir "generated"))
(define (output-file name)
  (let ((fname (thisdir (string-append "generated/" name))))
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

(define (at-most n list)
  (take list (min n (length list))))

(define note #f)
(define note-ref #f)
(define (post-like-dir dirname-plural dirname-singular)
 (define posts
   (filter-map
	(scheme-file-functor
	 (lambda (name file)
	   (receive (n nr format-notes) (footnotes)
		 (set! note n)
		 (set! note-ref nr)
		 (let ((p (dl-load file)))
		   (post-name! p name)
		   (post-body! p `(,@(post-body p) ,(format-notes)))
		   (set! note #f)
		   (set! note-ref #f)
		   p))))
	(dirfiles dirname-plural)))
 (define public-posts
   (sort (filter post-time posts)
		 (lambda (a b) (time>? (post-time a) (post-time b)))))

 (for-each
  (lambda (post)
	(write-sexp-to-html-file
	 (string-append dirname-singular "/" (post-name post) ".html")
	 (apply template (post-name post) #t (post-written-date post)
			(cons*
			 {h1 #(post-title post)}
			 {p #(post-description post)}
			 (post-body post)))))
  posts)

 (define post-list
   (map
	(lambda (post)
	  {li {{a {href #(string-append "/" dirname-singular "/"
									(post-name post) ".html")}}
		   #(post-title post) #"&ndash;" #(post-written-date post)}})
	public-posts))

 (write-sexp-to-html-file
  (string-append dirname-plural ".html")
  (page {h1 All #(string-append
				  (string-upcase (substring dirname-plural 0 1))
				  (substring dirname-plural 1))}
		{ul #@post-list}))

 (define recent-posts {ul #@(at-most 10 post-list)})
 (values post-list public-posts recent-posts))

(define post-list)
(define public-posts)
(define recent-posts)
(receive (a b c) (post-like-dir "posts" "post")
  (set! post-list a) (set! public-posts b) (set! recent-posts c))
(define thought-list)
(define public-thoughts)
(define recent-thoughts)
(receive (a b c) (post-like-dir "thoughts" "thought")
  (set! thought-list a) (set! public-thoughts b) (set! recent-thoughts c))

(for-each
 (lambda (dir)
   (define ext (basename dir))
   (for-each
	(scheme-file-functor
	 (lambda (name file)
	   (write-sexp-to-html-file (string-append name "." ext) (dl-load file))))
	(dirfiles (string-append "pages/" ext))))
 (dirfiles "pages"))
