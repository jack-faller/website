(use-modules (ice-9 ftw)
			 (ice-9 receive)
			 (srfi srfi-19))

(define (link-href prefix id)
  {href #(string-append "#" prefix "-" id)})

(define (link-heading depth id . text)
  (define (link . body)
	{{a {class headinglink} #(link-href "heading" id)}
	 #@body})
  {{div {id heading-#id}}
   {#(string-append "h" (number->string depth))
	#@(let loop ((result '()) (list text) (link? #t))
		(if (null? list)
			(reverse result)
			(loop (cons ((if link? link identity) (car list))
						result)
				  (cdr list)
				  (not link?))))
	{{span {class sectionmark}} #(link "§")}}})

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

(define* (template root body #:key (blog-name #f) (date #f) (wants-back-arrow? #t))
  {just
   {{!DOCTYPE html}}
   {html
	{head
	 {title Jack Faller}
	 {{meta {charset utf-8}}}
	 {{link {href {join #root /style.css}} {rel stylesheet} {type text/css}}}
	 #(and blog-name
		   {just
			{script const blog-name =
					#(string-append "\"" blog-name "\"")}
			{{script {src comment-script.js} defer}}})}
	{body
	 {main
	  {header
	   #(and wants-back-arrow?
			 {{a {href {join #root /index.html}} {title home} {class backarrow}} ←})
	   #(cond
		 (date {{div {class date}} #(date-format date)})
		 (blog-name {{div {class date}} DRAFT})
		 (else #f))}
	  #@body
	  {{footer {id copy-notice}}
	   ©
	   {join
		#(if (and date (not (= (date-year date) (date-year (current-date)))))
			 {join #(number->string (date-year date)) –}
			 "")
		#(number->string (date-year (current-date)))}
	   Jack Faller}}}}})
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
  (make-post name type dir title time date description body)
  post?
  (name         post-name         post-name!)
  (type         post-type         post-type!)
  (dir          post-dir          post-dir!)
  (time         post-time         post-time!)
  (title        post-title        post-title!)
  (date         post-date         post-date!)
  (description  post-description  post-description!)
  (body         post-body         post-body!))
(define (post-link root post)
  (string-append root "/" (post-dir post) "/" (post-name post) ".html"))

(define (post title date description . body)
  (let* ((date (and date (read-date-string date)))
		 (sort (and date (date->time-tai date))))
	(make-post #f #f #f title sort date description body)))

(define (page root . body) (template root body))
(define (home-page . body) (template "." body #:wants-back-arrow? #f))

(system* "rm" "-rf" (thisdir "generated"))
(define (output-file-name name)
  (let ((fname (thisdir (string-append "generated/" name))))
	(system* "mkdir" "-p" (dirname fname))
	fname))
(define (write-sexp-to-html-file name sexp)
  (call-with-output-file (output-file-name name)
	(lambda (port) (write-sexp->html sexp port))))

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
(define (post-time->? a b) (time>? (post-time a) (post-time b)))
(define (post->li root include-type?)
  (lambda (post)
	{li {{a {href #(post-link root post)}}
		 #(date->string (post-date post) "~y/~m/~d")
		 #(and include-type? {just – #(post-type post)})
		 –
		 #(post-title post)}}))

(define (write-posts-to-file file-name title include-type? posts)
  (write-sexp-to-html-file
   file-name
   (page "." {h1 #title} {ul #@(map (post->li "." include-type?) posts)})))
(define (post-like-dir dirname-plural dirname-singular post-type)
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
		   (post-type! p post-type)
		   (post-dir! p dirname-singular)
		   (set! note #f)
		   (set! note-ref #f)
		   p))))
	(dirfiles dirname-plural)))
 (define public-posts (sort (filter post-time posts) post-time->?))
 (for-each
  (lambda (post)
	(write-sexp-to-html-file
	 (string-append dirname-singular "/" (post-name post) ".html")
	 (template
	  ".."
	  (cons*
	   {h1 #(post-title post)}
	   {p #(post-description post)}
	   (post-body post))
	  #:blog-name (post-name post) #:date (post-date post))))
  posts)
 public-posts)

(define public-posts (post-like-dir "posts" "post" "Blog Post"))
(define public-thoughts (post-like-dir "thoughts" "thought" "Thought"))
(define public-stuff (merge public-thoughts public-posts post-time->?))
(write-posts-to-file "thoughts.html" "All Thoughts" #f public-thoughts)
(write-posts-to-file "posts.html" "All Blog Posts" #f public-posts)
(write-posts-to-file "stuff.html" "All Stuff" #t public-stuff)

(define stream-size 60)
;; Technically this is incorrect as it uses HTML rather than XML.
(define (rss-stream title include-type? posts description)
  (define (rfc-822 date) (date->string date "~a, ~d ~b ~T ~z"))
  {just
   {raw #"<?xml version=\"1.0\" encoding=\"UTF-8\" ?>"}
   {{rss {version 2.0}}
	{channel
	 {title #title}
	 {link https://jackfaller.xyz/}
	 {description #description}
	 {lastBuildDate #(rfc-822 (current-date))}
	 #@(map
		(lambda (post)
		  {item
		   {title
			#(and include-type? {just #(post-type post) –})
		  	#(post-title post)}
		   {link #(post-link "https://jackfaller.xyz" post)}
		   {pubDate #(rfc-822 (post-date post))}
		   {description
			#(let ((CD-begin "<![CDATA[") (CD-end "]]>")
				   (desc (sexp->html (post-description post))))
			   (when (string-contains desc CD-end)
				 (error "Post description contains CDATA end string:" CD-end))
			   (string-append CD-begin desc CD-end))}})
		(at-most stream-size posts))}}})

(for-each
 (lambda (dir)
   (define ext (basename dir))
   (for-each
	(scheme-file-functor
	 (lambda (name file)
	   (write-sexp-to-html-file (string-append name "." ext) (dl-load file))))
	(dirfiles (string-append "pages/" ext))))
 (dirfiles "pages"))
