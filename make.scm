(use-modules (ice-9 ftw)
			 (ice-9 match)
			 (ice-9 unicode)
			 (ice-9 receive)
			 (srfi srfi-19))

(define (style-vect-index capital? italic? bold?)
  (+ (if capital? 4 0) (if italic? 2 0) (if bold? 1 0)))
(define (char-- a b) (- (char->integer a) (char->integer b)))
(define (char-+ a b) (integer->char (+ (char->integer a) b)))
(define (style-vect-get vect letter capital? italic? bold?)
  (let* ((start (vector-ref vect (style-vect-index capital? italic? bold?))))
	(if start (char-+ start letter) #f)))

(define* (digit number capital? italic? bold? #:optional style)
  (style-vect-get
   (case style
	 ((#f) #(#\0 #\𝟎 #f #f #f #f #f #f))
	 ((sans-serif) #(#\𝟢 #\𝟬 #f #f #f #f #f #f))
	 ((monospace) #(#\𝟶 #f #f #f #f #f #f #f))
	 ((double-struck) #(#\𝟘 #f #f #f #f #f #f #f)))
   number capital? italic? bold?))
(define latin-letter-length 28)
(define* (latin-letter letter capital? italic? bold? #:optional style)
  (define (get v) (style-vect-get v letter capital? italic? bold?))
  (case style
	((#f)
	 (or (and (= letter (char-- #\h #\a)) (not capital?) italic? (not bold?)
			  #\ℎ)
		 (if (< letter 26)
			 (get #(#\a #\𝐚 #\𝑎 #\𝒂 #\A #\𝐀 #\𝐴 #\𝑨))
			 (style-vect-get
			  (if (= letter 26)
				  #(#\ı #f #\𝚤 #f #f #f #f #f)
				  #(#\ȷ #f #\𝚥 #f #f #f #f #f))
			  0 capital? italic? bold?))))
	((sans-serif)
	 (get #(#\𝖺 #\𝗮 #\𝘢 #\𝙖 #\𝖠 #\𝗔 #\𝘈 #\𝘼)))
	((fraktur)
	 (or
	  (and capital? (not italic?) (not bold?)
		   (let ((c (char-+ #\A letter)))
			 (case c
			   ((#\C #\H #\J #\R #\Z)
				(formal-name->char
				 (string-append "BLACK-LETTER CAPITAL " (string c))))
			   (else #f)))))
	 (get #(#\𝔞 #\𝖆 #f #f #\𝔄 #\𝕬 #f #f)))
	((script)
	 (or
	  (and (not bold?) (not italic?)
		   (let ((c (char-+ #\A letter)))
			 (if capital?
				 (case c
				   ((#\B #\E #\F #\H #\I #\L #\M #\R)
					(formal-name->char
					 (string-append "SCRIPT CAPITAL " (string c))))
				   (else #f))))
		   (get #(#\𝒶 #\𝓪 #f #f #\𝒜 #\𝓐 #f #f)))))
	((monospace)
	 (get #(#\𝚊 #f #f #f #\𝙰 #f #f #f)))
	((double-struck)
	 (or
	  (and capital? (not italic?) bold?
		   (let ((c (char-+ #\A letter)))
			 (case c
			   ((#\C #\H #\N #\P #\Q #\R #\Z)
				(formal-name->char
				 (string-append "DOUBLE-STRUCK CAPITAL " (string c))))
			   (else #f))))
	  (get #(#f #\𝕒 #f #f #f #\𝔸 #f #f))))))

(define greek-letter-length (+ 3 (char-- #\𝛡 #\𝛂)))
(define* (greek-letter letter capital? italic? bold? #:optional style)
  (define (get letter) (style-vect-get
						#(#\α #\𝛂 #\𝛼 #\𝜶 #\Α #\𝚨 #\𝛢 #\𝞐)
						letter capital? italic? bold?))
  ;; Use bold letters here as they give better indices.
  (cond
   (style #f)
   ((<= 0 letter (char-- #\𝛚 #\𝛂))
	(if (and (= letter (char-- #\𝛓 #\𝛂)) capital?)
		#f
		(get letter)))
   ((<= (char-- #\𝛛 #\𝛂) letter (char-- #\𝛡 #\𝛂))
	(cond
	 (capital?
	  (cond
	   ((not (= letter (char-- #\𝛝 #\𝛂))) #f)
	   ((or italic? bold?) (get (char-- #\𝚹 #\𝚨)))
	   (else #\ϴ)))
	 ((or italic? bold?) (get letter))
	 (else (vector-ref #(#\∂ #\ϵ #\ϑ #\ϰ #\ϕ #\ϱ #\ϖ)
					   (- letter (char-- #\𝛛 #\𝛂))))))
   ((= letter (+ 1 (char-- #\𝛡 #\𝛂)))
	(style-vect-get #(#\ϝ #\𝟋 #f #f #\Ϝ #\𝟊 #f #f) 0 capital? italic? bold?))
   ((= letter (+ 2 (char-- #\𝛡 #\𝛂)))
	(cond
	 (capital? #f)
	 ((or italic? bold?)
	  (set! capital? (not capital?))
	  (get (char-- #\𝛁 #\𝚨)))
	 (else #\∇)))
   (else #f)))

(define (simple-name char)
   (define match
	 (string-match "(FINAL )?(DOTLESS )?([A-Z]*)( SYMBOL)?$" (char->formal-name char)))
   (define name (string-append
				 (if (or (match:start match 1) (match:start match 2) (match:start match 4))
					 "alt" "")
				 (match:substring match 3)))
   (string-downcase (if (equal? name "DIFFERENTIAL") "pd" name)))


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

(define-record-type <expression>
  (make-expression* mathml operator left-fence? right-fence? top-fence? bottom-fence?)
  expression?
  (mathml expression-mathml)
  (operator expression-operator)
  (left-fence? expression-left-fence?)
  (right-fence? expression-right-fence?)
  (top-fence? expression-top-fence?)
  (bottom-fence? expression-bottom-fence?))
(define* (make-expression mathml operator left-fence? right-fence?
						  #:optional (top-fence? #t) (bottom-fence? #t))
  (make-expression* mathml operator left-fence? right-fence? top-fence? bottom-fence?))
(define* (derived-expression e new-mathml)
  (make-expression new-mathml
				   (expression-operator e)
				   (expression-left-fence? e)
				   (expression-right-fence? e)
				   (expression-top-fence? e)
				   (expression-bottom-fence? e)))
(define* (atom-expression mathml #:optional operator)
  (make-expression mathml operator #t #t))

(define (math->mathml expr) (expression-mathml (math->expression expr)))
(define (math expr) {math #(math->mathml expr)})
(define (math-block expr) {{math {display block}} #(math->mathml expr)})
;; Values returned are expr, operator, left-fence, right-fence.
(define (math-apply f . args)
  (apply apply (hashq-ref math-definitions f) f args))
(define* (math->expression expr)
  (cond
   ((number? expr) (if (<= 0 expr)
					   (atom-expression {mn #(number->string expr)} 'number)
					   (math-apply 'neg (- expr) '())))
   ((string? expr) (atom-expression {mi #expr}))
   ((symbol? expr) (hashq-ref math-definitions expr))
   ((eq? expr #f) (atom-expression #f))
   ((pair? expr)
	(if (symbol? (car expr))
		(math-apply (car expr) (cdr expr))
		(apply (hashq-ref math-definitions 'apply) 'apply expr)))
   (else expr)))
(define (expression-wrap expr)
  {mrow {{mo {fence true} {stretchy true}} \(}
		#(expression-mathml expr)
		{{mo {fence true} {stretchy true}} \)}})
(define (math-wrap expr) (expression-wrap (math->expression expr)))
(define (expression-wrap-unless cond expr)
  (if cond (expression-mathml expr) (expression-wrap expr)))
(define (expression-wrap-if cond expr)
  (if cond (expression-wrap expr) (expression-mathml expr)))

(define (wrap-left op e1)
  (let ((e1 (math->expression e1)))
	(expression-wrap-unless
	 (or
	  (precedence-> (expression-operator e1) op)
	  (and (precedence-= op (expression-operator e1))
		   (left-associative? (expression-operator e1))
		   (left-associative? op))
	  (expression-right-fence? e1))
	 e1)))
(define (wrap-right op e2)
  (let ((e2 (math->expression e2)))
	(expression-wrap-if
	 (or (not (or
			   (precedence-> (expression-operator e2) op)
			   (and (precedence-= op (expression-operator e2))
					(right-associative? (expression-operator e2))
					(right-associative? op))
			   (expression-left-fence? e2)))
		 (and (eq? op '*) (or (eq? (expression-operator e2) '/)
							  (eq? (expression-operator e2) 'number))))
	 e2)))
(define (wrap-lest cond e)
	(define wrapped (expression-wrap-unless cond e))
	(if cond
		(derived-expression e {mrow #wrapped})
		(atom-expression wrapped)))

(define (wrap-top e)
  (wrap-lest (expression-bottom-fence? e) e))
(define (wrap-bottom e)
  (wrap-lest (expression-top-fence? e) e))
(define (operate op op-mathml e1 e2)
  (make-expression
   {just #(wrap-left op e1) #op-mathml #(wrap-right op e2)}
   op #f #f))

(define (infix mathml)
  (lambda (name first . args)
	(let loop ((acc (math->expression first))
			   (args args))
	  (match args
		(() acc)
		((and (((and symbol other-name (? (lambda (n) (precedence-= n name))))
				#f . args) . rest))
		 (loop (math-apply other-name acc args) rest))
		((b . rest)
		 (loop (operate name mathml acc b) rest))))))

(define (prefix mathml)
  (lambda (name a)
	(make-expression {just #mathml #(wrap-right name a)} name #t #f)))

;; TODO: Use mo movable-limits property in sum.

(define math-definitions
  (let ((it {mo {raw &it\;}})
		(af {mo {raw &af\;}}))
	(alist->hashq-table
	 `((inf . ,(atom-expression {mi ∞}))
	   (comment . ,(lambda (name e comment)
					 (define e* (math->expression e))
					 (derived-expression
					  e*
					  {just #(expression-mathml e*) {mtext #comment}})))
	   (dots . ,(atom-expression {mi …}))
	   (lim . ,(lambda (name under expr)
				 ((prefix {just
						   {munder
							{mi lim}
							{mrow #(math->mathml (caar under))
								  {mo →} #(math->mathml (cadar under))}}
						   #it {{mspace {width 0.1667em}}}})
				  name expr)))
	   (^ . ,(lambda (name a b)
			   (define a* (math->expression a))
			   (define b* (math->expression b))
			   (make-expression
				{msup
				 #(wrap-left name (wrap-bottom a*))
				 {mrow #(expression-wrap-if (eq? name (expression-operator b*))
											b*)}}
				name #f #t)))
	   (logb . ,(lambda (name base)
				  (atom-expression {msub {mi log} {mrow #(math->mathml base)}})))
	   (apply . ,(lambda* (name f #:optional x)
				   (define f* {{mi {mathvariant normal}} #f})
				   (if (not x)
					   (if (string? f) f* (math->expression f))
					   (make-expression
						{mrow
						 #(if (string? f) f* (wrap-left name (math->expression f)))
						 #af #@(cdr (math-wrap x))}
						name #f #f))))
	   (/ . ,(lambda (name a b)
			   (make-expression
				{mfrac #(expression-mathml (wrap-top (math->expression a)))
					   #(expression-mathml (wrap-bottom (math->expression b)))}
				'/ #t #t #f #f)))
	   (stack . ,(lambda (name . rest)
				   (define (row x)
					 {mtr {{mtd {class math-stack-left}}
						   #(wrap-left (car x) (cadr x))}
						  {{mtd {class math-stack-right}}
						   #(math->mathml (cons* (car x) #f (cddr x)))}})
				   (atom-expression {mtable #@(map row rest)})))
	   (* . ,(infix it))
	   (+ . ,(infix {mo +}))
	   (- . ,(infix {mo −}))
	   (neg . ,(prefix {mo −}))
	   (pos . ,(prefix {mo +}))
	   (dot . ,(infix {mo ·}))
	   (x . ,(infix {mo ×}))
	   (= . ,(infix {mo =}))
	   (~~ . ,(infix {mo ≈}))))))

;; ("f") yeilds function f without italic instead of f()
;; TODO:
;; (f "ibscfsmd" "string") font
;; italic bold sans-serif calligraphy fraktur monospace double-struck
;; (+ a (- #f b) c) => a - b + c

;; Lists which dictate the order of evaluation of operators.
;; Earlier operators evaluate sooner.
(define-values (precedence-> precedence-=)
  (let* ((inequalities
		  '((^ apply * dot + sum =)))
		 (equalities
		  '((= ~~)
			(sum lim)
			(+ -)
			(dot x div)
			(* neg pos)))
		 (canonical-table
		  (alist->hashq-table
		   (append-map
			(lambda (group)
			  (define canon (car group))
			  (map (lambda (elt) (cons elt canon)) group))
			equalities)))
		 (canonical (lambda (a) (hashq-ref canonical-table a a))))
   (values
	(lambda (a b)
	  (find (lambda (i) (eq? i (canonical b)))
			(cdr (or (find-tail (lambda (i) (eq? i (canonical a)))
								(car inequalities))
					 '(())))))
	(lambda (a b)
	  (eq? (canonical a) (canonical b))))))

(define (script-operator? op)
  (eq? op '^))
(define (left-associative? op)
  (not (right-associative? op)))
(define (right-associative? op)
  (or (eq? op '^)))

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
	  {msup {{a #(link-href "footnote" id)} #(number->string count)}}))
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
