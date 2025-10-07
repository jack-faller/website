(define-module (make)
  #:use-module (utilities)
  #:use-module ((utilities iterators) #:prefix iter:)
  #:use-module ((utilities sinks) #:prefix sink:)
  #:use-module (doclisp)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 unicode)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 hash-table)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-26)
  #:export (build template page link-heading math math-block math-rewrite public-posts post->li))

(set-reader! doclisp-reader)

;; TODO: add open Graph tags.
;; <meta property="og:type" content="">
;; <meta property="og:title" content="">
;; <meta property="og:description" content="">
;; <meta property="og:image" content="">

(define (cmd prog . args)
  (call-with-port (apply open-pipe* OPEN_BOTH prog args) get-string-all))

(define input-directory (make-fluid))
(define output-directory (make-fluid))
(define (input-file name) (string-append (fluid-ref input-directory) "/" name))
(define (output-file name)
  (let ((fname (string-append (fluid-ref output-directory) "/" name)))
    (system* "mkdir" "-p" (dirname fname))
    fname))

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

(define (math-rewrite terms)
  (define (detag terms)
    (cond ((not (pair? terms)) terms)
          ((eq? (car terms) 'tag) (caddr terms))
          (else (map detag terms))))
  (define (rewrite terms dictionary)
    (let rw ((terms terms))
      (cond ((not (pair? terms)) terms)
            ((eq? (car terms) 'tag)
             (let ((replacement (hash-ref dictionary (cadr terms) dictionary)))
               (if (eq? replacement dictionary) (map rw terms) replacement)))
            (else (map rw terms)))))
  ;; TODO: Make allow for an arbitrary expression with (get tag) substituted.
  ;; (get #f) gives the previous expression.
  (define (get-next acc expr)
    (if (eqv? expr #f) acc (error "TODO: this")))
  (cons
   (detag (car terms))
   (->> (iter:from-list (cdr terms))
        (iter:scan
         (lambda (acc next)
           (rewrite (get-next acc (car next)) (alist->hash-table (cdr next))))
         (car terms))
        (iter:map detag)
        (iter:collect! (sink:list)))))

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
       ;; TODO: This alignment doesn't work properly on some browsers.
       ;; The left column aligns left instead of right.
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

;; TODO: link footnote back up to point of reference.
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

(define (copyright published updated)
  {just
   ©
   {join
    #(number->string (date-year published))
    #(if (and updated (not (= (date-year published) (date-year updated))))
         {join – #(number->string (date-year updated))}
         "")}
   Jack Faller})

(define* (template body #:key blog-name published updated (wants-back-arrow? #t))
  {just
   {{!DOCTYPE html}}
   {html
    {head
     {title Jack Faller}
     {{meta {charset utf-8}}}
     {{link {rel stylesheet} {type text/css} {href /style.css}}}
     {{link {rel blogroll} {type text/xml} {href /blogroll.xml}}}
     {{link {rel alternate} {type application/atom+xml} {href /atom.xml}}}
     #(and blog-name
           {just
            {script const blog-name =
                    #(string-append "\"" blog-name "\"")}
            {{script {src comment-script.js} defer}}})}
    {body
     {main
      {header
       #(and wants-back-arrow?
             {{a {href /index.html} {title home} {class backarrow}} ←})
       #(and
         blog-name
         (if published
             {{div {class date}} #(date-format published)}
             {{div {class date}} DRAFT}))}
      #@body
      #(and published
            {{footer {id copy-notice}} #(copyright published updated)})}}}})
(define (code-block file-name)
  {pre {{code {class block}}
        {raw
         #(cmd "highlight" (input-file (string-append "posts/" file-name))
               "-O" "html" "--inline-css" "--fragment"
               "--line-numbers" "--line-number-length" "3")}}})
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
  (make-post name parent uuid type type-pretty title published updated description body)
  post?
  (name         post-name         post-name!)
  (parent       post-parent       post-parent!)
  (uuid         post-uuid         post-uuid!)
  (type         post-type         post-type!)
  (type-pretty  post-type-pretty  post-type-pretty!)
  (title        post-title        post-title!)
  (published    post-published    post-published!)
  (updated      post-updated      post-updated!)
  (description  post-description  post-description!)
  (body         post-body         post-body!))
(define* (post-url post #:optional #:key full?)
  (if (blank-repost? post)
      {just #@(post-parent post)}
      (string-append
       (if full?
           "https://jackfaller.xyz/"
           "/")
       (post-type post) "/" (post-name post) ".html")))

(define (blank-repost? post)
  (and (string= (post-type post) "repost")
       (not (post-description post))))

(define (page . body) (template body))

(define (write-form-to-file name language sexp)
  (call-with-output-file (output-file name)
    (lambda (port) (write-form sexp language port))))

(define (scheme-file-functor f)
  (lambda (file)
    (if (string-suffix? ".scm" file)
        (f (basename file ".scm") file)
        #f)))
(define (dirfiles dir)
  (map
   (lambda (x) (string-append (input-file dir) "/" x))
   (scandir (input-file dir)
            (lambda (file) (not (or (string= file ".") (string= file "..")))))))

(define note #f)
(define note-ref #f)
(define (post-published->? a b)
  (time>? (date->time-tai (post-published a)) (date->time-tai (post-published b))))
(define (post-published-y/m/d post) (date->string (post-published post) "~y/~m/~d"))
(define (post->li include-type?)
  (lambda (post)
    {li {{a {href #(post-url post)}}
         #(post-published-y/m/d post)
         #(and include-type? {just – #(post-type-pretty post)})
         –
         #@(post-title post)}}))
(define pretty-types
  (alist->hash-table
   '(("post" . "Blog Post")
     ("thought" . "Thought")
     ("repost" . "Repost")
     ("reply" . "Reply"))))
(define (date-ref post-alist date-name)
  (define date (assoc-ref post-alist date-name))
  (and date (read-date-string (car date))))
(define (build-posts directory)
 (define posts
   (filter-map
    (scheme-file-functor
     (lambda (name file)
       (receive (n nr format-notes) (footnotes)
         (set! note n)
         (set! note-ref nr)
         (let*
             ((content (load file))
              (post (cdr content))
              (post (make-post
                     name
                     (assoc-ref post "parent")
                     (car (assoc-ref post "uuid"))
                     (caar content)
                     (hash-ref pretty-types (caar content))
                     (assoc-ref post "title")
                     (date-ref post "published")
                     (date-ref post "updated")
                     (assoc-ref post "description")
                     {#@(or (assoc-ref post "body") '()) #(format-notes)})))
           (set! note #f)
           (set! note-ref #f)
           post))))
    (dirfiles directory)))
 (for-each
  (lambda (post)
    (unless (blank-repost? post)
      (write-form-to-file
       (string-append (post-type post) "/" (post-name post) ".html")
       html
       (template
        (cons*
         {h1 {#(if (or (string= (post-type post) "reply")
                       (string= (post-type post) "repost"))
                   {a {href #@(or (post-parent post)
                                  (error "Post missing parent: " (post-title post)))}}
                   "just")
              #@(post-title post)}}
         {p #@(post-description post)}
         (post-body post))
        #:blog-name (post-name post)
        #:published (post-published post)
        #:updated (post-updated post)))))
  posts)
 (sort (filter post-published posts) post-published->?))

(define (atom-feed-for posts this-page prev-archive next-archive)
  (define (format-date date) (regexp-substitute/global
                              #f ".[^Z]$" (date->string date "~4")
                              'pre ":" 0))
  (define (-est get-date compare)
    (->> (iter:from-list posts)
         (iter:map get-date)
         (iter:map date->time-tai)
         (iter:collect! (sink:reduce (lambda (a b) (if (compare a b) a b)) #f))
         (time-tai->date)))
  (define earliest (-est post-published time<?))
  (define latest (-est
                   (lambda (i) (or (post-updated i) (post-published i)))
                   time>?))
  (define author
    {author
     {name Jack Faller}
     {uri https://jackfaller.xyz}
     {email jack.t.faller@gmail.com}})
  {just
   {? xml version="1.0" encoding="UTF-8"}
   {? xml-stylesheet type="text/xsl" href="./atom.xsl"}
   {{feed {xml:lang en-GB}
          {xmlns http://www.w3.org/2005/Atom}
          {xmlns:j http://jackfaller.xyz}}
    {{title {type text}} Jack Faller}
    {{subtitle {type text}} All the stuff from me.}
    #author
    {updated #(format-date latest)}
    {rights #(copyright earliest latest)}
    {id urn:uuid:4a904a9b-e398-4527-9db3-8a31426e4047}
    {{generator {uri https://github.com/jack-faller/website}} Doclisp}
    {icon https://jackfaller.xyz/favicon.ico}
    ;; TODO
    ;; {logo }
    {{link {rel self} {href https://jackfaller.xyz/#this-page}}}
    {{link {rel alternate} {type text/html} {href https://jackfaller.xyz}}}
    #(if next-archive
         {just
          {{link {rel next-archive} {href https://jackfaller.xyz/#next-archive}}}
          {{link {rel current} {href https://jackfaller.xyz/atom.xml}}}}
         #f)
    #(if prev-archive
         {{link {rel prev-archive} {href https://jackfaller.xyz/#prev-archive}}}
         #f)
    #@(map
       (lambda (post)
         {entry
          {{title {type text}} #@(post-title post)}
          {{content {type text/html} {src #(post-url post #:full? #t)}}}
          {published #(format-date (post-published post))}
          #(and (post-updated post) {updated #(format-date (post-updated post))})
          {{category {term #(post-type post)} {label #(post-type-pretty post)}}}
          {id urn:uuid:#(post-uuid post)}
          #(and (not (string= (post-type post) "repost"))
                {just
                 #author
                 {rights #(copyright (post-published post) (post-updated post))}
                 {{summary {type xhtml}}
                  {{div {xmlns http://www.w3.org/1999/xhtml}}
                   #@(post-description post)}}})
          {j:date #(post-published-y/m/d post)}})
       posts)}})

(define (build-pages ext path language loader)
  (for-each
   (scheme-file-functor
    (lambda (name file)
      (write-form-to-file
       (string-append path name "." ext) language (loader file))))
   (dirfiles (string-append "pages/" ext))))

(define public-posts (make-fluid))

(define (build arguments)
  (fluid-set! input-directory (cadr arguments))
  (fluid-set! output-directory (caddr arguments))

  (fluid-set! public-posts (build-posts "posts"))

  (build-pages "html" "" html
                (lambda (file)
                  (define content (cdr (load file)))
                  (template
                   (assoc-ref content "body")
                   #:wants-back-arrow?
                   (car (or (assoc-ref content "wants-back-arrow?") '(#t)))
                   #:published (date-ref content "published")
                   #:updated (date-ref content "updated"))))
  (build-pages "xsl" "" xslt
                (lambda (file)
                  (cons "just" (assoc-ref (cdr (load file)) "body"))))
  ;; TODO: archives.
  (write-form-to-file
   "atom.xml" xml
   (atom-feed-for (reverse (fluid-ref public-posts)) "atom.xml" #f #f)))


