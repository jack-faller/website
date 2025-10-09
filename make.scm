(define-module (make)
  #:use-module (utilities)
  #:use-module ((utilities iterators) #:prefix iter:)
  #:use-module ((utilities sinks) #:prefix sink:)
  #:use-module (doclisp)
  #:use-module (ice-9 ftw)
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
  #:export (build template page link-heading public-posts post->li))

(set-reader! doclisp-reader)

;; TODO: add open Graph tags.
;; <meta property="og:type" content="">
;; <meta property="og:title" content="">
;; <meta property="og:description" content="">
;; <meta property="og:image" content="">

(define (cmd prog . args)
  (call-with-port (apply open-pipe* OPEN_BOTH prog args) get-string-all))

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


(define input-directory (make-fluid))
(define output-directory (make-fluid))
(define (input-file name) (string-append (fluid-ref input-directory) "/" name))
(define (output-file name)
  (let ((fname (string-append (fluid-ref output-directory) "/" name)))
    (system* "mkdir" "-p" (dirname fname))
    fname))

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
