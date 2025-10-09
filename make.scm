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
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-69)
  #:export (build template page link-heading public-posts page->li))

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

(define-record-type <page>
  (make-page name parent uuid type type-pretty title published updated description body)
  page?
  (name         page-name         page-name!)
  (parent       page-parent       page-parent!)
  (uuid         page-uuid         page-uuid!)
  (type         page-type         page-type!)
  (type-pretty  page-type-pretty  page-type-pretty!)
  (title        page-title        page-title!)
  (published    page-published    page-published!)
  (updated      page-updated      page-updated!)
  (description  page-description  page-description!)
  (body         page-body         page-body!))
(define* (page-url page #:optional #:key full?)
  (if (blank-repost? page)
      {just #@(page-parent page)}
      (string-append
       (if full?
           "https://jackfaller.xyz/"
           "/")
       (page-type page) "/" (page-name page) ".html")))

(define (blank-repost? page)
  (and (string= (page-type page) "repost")
       (not (page-description page))))

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
  (define dirname (input-file dir))
  (->> (iter:iterator null ((port #f))
         (define port* (or port (opendir dirname)))
         (define out (readdir port*))
         (if (eof-object? out)
             (begin (closedir port*) (null))
             (values out port*)))
       (iter:remove (lambda (f) (or (string= f ".") (string= f ".."))))
       (iter:map (lambda (f) (string-append dirname "/" f)))))

(define note #f)
(define note-ref #f)
(define (page-published->? a b)
  (time>? (date->time-tai (page-published a)) (date->time-tai (page-published b))))
(define (page-published-y/m/d page) (date->string (page-published page) "~y/~m/~d"))
(define (page->li include-type?)
  (lambda (page)
    {li {{a {href #(page-url page)}}
         #(page-published-y/m/d page)
         #(and include-type? {just – #(page-type-pretty page)})
         –
         #@(page-title page)}}))
(define pretty-types
  (alist->hash-table
   '(("post" . "Blog Post")
     ("thought" . "Thought")
     ("repost" . "Repost")
     ("reply" . "Reply"))))
(define (date-ref page-alist date-name)
  (define date (assoc-ref page-alist date-name))
  (and date (read-date-string (car date))))
(define (doclisp->page name form)
  (define page (cdr form))
  (make-page
   name
   (assoc-ref page "parent")
   (car (assoc-ref page "uuid"))
   (caar form)
   (hash-table-ref pretty-types (caar form))
   (assoc-ref page "title")
   (date-ref page "published")
   (date-ref page "updated")
   (assoc-ref page "description")
   (or (assoc-ref page "body") '())))
(define (build-posts directory)
  (->>
   (dirfiles directory)
   (iter:map (scheme-file-functor
              (lambda (name file) (doclisp->page name (load file)))))
   (iter:filter identity)
   (iter:map
    (lambda (page)
      (unless (blank-repost? page)
        (write-form-to-file
         (string-append (page-type page) "/" (page-name page) ".html")
         html
         (template
          (cons*
           {h1 {#(if (or (string= (page-type page) "reply")
                         (string= (page-type page) "repost"))
                     {a {href #@(or (page-parent page)
                                    (error "Post missing parent: " (page-title page)))}}
                     "just")
                #@(page-title page)}}
           {p #@(page-description page)}
           (page-body page))
          #:blog-name (page-name page)
          #:published (page-published page)
          #:updated (page-updated page))))
      page))
   (iter:filter page-published)
   (iter:collect! (sink:list))
   ((cut sort <> page-published->?))))

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
  (define earliest (-est page-published time<?))
  (define latest (-est
                   (lambda (i) (or (page-updated i) (page-published i)))
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
          {{title {type text}} #@(page-title post)}
          {{content {type text/html} {src #(page-url post #:full? #t)}}}
          {published #(format-date (page-published post))}
          #(and (page-updated post) {updated #(format-date (page-updated post))})
          {{category {term #(page-type post)} {label #(page-type-pretty post)}}}
          {id urn:uuid:#(page-uuid post)}
          #(and (not (string= (page-type post) "repost"))
                {just
                 #author
                 {rights #(copyright (page-published post) (page-updated post))}
                 {{summary {type xhtml}}
                  {{div {xmlns http://www.w3.org/1999/xhtml}}
                   #@(page-description post)}}})
          {j:date #(page-published-y/m/d post)}})
       posts)}})

(define (build-pages ext path language loader)
  (iter:for-each!
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
