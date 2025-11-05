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
  #:export (build doclisp-template page link-heading public-posts page->li code-block))

(set-reader! doclisp-reader)

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

(define (code-block file-name)
  (define file (input-file (string-append "posts/" file-name)))
  {{pre {class block}} {{code {class block}} {raw #(cmd "./highlight.sh" file)}}})

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

(define-record-type <path>
  (make-path directory name extension)
  path?
  (directory path-directory)
  (name      path-name)
  (extension path-extension))

(define (path-is? path directory name extension)
  (and (string= name (path-name path))
       (string= directory (path-directory path))
       (string= extension (path-extension path))))

(define-record-type <page>
  (make-page path parent uuid type type-pretty title published updated description body)
  page?
  (path        page-path        page-path!)
  (parent      page-parent      page-parent!)
  (uuid        page-uuid        page-uuid!)
  (type        page-type        page-type!)
  (type-pretty page-type-pretty page-type-pretty!)
  (title       page-title       page-title!)
  (published   page-published   page-published!)
  (updated     page-updated     page-updated!)
  (description page-description page-description!)
  (body        page-body        page-body!))

(define* (page-url page #:optional #:key full? omit-leading-/?)
  (if (blank-repost? page)
      {just #@(page-parent page)}
      (string-append
       (cond (full? "https://jackfaller.xyz/")
             (omit-leading-/? "/")
             (else ""))
       (path-directory (page-path page))
       (if (string= "" (path-directory (page-path page)))
           ""
           "/")
       (path-name (page-path page))
       (string-append "." (path-extension (page-path page))))))

;; TODO: figure out error if I declare this above <page>
(define doclisp-template
  (case-lambda
    ((doclisp) (doclisp-template #f doclisp))
    ((path doclisp)
     (template (doclisp->page path doclisp)))))
(define (template page)
  (define article? (is-article? page))
  {just
   {{!DOCTYPE html}}
   {{html {prefix og: https://ogp.me/ns\# article: https://ogp.me/ns/article\#}}
    {head
     {title Jack Faller}
     {{meta {charset utf-8}}}
     {{meta {name viewport} {content width=device-width, initial-scale=1.0}}}
     #(and (page-path page)
           {{meta {property og:url} {content #(page-url page #:full? #t)}}})
     {{meta {property og:site_name} {content Jack Faller}}}
     ;; TODO: Make a version of the tiling pattern without dithering and use
     ;; it here as the og:image tag.
     #(and
       (page-description page)
       {{meta {property og:description}
              {content
               #@(if (length> (page-description page) 30)
                     (append (->> (iter:from-list (page-description page))
                                  (iter:take 30)
                                  (iter:collect! (sink:list)))
                             {#'join …})
                     (page-description page))}}})
     #(if article?
          {just {{meta {property og:type} {content article:article}}}
                #(and
                  (page-published page)
                  {{meta {property article:published_time}
                         {content #(date->string (page-published page) "~4")}}})
                #(and
                  (page-updated page)
                  {{meta {property article:updated_time}
                         {content #(date->string (page-updated page) "~4")}}})}
          {{meta {property og:type} {content website}}})
     {{meta {property og:title} {content #@(page-title page)}}}
     {{link {rel stylesheet} {type text/css} {href /style.css}}}
     {{link {rel blogroll} {type text/xml} {href /blogroll.xml}}}
     {{link {rel alternate} {type application/atom+xml} {href /atom.xml}}}}
    {body
     {main
      {header
       #(and (not (and (page-path page)
                       (path-is? (page-path page) "" "index" "html")))
             {{a {href /index.html} {title home} {class backarrow}} ←})
       #(and
         article?
         (if (page-published page)
             {{div {class date}} #(date-format (page-published page))}
             {{div {class date}} DRAFT}))}
      #@(cons*
         {h1 {#(if (or (string= (page-type page) "reply")
                       (string= (page-type page) "repost"))
                   {a {href #@(or (page-parent page)
                                  (error "Post missing parent: " (page-title page)))}}
                   "just")
              #@(page-title page)}}
         (and (page-description page) {p #@(page-description page)})
         (page-body page))
      #(and (page-published page)
            {{footer {id copy-notice}}
             #(copyright (page-published page) (page-updated page))})}}}})

(define (blank-repost? page)
  (and (string= (page-type page) "repost")
       (not (page-description page))))

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
(define (is-article? page)
  (hash-table-ref/default pretty-types (page-type page) #f))
(define (date-ref page-alist date-name)
  (define date (assoc-ref page-alist date-name))
  (and date (read-date-string (car date))))
(define (doclisp->page path form)
  (define page (cdr form))
  (make-page
   path
   (assoc-ref page "parent")
   (let ((a (assoc-ref page "uuid"))) (and a (car a)))
   (caar form)
   (hash-table-ref/default pretty-types (caar form) #f)
   (assoc-ref page "title")
   (date-ref page "published")
   (date-ref page "updated")
   (assoc-ref page "description")
   (or (assoc-ref page "body") '())))
(define (doclisp->post name form)
  (doclisp->page (make-path (caar form) name "html") form))
(define (build-posts directory)
  (->>
   (dirfiles directory)
   (iter:map (scheme-file-functor
              (lambda (name file) (doclisp->post name (load file)))))
   (iter:filter identity)
   (iter:map
    (lambda (page)
      (unless (blank-repost? page)
        (write-form-to-file
         (page-url page #:omit-leading-/? #t) html (template page)))
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
                 (doclisp-template (make-path "" (basename file ".scm") "html")
                                   (load file))))
  (build-pages "xsl" "" xslt
                (lambda (file)
                  (cons "just" (assoc-ref (cdr (load file)) "body"))))
  ;; TODO: archives.
  (write-form-to-file
   "atom.xml" xml
   (atom-feed-for (reverse (fluid-ref public-posts)) "atom.xml" #f #f)))
