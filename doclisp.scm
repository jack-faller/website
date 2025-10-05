(define-module (doclisp)
  #:use-module (iterators)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-13)
  #:use-module (srfi srfi-69)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 textual-ports)
  #:export (set-reader! doclisp-reader write-form write-forms
            make-escaper escaper? escaper-hash-table escaper-write
            make-language language?
            language-write-tag
            language-escaper language-unescaper language-newline
            language-augment language-write-escaped language-write-unescaped
            xml html escaped xslt))

(define-syntax set-reader!
  (syntax-rules ()
    ((_ reader)
     (eval-when (compile eval)
       (fluid-set! current-reader reader)))))

(define (port-position port)
  (vector (port-filename port) (port-line port) (port-column port)))

(define (drop-space port)
  (let ((found-space #f))
    (while (let ((c (peek-char port)))
             (and (not (eof-object? c))
                  (char-whitespace? c)))
      (set! found-space #t)
      (read-char port))
    found-space))

(define (read-until f port)
  (define (peek-escape port)
    (define out (peek-char port))
    (if (eq? out #\\)
        (begin
          (read-char port)
          (list (peek-char port)))
        out))
  (string-unfold
   (lambda (c)
     (if (pair? c)
         (eof-object? (car c))
         (or (eof-object? c) (f c))))
   (lambda (c)
     (if (pair? c) (car c) c))
   (lambda (ign)
     (read-char port)
     (peek-escape port))
   (peek-escape port)))

(define (delimited-read end port table join?)
  (iter->list
   (iterate null recur ((first? #t))
     (let* ((dropped-space? (drop-space port))
            (peeked (peek-char port)))
       (cond
        ((eof-object? peeked) (error "Missing closing delimiter."))
        ((eq? peeked end) (read-char port) (null))
        ((or dropped-space? (not join?) first?)
         (let ((result (table-read port table)))
           (if (eq? result comment)
               (recur #f)
               (values result #f))))
        (else (values 'join #t)))))))

(define (table-read port table)
  (drop-space port)
  (define peeked (peek-char port))
  (define fun (hash-table-ref/default table peeked #f))
  (cond
   ((eof-object? peeked) peeked)
   (fun (read-char port) (fun port table))
   (else ((hash-table-ref table 'atom) port table))))

(define comment (substring/copy "comment" 0))
(define (list-syntax-pair start end)
  (cons start
        (lambda (port table)
          (fold-right
           (lambda (i rest)
             (cond
              ((not (or (eq? (syntax->datum i) '#{.}#)))
               (if (eq? comment i)
                   rest
                   (cons i rest)))
              ((not (= (length rest) 1)) (error "Malformed dotted list."))
              (else (car rest))))
           '()
           (delimited-read end port table #f)))))
(define (quote-syntax-pair char sym)
  (cons char
        (lambda (port table)
          (define pos (port-position port))
          (list (datum->syntax #f sym #:source pos) (table-read port table)))))
(define (unquote-reader port table)
  (let ((sym (if (eq? (peek-char port) #\@)
                 (begin (read-char port) 'unquote-splicing)
                 'unquote)))
    (list sym (table-read port table))))
(define (read-atom port table)
  (read-until (lambda (char) (or (char-whitespace? char)
                                 (hash-table-exists? table char)))
              port))
(define comment-reader-pair
  (cons #\; (lambda (port table)
              (read-line port)
              comment)))
(define (drop-block-comment port)
  (let ((got (cdr (read-delimited "|#" port 'split))))
    (cond
     ((eof-object? got)
      (error "Unterminated comment."))
     ((and (eq? got #\|) (eq? (peek-char port) #\#))
      (read-char port)
      comment)
     ((and (eq? got #\#) (eq? (peek-char port) #\|))
      (read-char port)
      (drop-block-comment port)
      (drop-block-comment port))
     (else
      (drop-block-comment port)))))

(define curly-read-table
  (alist->hash-table
   `((#\{ . ,(lambda (port table)
               (delimited-read #\} port curly-read-table #t)))
     ,comment-reader-pair
     (#\} . ,(lambda _ (error "Unexpected closing delimiter" #\})))
     (#\# . ,(lambda (port table)
               (if (eq? (peek-char port) #\|)
                   (begin (read-char port)
                          (drop-block-comment port)
                          (table-read port table))
                   (unquote-reader port default-read-table))))
     (atom . ,(lambda (port table)
                (define pos (port-position port))
                (datum->syntax #f (read-atom port table) #:source pos))))
   eq? hash-by-identity))
(define default-read-table
  (alist->hash-table
   `((atom
      . ,(lambda (port table)
         (if (eq? (peek-char port) #\")
             (read-syntax port)
             (let ((pos (port-position port)))
               (datum->syntax
                #f
                (call-with-input-string (read-atom port table) read)
                #:source pos)))))
     ,(list-syntax-pair #\( #\))
     ,(list-syntax-pair #\[ #\])
     ,(quote-syntax-pair #\' 'quote)
     ,(quote-syntax-pair #\` 'quasiquote)
     (#\, . ,unquote-reader)
     (#\{ . ,(lambda (port table)
               (cons 'doclisp-quasiquote
                     (list (delimited-read #\} port curly-read-table #t)))))
     ,comment-reader-pair
     (#\# . ,(lambda (port table)
               (case (peek-char port)
                 ((#\|)
                  (read-char port)
                  (drop-block-comment port))
                 ((#\()
                  (apply vector (table-read port table)))
                 (else
                  (unget-char port #\#)
                  (read port)))))
     ,@(map (lambda (a)
              (cons a (lambda _ (error "Unexpected closing delimiter" a))))
            '(#\) #\) #\})))
   eq? hash-by-identity))

(define (doclisp-reader port)
  (define val (table-read port default-read-table))
  (if (eq? val comment)
      (doclisp-reader port)
      val))

(define* (xml-tag-writer #:key should-self-close? needs-empty-body?)
  (lambda (name attributes body language port)
    (define (put s p) (language-write-unescaped language s p))
    (put "<" port)
    (put name port)
    (for-each
     (lambda (i)
       (put " " port)
       (if (string? i)
           (put i port)
           (begin
             (put (car i) port)
             (put "=\"" port)
             (regexp-substitute/global
              port "\""
              (call-with-output-string
                (lambda (port) (write-forms (cdr i) language port)))
              'pre "&quot;" 'post)
             (put "\"" port))))
     attributes)
    (if (and (null? body) (not needs-empty-body?))
        (put (if should-self-close? ">" " />") port)
        (begin
          (put ">" port)
          (write-forms body language port)
          (put "</" port)
          (put name port)
          (put ">" port)))))

(define-record-type <escaper>
  (make-escaper* hash-table regexp)
  escaper?
  (hash-table escaper-hash-table)
  (regexp escaper-regexp))
(define (key-regexp hash-table)
  (if (= 0 (hash-table-size hash-table))
      #f
      (let* ((sep "")
             (rx (call-with-output-string
                   (lambda (port)
                     (hash-table-walk
                      hash-table
                      (lambda (key _)
                        (display sep port)
                        (set! sep "|")
                        (display (regexp-quote key) port)))))))
        (make-regexp rx regexp/extended))))
(define (make-escaper hash-table)
  (make-escaper* hash-table (key-regexp hash-table)))
(define (escaper-write escaper string port)
  (define rx (escaper-regexp escaper))
  (define (map-match m)
    (hash-table-ref (escaper-hash-table escaper) (match:substring m)))
  (if rx
      (regexp-substitute/global port rx string 'pre map-match 'post)
      (display string port)))

(define-record-type <language>
  (make-language newline unescaper escaper write-tag)
  language?
  ;; (newline)
  (newline language-newline)
  (unescaper language-unescaper)
  (escaper language-escaper)
  ;; (write-tag name attributes body language port)
  (write-tag language-write-tag))


(define* (language-augment language #:key newline unescaper escaper write-tag)
  (make-language
   (or newline (language-newline language))
   (or unescaper (language-unescaper language))
   (or escaper (language-escaper language))
   (or write-tag (language-write-tag language))))


(define (language-write-unescaped language string port)
  (escaper-write (language-unescaper language) string port))
(define (language-write-escaped language string port)
  (escaper-write (language-escaper language) string port))

(define xml
  (make-language
   newline
   (make-escaper (alist->hash-table '()))
   (make-escaper
    (alist->hash-table
     '(("<" . "&lt;")
       (">" . "&gt;")
       ("&" . "&amp;"))))
   (let ((tag (xml-tag-writer)))
     (lambda (name attributes body language port)
       (cond
        ((equal? name "just") (write-forms body language port))
        ((equal? name "raw")
         (language-write-unescaped language (car body) port))
        ((equal? name "join")
         (for-each (lambda (x) (write-form x language port)) body))
        (else
         (tag name attributes body language port)))))))

(define self-closing-html-tags
  '("area" "base" "br" "col" "embed" "hr" "img" "input" "link" "meta" "param"
    "source" "track" "wbr" "!DOCTYPE"))
(define html
  (let ((tags (alist->hash-table
               (cons
                (cons "script" (xml-tag-writer #:needs-empty-body? #t))
                (map (lambda (x) (cons x (xml-tag-writer #:should-self-close? #t)))
                     self-closing-html-tags))))
        (parent (language-write-tag xml)))
    (language-augment
     xml
     #:write-tag
     (lambda (name attributes body language form)
       ((hash-table-ref/default tags name parent) name attributes body language form)))))
(define xslt
  (let ((parent (language-write-tag xml)))
    (language-augment
     xml
     #:write-tag
     (lambda (name attributes body language form)
       (unless (string-ci= name "!DOCTYPE")
         (parent name attributes body language form))))))
(define (escaped language)
  (define old-escaper (language-escaper language))
  (language-augment
   language
   #:unescaper old-escaper
   #:escaper
   (make-escaper
    (let ((out (hash-table-copy (escaper-hash-table old-escaper))))
      (hash-table-walk
       (escaper-hash-table old-escaper)
       (lambda (key value)
         (hash-table-set!
          out key
          (call-with-output-string
            (lambda (port)
              (escaper-write old-escaper value port))))))
      out))))

(define (write-forms forms language port)
  (unless (null? forms)
    (write-form (car forms) language port)
    (let loop ((forms (cdr forms)))
      (cond
       ((null? forms))
       ((eq? (car forms) 'join)
        (write-form (cadr forms) language port)
        (loop (cddr forms)))
       (else
        (language-write-escaped language " " port)
        (write-form (car forms) language port)
        (loop (cdr forms)))))))
(define (write-form form language port)
  (cond
   ((or (not form) (null? form)))
   ((string? form) (language-write-escaped language form port))
   ((not (pair? form))
    (error "Unexpected object in form->xml." form))
   ((language? (car form))
    (write-forms (cdr form) (car form) port))
   (else
    (receive (name attributes body)
        (if (pair? (car form))
            (values (caar form) (cdar form) (cdr form))
            (values (car form) '() (cdr form)))
      (if (procedure? name)
          (name attributes body language port)
          ((language-write-tag language) name attributes body language port))))))
