(use-modules (doclisp)
             (ice-9 textual-ports))
(set-reader! doclisp-reader)

(define type (cadr (program-arguments)))
(define source (call-with-input-file (caddr (program-arguments)) doclisp-read))

(define title (cadr source))
(define date (caddr source))
(define description (cadddr source))
(define body (cddddr source))

(define strip-doclisp)

(write '(use-modules (doclisp) (make)))
(newline)
(write '(set-reader! doclisp-reader))
(newline)
(doclisp-write
 `(doclisp
   (,type ("version" ,',1))
   ("title" ,title)
   ,@(if date
         `(("date" ,(list 'unquote date)))
         '())
   ("uuid" ,(call-with-input-file "/proc/sys/kernel/random/uuid" get-string-all))
   ("description"
    ,@(if (string? description)
          (list description)
          (cddr description)))
   ,@(if (null? body)
         '()
         `(("body" ,@(map doclisp-embed-in-curly body))))))
(newline)
