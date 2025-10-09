(define-module (maths)
  #:use-module (utilities)
  #:use-module ((utilities iterators) #:prefix iter:)
  #:use-module ((utilities sinks) #:prefix sink:)
  #:use-module (doclisp)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 match)
  #:use-module (ice-9 unicode)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 hash-table)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-26)
  #:export (math math-block math-rewrite))

(set-reader! doclisp-reader)

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
