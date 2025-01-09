(define-library (iterators)
  (export
   ;; Primitive creation.
   iterator iterator?
   iterator-defer! iterator-replace!
   iter-null iter-null? iter-or
   ;; TODO: (iterate-for[*!] null recur ((var iter)) ((var val)) body)
   iterate iterate*
   let-next if-let-next apply-to-next apply-to-next* iter-for iter-for!

   ;; Access.
   iter-find iter-next iter-peek

   ;; Creation.
   list->iter repeat-list->iter pairs->iter
   string->iter repeat-string->iter
   vector->iter repeat-vector->iter
   port->char-iter
   iter-forever iter-repeat
   tree->iter
   iter-range iter-iota

   ;; Manipulation.
   iter-map iter-scan iter-filter iter-remove
   iter-take iter-take-while iter-drop iter-drop-while
   iter-flatten iter-recursive-flatten
   iter-duplicate
   iter-cons
   iter-zip

   ;; Collection.
   iter-any iter-every iter-count
   iter->string iter->list iter->vector
   iter-for-each iter-run iter-last
   iter-fold iter-reduce)

  (import (scheme base) (scheme case-lambda))
  ;; R6RS version.
  ;; (import (rnrs control (6))
  ;; 		  (rnrs io ports (6))
  ;; 		  (rnrs mutable-pairs (6))
  ;; 		  (srfi 6)	;; String ports.
  ;; 		  (srfi 9)	;; Records.
  ;; 		  (srfi 16)	;; Case lambda.
  ;; 		  ;; Can't import in guile.
  ;; 		  ;; (srfi 46) ;; Syntax rules using different ellipsis.base (6)
  ;; 		  )
  (include "iterators-impl.scm"))
