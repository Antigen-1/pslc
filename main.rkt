#lang racket/base
(require (for-syntax racket/base syntax/parse))

;; Notice
;; To install (from within the package directory):
;;   $ raco pkg install
;; To install (once uploaded to pkgs.racket-lang.org):
;;   $ raco pkg install <<name>>
;; To uninstall:
;;   $ raco pkg remove <<name>>
;; To view documentation:
;;   $ raco docs <<name>>
;;
;; For your convenience, we have included LICENSE-MIT and LICENSE-APACHE files.
;; If you would prefer to use a different license, replace those files with the
;; desired license.
;;
;; Some users like to add a `private/` directory, place auxiliary files there,
;; and require them in `main.rkt`.
;;
;; See the current version of the racket style guide here:
;; http://docs.racket-lang.org/style/index.html

;; Code here

;;Literals
(define-syntax in #t)

;;Syntax classes
(begin-for-syntax
  (define-syntax-class iterator
    #:description "iterator"
    #:literals (for if in)
    (pattern (body:expr for var:id in sequence:expr) #:with cond #'#t)
    (pattern (body:expr for var:id in sequence:expr if cond:expr))))

;;I'll not strictly abide by the python syntax due to its complicacy
(define-syntax (#%pslc-datum stx)
  (syntax-parse stx
    ((_ . iter:iterator)
     #'(reverse
        (for/fold ((ac null)) ((iter.var iter.sequence))
          (if iter.cond (cons iter.body ac) ac))))
    ((_ . other) #''other)))

;;Alias
(define-syntax-rule (pslc-quote o)
  (#%pslc-datum . o))

(provide (rename-out (#%pslc-datum #%datum) (pslc-quote quote))
         in
         (except-out (all-from-out racket/base) #%datum quote))

(module* test (submod "..")
  (require rackunit)
  
  (check-equal? '[(add1 v) for v in (list 1 2 3)] (list 2 3 4))
  (check-equal? '[v for v in (list 1 2 3) if (odd? v)] (list 1 3))
  (check-equal? '[v for v in (list 1 2 3) if (and (odd? v) (zero? (sub1 v)))] (list 1))
  (check-eq? 'a (string->symbol "a")))
