#lang racket/base
(require (for-syntax racket/base) racket/list racket/runtime-path)

(define-runtime-path lib "main.rkt")

(module+ test
  (require rackunit))

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

;;I'll not strictly abide by the python syntax due to its complicacy
(define-syntax (#%pslc-datum stx)
  (syntax-case stx (for in if and)
    ((_ . (expr for var in collection))
     #'(map (lambda (var) expr) collection))
    ((_ .  (expr for var in collection if test))
     #'(filter-map (lambda (var) (if test expr #f)) collection))
    ((_ . (expr for var in collection if and test other ...))
     #'(filter-map (lambda (var) (if (and test other ...) expr #f)) collection))
    ((_ . d) (datum->syntax #'stx (cons '#%datum #'d)))))

(define-syntax-rule (pslc-quote o)
  (#%pslc-datum . o))

(define-syntax-rule (#%pslc-module-begin body ...)
  (#%module-begin
   body
   ...))

(define (read-syntax src port)
  (let loop ((r null))
    (define e (read port))
    (cond ((eof-object? e)
           (datum->syntax #f (append (list 'module (gensym 'pslc) (list 'file (path->string (path->complete-path lib))))
                                     (reverse r))))
          (else (loop (cons e r))))))

(provide read-syntax
         (rename-out (#%pslc-datum #%datum)
                     (#%pslc-module-begin #%module-begin)
                     (pslc-quote quote))
         (except-out (all-from-out racket/base) #%module-begin #%datum quote))

(module+ test
  (test-case
      "#%pslc-datum"
    (check-equal? (#%pslc-datum . [v for v in (list 1 2 3)])
                  (list 1 2 3))
    (check-equal? (#%pslc-datum . [v for v in (list 1 2 3) if (odd? v)])
                  (list 1 3))
    (check-equal? (#%pslc-datum . [v for v in (list 1 2 3) if and (odd? v) (zero? (sub1 v))])
                  (list 1))
    (check-eq? (#%pslc-datum . a)
               'a))
  (test-case
      "read-syntax"
    (check-match (syntax->datum (read-syntax #f (open-input-string "(define a ''[])")))
                 (list 'module modname (list 'file (regexp "^.*main\\.rkt$"))
                       (list 'define 'a '''[])))))
  
