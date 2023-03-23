#lang racket/base
(require (for-syntax racket/base racket/match) racket/list racket/runtime-path)

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

(define-syntax (#%pslc-datum stx)
  (syntax-case stx ()
    ((_ . d)
     (let/cc ret
       (let ((datum (syntax->datum #'d))
             (orig (lambda (s) (datum->syntax #'stx (cons '#%datum s)))))
         (datum->syntax
          #'stx
          (match datum
            ((list 'quote (list expr 'for var 'in collection))
             (list 'map (list 'lambda (list var) expr) collection))
            ((list 'quote (list expr 'for var 'in collection 'if test token ...))
             (define conds (let loop ((token token) (state #f) (result null))
                             (cond ((null? token) (cond ((not state) (reverse result))
                                                        (else (ret (orig #'d)))))
                                   (else (loop (cdr token)
                                               (cond ((and (eq? (car token) 'and) (not state)) 'and)
                                                     ((and (not (eq? (car token) 'and))
                                                           state)
                                                      #f)
                                                     (else (ret (orig #'d))))
                                               (if (eq? (car token) 'and)
                                                   result
                                                   (cons (car token) result)))))))
             (list 'filter-map
                   (list 'lambda
                         (list var)
                         (list 'if (append (list 'and test) conds) expr #f))
                   collection))
            (else (ret (orig #'d))))))))))

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
    (check-equal? (#%pslc-datum . '[v for v in (list 1 2 3)])
                  (list 1 2 3))
    (check-equal? (#%pslc-datum . '[v for v in (list 1 2 3) if (odd? v)])
                  (list 1 3))
    (check-equal? (#%pslc-datum . '[v for v in (list 1 2 3) if (odd? v) and (zero? (sub1 v))])
                  (list 1))
    (check-eq? (#%pslc-datum . a)
               'a))
  (test-case
      "read-syntax"
    (check-match (syntax->datum (read-syntax #f (open-input-string "(define a ''[])")))
                 (list 'module modname (list 'file (regexp "^.*main\\.rkt$"))
                       (list 'define 'a '''[])))))
  
