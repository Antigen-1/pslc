#lang reader "../main.rkt"
(require rackunit)

(test-case
    "overall"
  (check-equal? '[v for v in (range 0 1000) if (zero? (remainder v 7))]
                (let loop ((s 0) (r null))
                  (define v (* s 7))
                  (cond ((>= v 1000) (reverse r))
                        (else (loop (add1 s) (cons v r)))))))
