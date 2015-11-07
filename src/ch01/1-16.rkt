#lang racket

(define (expt base n)
  (define (square n)
    (* n n))
  (define (even? n)
    (= (remainder n 2) 0))
  (define (iter base counter product)
    (cond ((= counter 0) product)
          ((even? counter) (iter (square base)
                                 (/ counter 2)
                                 product))
          (else (iter base
                      (- counter 1)
                      (* product base)))))
  (iter base n 1))
