#lang racket

(define (fast-mult base n)
  (define (double n)
    (+ n n))
  (define (halve n)
    (/ n 2))
  (define (even? n)
    (= (remainder n 2) 0))
  (define (iter base counter product)
    (cond ((= counter 0) product)
          ((even? counter) (iter (double base)
                                 (halve counter)
                                 product))
          (else (iter base
                      (- counter 1)
                      (+ product base)))))
  (iter base n 0))
