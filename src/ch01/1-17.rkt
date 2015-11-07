#lang racket

(define (double n)
    (+ n n))
(define (halve n)
    (/ n 2))
(define (even? n)
    (= (remainder n 2) 0))

(define (fast-mult a b)
  (cond ((= b 0) 0)
        ((even? b) (fast-mult (double a)
                              (halve b)))
        (else (+ a
                 (fast-mult a (- b 1))))))
