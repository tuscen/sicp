#lang racket

(define (cbrt x)
  (define (average x y z)
    (/ (+ x y z) 3))
  (define (improve guess)
    (average guess
             guess
             (/ x (* guess guess))))
  (define (good-enough? prev-guess guess)
    (< (abs (- guess prev-guess))
       0.00000001))
  (define (iter prev-guess guess)
    (if (good-enough? prev-guess guess)
        guess
        (iter guess
              (improve guess))))
  (iter 0 1.0))
