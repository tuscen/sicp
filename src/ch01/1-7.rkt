#lang racket

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))
(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x))
     0.001))

(define (square x)
  (* x x))

(define (sqrt x)
  (sqrt-iter 1.0 x))

; Exercise 1.7 solution
(define (better-good-enough? prev-guess guess)
  (< (abs (- guess prev-guess))
     0.00001))

(define (better-sqrt-iter prev-guess guess x)
  (if (better-good-enough? prev-guess guess)
      guess
      (better-sqrt-iter guess
                        (improve guess x)
                        x)))
(define (better-sqrt x)
  (better-sqrt-iter 0 1.0 x))
