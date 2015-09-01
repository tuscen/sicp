#lang racket

(define (square x) (* x x))
(define (largest x y)
  (if (> x y) x y))

(define (foo a b c)
  (+ (square (largest a b))
     (square (largest b c))))
