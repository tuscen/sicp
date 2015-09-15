#lang racket

(define (square x) (* x x))

(define (largest x y)
  (if (> x y) x y))

(define (sum-of-squares-of-two-largest a b c)
  (+ (square (largest a b))
     (square (largest b c))))
