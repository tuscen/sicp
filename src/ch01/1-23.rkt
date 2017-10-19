#lang racket

(define (square n)
    (* n n))

(define (divides? a b)
    (= (remainder b a) 0))

(define (find-divisor n test-divisor)
    (let ([next (lambda (n)
                    (if (even? n) (+ n 1) (+ n 2)))])
        (cond ((> (square test-divisor) n) n)
              ((divides? test-divisor n) test-divisor)
              (else (find-divisor n (next test-divisor))))))

(define (smallest-divisor n)
    (find-divisor n 2))
