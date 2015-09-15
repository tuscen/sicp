#lang racket

(define (pascal-triangle-element i j)
  (cond ((= j 1) 1)
        ((= i j) 1)
        ((< i j) '())
        (else (+ (pascal-triangle-element (- i 1) (- j 1))
                 (pascal-triangle-element (- i 1) j)))))
