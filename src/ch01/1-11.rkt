#lang racket

(define (f-recursive n)
  (cond ((< n 3) n)
        (else (+ (f-recursive (- n 1))
                 (f-recursive (- n 2))
                 (f-recursive (- n 3))))))
(define (f-iterative n)
  (define (iter a b c count)
    (cond ((= count 0) c)
          (else (iter (+ a b c)
                      a
                      b
                      (- count 1)))))
  (iter 2 1 0 n))
