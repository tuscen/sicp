#lang racket

(require (prefix-in coll: data/collection))

(define (square n)
  (* n n))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (hash n (- (current-inexact-milliseconds) start-time))
      (hash)))

(define (timed-prime-test n)
  (start-prime-test n (current-inexact-milliseconds)))

(define (search-for-primes start prime-count)
  (define (iter acc number)
    (cond ((= (hash-count acc) prime-count) acc)
          (else (iter (coll:extend acc (timed-prime-test number)) (+ number 1)))))
  (iter (hash) (if (even? start) (+ start 1) start)))

(define (test start-numbers)
    (let ([timed-primes (stream->list (map (lambda (start) (search-for-primes start 3)) start-numbers))])
        timed-primes))

(test '(1000 10000 100000 1000000))
