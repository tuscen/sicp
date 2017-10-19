#lang racket

(require (prefix-in coll: data/collection))

(define (expmod base exp m)
    (cond ((= exp 0) 1)
          ((even? exp) (remainder (square (expmod base (/ exp 2) m))
                                  m))
          (else (remainder (* base (expmod base (- exp 1) m))
                           m))))

(define (fermat-test n)
    (define (try-it a)
        (= (expmod a n n) a))
    (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
    (cond ((= times 0) true)
          ((fermat-test n) (fast-prime? n (- times 1)))
          (else false)))

(define (square n)
  (* n n))

(define (start-prime-test n start-time)
  (if (fast-prime? n 1)
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
