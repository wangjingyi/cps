#lang racket

(define-struct coor (x y))
(define N 1000000)
(define R 0.5)

(define (make-point)
  (make-coor (- (random) R)
             (- (random) R)))


(define (compute-pi hits n)
  (cond [(= n N) (/ hits n)]
        [(hits?) (compute-pi (add1 hits)
                             (add1 n))]
        [else (compute-pi hits
                          (add1 n))]))

(define (hits?)
  (local [(define pt (make-point))]
    (< (+ (sqr (coor-x pt))
          (sqr (coor-y pt)))
       (sqr R))))


(define (pi)
  (* (compute-pi 0 0)
     4))

(pi)