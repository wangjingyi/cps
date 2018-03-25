#lang racket

;find ways to calculate a target from elements of specified array.
;the number of ways to reach a target using only + and - operators.


(define (ways lst target)
  (cond [(= target 0) 1]
        [(empty? lst) 0]
        [else (local [(define f (car lst))
                      (define r (cdr lst))]
                (+ (ways r (- target f))
                   (ways r (+ target f))
                   (ways r target)))]))

(ways (list 5 3 -6 2) 6)
       
       