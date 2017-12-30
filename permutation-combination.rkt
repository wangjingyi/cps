#lang racket

(define (permutation sofar remaining)
  (if (= (length remaining) 0)
      (list sofar)
      (permutation-all sofar remaining 0)))

(define (permutation-all sofar remaining i)
  (if (= i (length remaining))
      empty
      (local [(define cur-element (list-ref remaining i))
              (define nremaining (append (take remaining i)
                                         (drop remaining (add1 i))))]
        (append (permutation (append sofar
                                     (list cur-element))
                             nremaining)
                (permutation-all sofar remaining (add1 i))))))

(define (combination sofar remaining)
  (if (= (length remaining) 0)
      (list sofar)
      (local [(define nremaining (drop remaining 1))
              (define cur-element (car remaining))]
        (append (combination (append sofar
                                     (list cur-element))
                             nremaining)
                (combination sofar
                             nremaining)))))


(permutation empty (list 'a 'b 'c))
(combination empty (list 1 2 3))

