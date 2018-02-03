#lang racket
(require racket/list)

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


;another way to do 

(define (add-each elem l)
  (foldl (λ (e acc)
             (cons (cons elem e) acc))
         empty
         l))

(define (combinate l)
  (if (empty? l)
      (list empty)
      (local [(define f (car l))
              (define r (cdr l))
              (define result (combinate r))]
        (append result (add-each f result)))))

(define (swap i j l)
  (local [(define i-elem (list-ref l i))
          (define j-elem (list-ref l j))]
    (list-set (list-set l j i-elem)
              i
              j-elem)))

(define (permute l)
  (if (empty? l)
      (list empty)
      (local [(define len (length l))
              (define (iter i result)
                (if (= i len)
                    result
                    (local [(define swapped (swap 0 i l))
                            (define f (car swapped))
                            (define r (cdr swapped))
                            (define i-result (add-each f (permute r)))]
                      (iter (add1 i) (append result i-result)))))]
        (iter 0 empty))))

(define (permute-all l)
  (local [(define len (length l))
          (define (iter i)
            (if (= i len)
                (list empty)
                (local [(define swapped (swap 0 i l))
                        (define f (car swapped))
                        (define r (cdr swapped))]
                  (append (add-each f (permute-all r))
                          (iter (add1 i))))))]
    (iter 0)))


(permute (list 1 2 3))
