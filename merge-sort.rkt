#lang racket

(define (split-acc half1 half2 l)
  (cond [(empty? l) (list half1 half2)]
        [else (split-acc half2
                         (cons (car l)
                                     half1)
                         (cdr l))]))

(define (split l)
  (split-acc empty empty l))


(define (merge l1 l2)
  (cond [(empty? l1) l2]
        [(empty? l2) l1]
        [else (local ([define e1 (car l1)]
                      [define e2 (car l2)]
                      [define rl1 (cdr l1)]
                      [define rl2 (cdr l2)])
                (if (> e1 e2)
                    (cons e2 (merge l1 rl2))
                    (cons e1 (merge rl1 l2))))]))

(define (merge-sort l)
  (local ([define ll (split l)]
          [define first (car ll)]
          [define second (cadr ll)]
          [define first-len (length first)]
          [define second-len (length second)])
    (merge (if (<= first-len 1)
               first
               (merge-sort first))
           (if (<= second-len 1)
               second
               (merge-sort second)))))

(merge-sort (list 8 80 100 20 150 60))
        
                         