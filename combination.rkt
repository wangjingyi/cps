;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname combination) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

; l is a list of lists
(define (insert-all elem l)
  (if (empty? l)
      empty
      (cons
       (cons elem (car l))
       (insert-all elem (cdr l)))))

;(insert-all 1 (list (list 2 3 4) (list 5 6 7)))

; l is a list
(define (combination l)
  (if (empty? l)
      (list empty)
      (append (insert-all (car l)
                          (combination (cdr l)))
              (combination (cdr l)))))


(define (comb l)
  (if (empty? l)
      (list empty)
      (let [(existing (comb (cdr l)))]
        (append existing
                (insert-all (car l)
                            existing)))))

(combination (list 1 2 3 4 5))