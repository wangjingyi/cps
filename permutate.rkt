;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname permutate) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))

(define (shift l)
  (if (empty? l)
      empty
      (append (cdr l)
              (list (car l)))))

;;(shift (list 1 2 3))


;; l is a list, n is the length of the list, l
(define (rotate l n)
  (cond [(= n 1) (list l)]
        [else (cons l
                    (rotate (shift l)
                            (sub1 n)))]))

;;(rotate (list 1 2 3 4) (length (list 1 2 3 4)))

(define (rotate-all l)
  (if (empty? l)
      empty
      (append (rotate (car l)
                      (length (car l)))
              (rotate-all (cdr l)))))

;;(rotate-all (list (list 1 2 3) (list 4 5 6)))

(define (append-all elem l)
  (if (empty? l)
      empty
      (cons (cons elem (car l))
            (append-all elem (cdr l)))))

;; permutate cdr first, then append the car to the permutated cdr (list of list)
;; and then rotate each list

(define (permutate l)
  (cond [(empty? l) (list empty)]
        [else
         (rotate-all
          (append-all(car l)
                     (permutate (cdr l))))]))

(permutate (list "a" "b" "c" "d"))



          
