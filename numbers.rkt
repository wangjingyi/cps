;using '*' to represent number from 0 to infinite
;implementing ADD, SUB and TIMES
;TIMES is using cps to implement

(define unit "*")
(define N0  empty)
(define N1  (cons unit N0))
(define N2  (cons unit N1))
(define N3  (cons unit N2))
(define N4  (cons unit N3))
(define N5  (cons unit N4))
(define N6  (cons unit N5))
(define N7  (cons unit N6))
(define N8  (cons unit N7))
(define N9  (cons unit N8))
(define N10 (cons unit N9))

(define (ZERO? n) (empty? n))
(define (ONE?  n) (empty? (cdr n)))
(define (ADD1  n) (cons unit n))
(define (SUB1  n) (cdr n))

(define ZERO empty)
(define ONE (cons unit empty))

(define (ADD a b)
  (cond [(ZERO? a) b]
        [else (ADD (SUB1 a)
                   (ADD1 b))]))

(define (SUB a b)
  (cond [(ZERO? b) a]
        [else (SUB (SUB1 a)
                   (SUB1 b))]))

(define (TIMES a b f)
  (cond [(or (ZERO? a)
             (ZERO? b)) ZERO]
        [(ONE? b) (f a)]
        [else
         (TIMES a
                (SUB1 b)
                (lambda (answer)
                  (f (ADD answer a))))]))

(check-expect (TIMES N2 N5 identity) N10)
(check-expect (TIMES N0 N2 identity) N0)
(check-expect (TIMES N1 N10 identity) N10)
(check-expect (TIMES N3 N3 identity) N9)
