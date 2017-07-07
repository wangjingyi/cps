;few examples for CPS style programming in Dr Racket


(define (e-length lst cc)
  (cond [(empty? lst) (cc 0)]
        [else
         (e-length (cdr lst)
                   (lambda (acc)
                     (cc (add1 acc))))]))

(define (e-sum lst cc)
  (cond [(empty? lst) (cc 0)]
        [else (e-sum (cdr lst)
                     (lambda (agg)
                       (cc (+ (car lst)
                              agg))))]))



(define (e-map lst f cc)
  (cond [(empty? lst) (cc empty)]
        [else
         (e-map (cdr lst) f
                (lambda (acc)
                  (cc (cons (f (car lst))
                            acc))))]))


(define (e-reduce lst f cc)
  (cond [(empty? lst) (cc 0)]
        [else
         (e-reduce (cdr lst) f
                   (lambda (acc)
                     (cc (f (car lst) acc))))]))


(define (fib i cc)
  (cond [(<= i 1) (cc i)]
        [else
         (fib (sub1 i)
              (lambda (v1)
                (fib (- i 2)
                     (lambda (v2)
                       (cc (+ v1 v2))))))]))


(define (fact n cc)
  (cond [(<= n 1) (cc 1)]
        [else
         (fact (sub1 n)
               (lambda (acc)
                 (cc (* n acc))))]))


(define (e-odd? n cc)
  (cond [(= n 0) (cc false)]
        [(= n 1) (cc true)]
        [else
         (e-odd? (- n 2)
                 (lambda (acc)
                   (cc acc)))]))

(define (e-remove elm lst cc)
  (cond [(empty? lst) (cc empty)]
        [(eq? elm (car lst)) (e-remove elm (cdr lst) cc)]
        [else
         (e-remove elm (cdr lst)
                   (lambda (acc)
                     (cc (cons (car lst) acc))))]))


(define (e-filter f lst cc)
  (cond [(empty? lst) (cc empty)]
        [(f (car lst)) (e-filter f (cdr lst) cc)]
        [else (e-filter f (cdr lst)
                        (lambda (acc)
                          (cc (cons (car lst) acc))))]))

; cp is a cps style function
(define (c-map cp lst cc)
  (cond [(empty? lst) (cc empty)]
        [else
         (cp (car lst)
             (lambda (elm)
               (c-map cp (cdr lst)
                      (lambda (sub-lst)
                        (cc (cons elm sub-lst))))))]))

(c-map (lambda (e ct)
         (ct (string->number e))) '("1" "2" "3") identity)



(define (c-map-r cp lst cc)
  (cond [(empty? lst) (cc empty)]
        [else
         (c-map-r cp (cdr lst)
                  (lambda (sub-lst)
                    (cp (car lst)
                        (lambda (elm)
                          (cc (cons elm sub-lst))))))]))

(c-map-r (lambda (e ct)
              (ct (string->number e))) '("1" "2" "3") identity)
                           


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; main() { Console.WriteLone(F(1) + 1); }
;
; int F(int n) { return G(n + 1) + 1; }
;
; int G(int n) { return n + 1; }
;
; the above program will be translated as the following
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (G n cc)
  (cc (add1 n)))

(define (F n cc)
  (G (add1 n)
     (lambda (x)
       (cc (add1 x)))))

(define (main)
  (F 1 (lambda (x)
         (print (add1 x)))))

(main)





