; count the max airplane number on the fly according to the schedules
; [[1, 10], [2, 3], [5, 8], [4, 7]] => 3

(define-struct schedule (time side))

(define (build-schedule in)
  (local [(define schs
            (map (λ(e)
                   (list (make-schedule (car e) "start")
                         (make-schedule (cadr e) "end")))
                 in))]
    (foldl (λ (acc e) (append acc e))
           empty
           schs)))

(define (count in)
  (local [(define s-list (build-schedule in))
          (define sorted-list
            (sort s-list
                  (λ(x y) (< (schedule-time x)
                             (schedule-time y)))))
          (define (count-overlap s-list layer max)
            (if (empty? s-list)
                max
                (local [(define f (car s-list))
                        (define r (cdr s-list))
                        (define side (schedule-side f))]
                  (if (string=? side "start")
                      (count-overlap r (add1 layer) max)
                      (local [(define nmax (if (> layer max)
                                               layer
                                               max))]
                        (count-overlap r (sub1 layer) nmax))))))]
    (count-overlap sorted-list 0 0)))

(define s (list '(1 10) '(2 3) '(5 8) '(4 7)))
(count s)
