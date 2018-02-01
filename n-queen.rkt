(require racket/list)

(define (make-board n)
  (build-list n (λ(_) false)))

(define (attack? board row col)
  (local [(define p-rows (range row))]
    (ormap (λ (p-row)
             (local [(define elem (list-ref board p-row))
                     (define delta (- row p-row))]
               (or (eq? elem col)
                   (eq? elem (+ col delta))
                   (eq? elem (- col delta)))))
           p-rows)))
                       

(define (queen n)
  (local [(define results empty)
          (define (n-queen board r)
            (local [(define n (length board))
                    (define cols (range n))
                    (define (new-board c)
                      (list-set board r c))]       
              (if (= r n)
                  (set! results (cons board results))
                  (for-each (λ(col)
                              (when (not (attack? board
                                                  r
                                                  col))
                                (n-queen (new-board col)
                                         (add1 r))))
                  cols))))]
    (begin (n-queen (make-board n)
                    0)
           results)))

(define (convert l)
  (local [(define n (length l))]
    (map (λ (pos)
           (append (make-list pos 0)
                   (list 1)
                   (make-list (- n pos 1) 0)))
         l)))

(let ([pos (queen 4)])
  (map convert pos))
