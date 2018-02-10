; two player who will take turn to take a total N tokens. there is a limit that
; the player can take each time (say can take 1, 2 or 3 each time, the limit is 3)
; whoever will lose when there is 1 token left for his turn.

(require racket)

(define (is-bad? total limit)
  (local [(define n (if (< total limit)
                        total
                        limit))
          (define steps (range 1 (add1 n) 1))]
    (cond [(= total 1) #t]
          [else (not (ormap (λ(e)   ;after my turn, as long as there is one bad for oppose, then i am good.
                              (is-bad? (- total e) ;now it is oppose turn after subtract my turn.
                                        limit))
                            steps))])))

(define (next-step total limit)
  (local [(define (iter total taken limit)
            (cond [(> taken limit) 1] ;could not find good one, pick the smallest
                  [(is-bad? (- total taken) limit) taken] ;is-bad? for oppose
                  [else (iter total (add1 taken) limit)]))]
    (iter total 1 limit)))

(define (read-in lower upper)
  (local [(define n (read))]
    (if (and (<= n upper)
             (>= n lower))
        n
        (begin (printf "number should be between ~a and ~a.~n=>" lower upper)
               (read-in lower upper)))))

(define (game-display total limit)
  (printf "Start a game; total tokens are ~a; each time you can take from 1 to ~a tokens.~n" total limit))

(define (play total limit)
  (if (= total 1)
      (display "You lose!")
      (begin (printf "You turn to take.~n")
             (local [(define your-step (read-in 1 limit))
                     (define my-total (- total your-step))]
               (cond [(= my-total 1) (display "You win!")]
                     [else (local [(define taken (next-step my-total limit))
                                   (define ntotal (- my-total taken))]
                             (begin (printf "You have taken ~a tokens; and computer has taken ~a tokens.~n" your-step taken)
                                    (printf "Now the left tokens are ~a.~n" ntotal)
                                    (play ntotal limit)))])))))

(define (start total limit)
  (begin (game-display total limit)
         (play total limit)))


(start 10 3)

             
