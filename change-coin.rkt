; how many ways to change for $1 with 1, 5, 10 25, 50 cents
(define (change-coin amount)
  (cc amount 5))


(define (cc amount kinds-of-coin)
  (cond [(= amount 0) 1]
        [(or (< amount 0) (= kinds-of-coin 0)) 0]
        [else (+ (cc (- amount (last-coin kinds-of-coin)) kinds-of-coin)
                 (cc amount (sub1 kinds-of-coin)))]))

(define (last-coin kinds-of-coin)
  (cond [(= kinds-of-coin 1) 1]
        [(= kinds-of-coin 2) 5]
        [(= kinds-of-coin 3) 10]
        [(= kinds-of-coin 4) 25]
        [else 50]))


(change-coin 100)


(define (walk total-steps largest-step)
  (cond [(or (< total-steps 0) (= largest-step 0)) 0]
        [(or (= total-steps 0) (= largest-step 1)) 1]
        [(< total-steps largest-step) (walk total-steps (sub1 largest-step))]
        [else (+ (walk (sub1 total-steps) largest-step)
                 (walk (- total-steps 2) largest-step))]))


(walk 4 2)
