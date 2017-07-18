; Hanoi Tower
; moving discs from A to B via C

(define (moving name from to )
  (string-append "moving "
                 name
                 " from "
                 from
                 " to "
                 to))


(define (move-disc A B C n)
  (cond [(= n 0) (display "\n")]
        [else
         (begin
           (move-disc A C B (sub1 n))
           (display (moving (number->string n)
                            A
                            B))
           (move-disc C B A (sub1 n)))]))

                    
