(require racket/list)

(define (swap list i j)
  (local [(define a (list-ref list i))
          (define b (list-ref list j))]
    (list-set (list-set list i b) j a)))


(define (sink list i end-idx)
  (local [(define (left i)
            (add1 (* 2 i)))
          (define (right i)
            (add1 (left i)))
          (define l-idx (left i))
          (define r-idx (right i))]
    (cond [(> l-idx end-idx) list]
          [(> r-idx end-idx) (sink-left list i l-idx end-idx)]
          [else (sink-both list i l-idx r-idx end-idx)])))

(define (sink-left list i l-idx end-idx)
  (local [(define current (list-ref list i))
          (define l-child (list-ref list l-idx))]
    (if (> l-child current)
        (sink (swap list l-idx i) l-idx end-idx)
        list)))

(define (sink-both list i l-idx r-idx end-idx)
  (local [(define current (list-ref list i))
          (define l-child (list-ref list l-idx))
          (define r-child (list-ref list r-idx))
          (define j (if (> l-child r-child)
                        l-idx
                        r-idx))]
    (if (> (list-ref list j) current)
        (sink (swap list j i) j end-idx)
        list)))

(define (heapify list)
  (local [(define end-idx (sub1 (length list)))
          (define start-idx (quotient (sub1 end-idx) 2))
          (define (iter list idx)
            (if (>= idx 0)
                (local [(define n-list
                          (sink list idx end-idx))]
                  (iter n-list (sub1 idx)))
                list))]
    (iter list start-idx)))


(define (heap-sort list)
  (local [(define last-idx (sub1 (length list)))
          (define max-heap (heapify list))
          (define (iter heap i)
            (if (= i 0)
                heap
                (local [(define exch (swap heap 0 i))
                        (define n-end-idx (sub1 i))
                        (define n-heap (sink exch 0 n-end-idx))]
                  (iter n-heap (sub1 i)))))]
    (iter max-heap last-idx)))

(heap-sort (list 5 1 66 80 30 100 120 111))


                      
                 
        

               
          
                              
  


              
