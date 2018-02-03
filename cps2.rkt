#lang racket
;rules for conversion to cps
; 1) add an extra argument k to each function
; 2) instead of returning a result in a function, pass the result to k
; 3) lift a nested function call out of its sub-expression by selecting a 
;    variable X to replace the function call, wrapping the expression with
;    (lambda(X) ...), and providing the result (lambda(X) _) as the third
;    argument to the function. For ex, covert (add1 (f x)) to 
;    (f x (lambda(v) (add1 v))). Applications of primitive operations need 
;    no lifting.

; example to change the following code;
(define (map f l)
  (if (null? l)
      '()
      (cons (f (car l))
            (map f (cdr l)))))

(define (map2 f l)
  (if (null? l)
      '()
      (cons (call/cc (λ(k) (k (f (car l)))))
            (call/cc (λ(k) (k (map2 f (cdr l))))))))
; NOTE: f should accept a continuation argument in addition to a list element.
; in general, a CPS conversion changes the meaning of "function" globally so
; that every function consumes a continuation.
; when we convert, we need to convert define function as well as all the
; function used in the define function. Here are "map" and "f", they both
; should take an continuation parameter.
; In general, we lift application expressions out of other applications,
; out of primitive operations, and out of the test position of if.
; we do not lift past lambda (including the implicit one with define) or
; past the branches of an if expression.

; 1). add a k argument to map2 and pass the result to k instead of returning
;     them directly.

; (define (map2 f l k)
;   (if (null? l)
;       (k '())
;       (k (cons (f (car l)) (map2 f (cdr l))))))

; 2). need lift (f (car l)) & (map f (cdr l)) because both need a continuation
;     parameter.
;
; (define (map2 f l k)
;   (if (null? l)
;       (k '())
;       (f (car l)
;          (λ(X) (k (cons X (map2 f (cdr l))))))
;
; (define (map2 f l k)
;   (if (null? l)
;       (k '())
;       (f (car l)
;          (λ(X) (map2 f (cdr l)
;                   (λ(Y) (k (cons X Y))))))))

; when call, as following
; (map2 (λ(e c) (c (add1 e))) (range 5) identity)
;
; (define (filter f l)
;   (if (null? l)
;       '()
;      (if (f (car l))  # this (f (car l)) should be lifted outside if
;          (cons (car l) (filter f (cdr l)))
;          (filter f (cdr l)))))
;
;

; (define (filter f l k)
;   (if (null? l)
;       (k '())
;      (if (f (car l))  # this (f (car l)) should be lifted outside if
;          (k (cons (car l) (filter f (cdr l))))
;          (k (filter f (cdr l))))))

; (define (filter f l k)
;   (if (null? l)
;       (k '())
;       (f (car l)
;          (λ(X)
;            (if X
;              (k (cons (car l) (filter f (cdr l)))
;              (k (filter f (cdr l))))))

; (define (filter f l k)
;   (if (null? l)
;       (k '())
;       (f (car l)
;          (λ(X)
;            (if X
;              (filter f (cdr l) (λ(Y)
;                                  (k (cons (car l) Y)))
;              (filter f (cdr l) (λ(Y)
;                                  (k Y))))
;   

; 
(define (filter2 f l k)
  (if (null? l)
      (k '())
      (f (car l)
         (λ(X)
           (if X
             (filter2 f (cdr l)  ; lift not past lambda and if branch
                     (lambda (Y)
                       (k (cons (car l) Y))))
             (filter2 f (cdr l)
                     (lambda (Y)
                       (k Y))))))))

;when call,

(filter2 (λ(e c)
           (c (number? e)))
         (list "one" 1 2 "two" 3 4) identity)

;convert the next example to cps.
(define (sum-tree stop? t)
  (if (stop? t)
      t
      (+ (sum-tree stop? (car t))
         (sum-tree stop? (cdr t)))))

(sum-tree (λ(x) (number? x)) (cons (cons 1 2) (cons 3 (cons 4 5))))

;1). add k and pass all the return/result to continuation k.
;(define (sum-tree stop? t k)
;  (if (stop? t)
;      (k t)
;      (k (+ (sum-tree stop? (car t))
;            (sum-tree stop? (cdr t)))))
; let's make stop? as cps as well.

;(define (sum-tree stop? t k)
;  (stop? t (λ(flag))
;         (if flag
;             (k t)
;             (sum-tree stop?
;                       (car t)
;                       (λ(left)
;                         (k (+ left
;                               (sum-tree stop? (cdr t)))))))))


(define (sum-tree2 stop? t k)
  (stop? t (λ(flag)
         (if flag
             (k t)
             (sum-tree2 stop?
                       (car t)
                       (λ(left)
                         (sum-tree2 stop?
                                   (cdr t)
                                   (λ(right)
                                     (k (+ left right))))))))))
                               
;when called, using

(sum-tree2 (λ(e c) (c (number? e)))
          (cons (cons 1 2) (cons 3 (cons 4 5)))
          identity)

; now let's understand call/cc. the stand interface of call/cc is
; call/cc (λ(k) ...). where k is so-called continuation.
; basically call/cc will look at the surrounding expression and
; automatically compute k by capture the surrounding expression.
; scheme/drracket allows the continuation of any expression to be captured with
; procedure call/cc. call/cc must be pass a procedure p (p = λ(k)(....)) of one
; argument. call/cc constructs a concrete representation of the current continuation (represented by k)
; and pass it to p. NOTE: if  p (λ(k)(...)) returns without invoking k, the value returned by the
; procedure become the value of application of call/cc.
; so, 1) if invoking k in procedure, p, (λ(k) (k ...)), then the value returning by invoking k, which is the value of
;        the continuation of the call/cc application,
;        will become the value of entire application of call/cc.
;     2) if no invoking k in p, say, (λ(k) (* 3 4)), then the value returned by procedure "p" becomes the value
;        of application of call/cc.

(* 3  (call/cc (λ(k) 20))) ; => 60, application of call/cc is 20 and the entire expression is 30.
 
; in any expression, we can replace any subexpressin with call/cc apply to 
; that subexpression;
; for ex, if we want to use continuation to replace exp2, it should be
; (+ exp1 (* exp2 exp3))
; (+ exp1 (* (call/cc (λ(k) (k exp2))) exp3)
; this will be translated into
; k = ((λ(v) (+ exp1 (* v exp3)) exp2).
; if we pass some value to k, then the entire application of call/cc will be replaced
; by the above (k) lambda (parameter v) and the argument for v will be the argument passed to the lambda k.

; few examples: using expression (+ (* 3 4) 5) for all examples.
; 1)
; the continuation of the entire expression is => k: (λ(v) v)
; so, if we do (call/cc (λ(k) (+ (* 3 4) 5)))), then k is the identity function (λ(v) v)

; 2) the same expressioon, the continuation of (* 3 4) is
; (* 3 4)  =>  k: (λ(v) (+ v 5))
; which means, if we do 
; (+ (call/cc 
;	(λ(k)
;	  (k (* 3 4))))
;    5)
; then k is (λ(v) (+ v 5))

; 3) the same expression, the continuation of 3 is
; 3 => k: (λ(v) (+ (* v 4) 5))
; if we do
; (+ (* (call/cc 
;	    (λ(k) 
;		(k 3))) 
;        4)
;    5)
; then k is (λ(v) (+ (* v 4) 5))
;
; apply the same rule for 4 and 5, we can see the continuation would be:
; 4		k: (λ(v) (+ (* 3 v) 5))
; 5		k: (λ(v) (+ (* 3 4) v))

; now let's see the following example

(let [(x (call/cc (λ(k) k)))]
  (x (λ (ignore) "hi")))

; the compiler will generate the following continuation k
;
;(λ(v)
;  (let [(x v)]
;    (x (λ(ignore) "hi"))))
; because there is no value passed to k, so the returned value of
; procedure p (λ(k) k) will become the value of call/cc application, the "k", which
; is the above lambda, which is bound to x, then x basically is the continuation k.
; when (λ (ignore) "hi") is passed to x (as parameter v), then x is bound to it by "let" expression.
; the (λ(ignore) "hi") is passed to itself basically. so it will generate "hi".

; another example
(((call/cc (λ(k) k)) (λ (x) x)) "HEY!")

; here the continuation k is:
;(λ(v)
;  ((v (λ (x) x)) "HEY!") ........(a)
; again, inside procedure p =(λ(k) k), again, nothing is passed to k, then k become the value of
; call/cc, which is
; ((k (λ(x) x)) "HEY!"), where k is above lambda (a).
; then we pass (λ (x) x) as parameter v in (a). then we pass (λ (x) x) to itself, and the return
; value it itself as well. finally we pass "HEY!" to (λ (x) x), we got "HEY!" printed.