; -- chapter 1.1 code --

(define (square x) (* x x))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(define (f a)
 (sum-of-squares (+ a 1) (* a 2)))

(define (abs x)
  (cond ((> x 0) x)
    ((= x 0) 0)
    ((< x 0) (- x))))

(define (abs x)
  (if (< x 0)
    (- x)
    x))

; The contrast between function and procedure is a reflection of the general distinction between 
; describing properties of things and describing how to do things, or, as it is sometimes referred to, 
; the distinction between declarative knowledge and imperative knowledge. In mathematics we are usually 
; concerned with declarative (what is) descriptions, whereas in computer science we are usually concerned 
; with imperative (how to) descriptions.

; 1.1.7
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

; interestingly, you can use the symbols without previous definition. this is an error in clojure, 
; without the use of the 'declare' form

; nice use of tco, actually having tco is a real joy

(define (sqrt x)
  (sqrt-iter 1.0 x))

(define (run-sqrt)
  (print (sqrt 9))                     ; 3.00009155413138
  (print (sqrt (+ 100 37)))            ; 11.704699917758145
  (print (sqrt (+ (sqrt 2) (sqrt 3)))) ; 1.7739279023207892
  (print (square (sqrt 1000))))        ; 1000.000369924366
; (run-sqrt)

; 1.6
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
    (else else-clause)))
;(new-if (= 2 3) 0 5)
;(new-if (= 1 1) 0 5)

;(define (sqrt-iter guess x)
;  (new-if (good-enough? guess x)
;    guess
;    (sqrt-iter (improve guess x) x)))
; endless recursion, if this form is used

; my guess at this, is that the cond is evaluated at compilation, and the value
; never changes in the subsequent calculations, so the loop never executes the 
; alternative

; ellipses character
;(let-syntax
;  ((foo (syntax-rules ()
;    ((foo ?x ?y ... ?z)
;     (list ?x (list ?y ...) ?z)))))
;  (print (foo 1 2 3 4 5))
;  (print (foo 1 2      )))

; -- chapter 1.2 code --
(define (factorial n)
  (if (= 1 n) 1 (* n (factorial (- n 1)))))

(define (factorial n)
  (fact-iter 1 1 n))

(define (fact-iter product counter max-count)
  (if (> counter max-count)
    product
    (fact-iter (* counter product)
               (+ counter 1)
               max-count)))
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1) (A x (- y 1))))))

;(A 1 10) ; 1024
;(A 2 4)  ; 65536 
;(A 3 3)  ; 65536 

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

; (fib 10)

; In general, the number of steps required by a tree-recursive process will be proportional 
; to the number of nodes in the tree, while the space required will be proportional to the 
; maximum depth of the tree.

(define (fib n)
  (fib-iter 1 0 n))

(define (fib-iter a b count)
  (if (= count 0)
    b
    (fib-iter (+ a b) a (- count 1))))

(define (count-change amount)
  (cc amount 5))
(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination kinds-of-coins))
                     kinds-of-coins)))))
(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

(use test)

(define count (make-parameter 0))

(define (cube x) (* x x x))
(define (p x) (- (* 3 x) (* 4 (cube x))))
(define (sine angle)
  (count (+ (count) 1))
  (if (not (> (abs angle) 0.1))
    angle
    (p (sine (/ angle 3.0)))))


; SLIMV NOTES (http://kovisoft.bitbucket.org/tutorial.html)
; ------------------------
; REPL connection      ,c (or first evaluation)
;
; eval defun           ,d
; eval curr expression ,e
; eval region          ,r
; eval buffer          ,b
; eval interactively   ,v
; undefine function    ,u
;
; select restart line in buffer to continue in debug

(define (expt b n)
  (if (= n 0)
    1
    (* b (expt b (- n 1)))))

(define (expt b n)
  (expt-iter b n 1))

(define (expt-iter b counter product)
  (if (= counter 0)
    product
    (expt-iter b
               (- counter 1)
               (* b product)))) 

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (even? n)
  (= (remainder n 2) 0))

;(fast-expt 2 2048)
(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (square p) (square q))
                   (+ (* 2 (* p q))
                      (square q))
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))
(define (gcd a b)
  (if (= b 0)
    a
    (gcd b (remainder a b))))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
          (remainder (* base (expmod base (- exp 1) m))
                     m))))        

(define (fermat-test n)
  (define (try-it a)
      (= (expmod a n n) a))
        (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

;(fast-prime? 23 20)
