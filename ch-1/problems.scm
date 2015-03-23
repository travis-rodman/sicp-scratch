; problems 1.21 - 1.28

; 1.21 : covered in section 1.2.6
(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))
(define (prime? n)
  (= n (smallest-divisor n)))

; solution (trivial) :
(smallest-divisor 199)   ; => 199   
(smallest-divisor 1999)  ; => 1999    
(smallest-divisor 19999) ; => 7        

; what looks like a prime may not be a prime, running this surprised me

; 1.22
(define (timed-prime-test n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (prime? n)
    (report-prime n (- (runtime) start-time))))
(define (report-prime n elapsed-time)
  (display n)
  (display " : ")
  (display elapsed-time)
  (newline))

;; solution
;(define (search-for-primes a b) 
;  (if (even? a) (display "error: the starting value must be odd")
;    (define (sfp-iter a b)
;      (if (<= a b) (timed-prime-test a))
;      (if (<= a b) (sfp-iter (+ 2 a) b)))))

;(define (search-for-primes a b) 
;  (define (sfp-iter a b)
;    (if (<= a b)
;      (begin 
;        (timed-prime-test a) 
;        (sfp-iter (+ 2 a) b))))
;  (if (= (modulo a 2) 1) 
;    (sfp-iter a b) 
;    (sfp-iter (+ 1 a) b)))

(define (search-for-primes a b) 
  (letrec ((iter (lambda (a b)
                   (if (<= a b) (timed-prime-test a))
                   (if (<= a b) (iter (+ 2 a) b)))))
    (if (odd? a) 
      (iter a b) 
      (iter (+ 1 a) b))))

(search-for-primes 2 3)
(search-for-primes 3 4)
(search-for-primes 4 20)
(search-for-primes 5 20)

; essentially zero return time for all of these values
(search-for-primes 1000 1019)
(search-for-primes 10000 10037)
(search-for-primes 100000 100043)
(search-for-primes 1000000 1000037)

; larger values to capture some times...
(search-for-primes 1000000000 1000000021)
(search-for-primes 10000000000 10000000061)
(search-for-primes 100000000000 100000000057)
(search-for-primes 1000000000000 1000000000063)

;1000000007 : .05999999999999994
;1000000009 : .06000000000000005
;1000000021 : .04999999999999993

;10000000019 : .16000000000000014
;10000000033 : .16999999999999993
;10000000061 : .16999999999999993

;100000000003 : .52
;100000000019 : .5199999999999998
;100000000057 : .52

;1000000000039 : 1.54
;1000000000061 : 1.5099999999999998
;1000000000063 : 1.5

(define (mul-sqrt-10 x) (* (sqrt 10) x))
(map mul-sqrt-10 '(.05999999999999994 .06000000000000005 .04999999999999993))
(map mul-sqrt-10 '(0.16000000000000014 0.16999999999999993 0.16999999999999993))
(map mul-sqrt-10 '(0.52 0.5199999999999998 0.52))                               
(map mul-sqrt-10 '(1.54 1.5099999999999998 1.5))                                

; I learned a lot about display, printf not existing, define not being able to be nested (first attempt)
; and the purpose of let, let* and letrec
; I also refactored their code (line 22) to move all the output to #'report-prime
; I like the letrec version of the function best.

; the (sqrt 10) relationship seems to hold approximately 

; 1.23
(define (next n) (if (= n 2) 3 (+ 2 n)))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(search-for-primes 1000000000 1000000021)
(search-for-primes 10000000000 10000000061)
(search-for-primes 100000000000 100000000057)
(search-for-primes 1000000000000 1000000000063)

;(search-for-primes 1000000000 1000000021)
;1000000007    : 0.04000000000000003
;1000000009    : 0.02999999999999936
;1000000021    : 0.02999999999999936
;(search-for-primes 10000000000 10000000061)
;10000000019   : 0.11000000000000121
;10000000033   : 0.08999999999999986
;10000000061   : 0.08999999999999986
;(search-for-primes 100000000000 100000000057)
;100000000003  : 0.29999999999999893
;100000000019  : 0.28999999999999915
;100000000057  : 0.29999999999999893
;(search-for-primes 1000000000000 1000000000063)
;1000000000039 : 0.9299999999999997
;1000000000061 : 0.9399999999999995
;1000000000063 : 0.9299999999999997

; yes, it about halves the time

; 1.24
(define (square x) (* x x))
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
(define (start-prime-test n start-time)
  (if (fast-prime? n 10)
    (report-prime n (- (runtime) start-time))))

(search-for-primes 1000000000 1000000021)
(search-for-primes 10000000000 10000000061)
(search-for-primes 100000000000 100000000057)
(search-for-primes 1000000000000 1000000000063)
(search-for-primes 1728 1730)

;(search-for-primes 1000000000 1000000021)
;1000000007 : 0.
;1000000009 : 0.
;1000000021 : 0.
;(search-for-primes 10000000000 10000000061)
;10000000019 : 0.
;10000000033 : 0.
;10000000061 : 0.
;(search-for-primes 100000000000 100000000057)
;100000000003 : 0.
;100000000019 : 9.999999999999787e-3
;100000000057 : 0.
;(search-for-primes 1000000000000 1000000000063)
;1000000000039 : 9.999999999999787e-3
;1000000000061 : 0.
;1000000000063 : 0.

; the search time is amazingly reduced

; 1.25
; the exptmod version creates large intermediate numbers, which need to be specially handled in memory, which makes it 
; very resource-intensive and slow, sub optimal compared to the fast-prime? calculations

; 1.26
; he is using direct multiplication, so the computation tree grows exponentially (it is branching) it offsets the logrithmic advantage
; of the optimized algorith, producing a linear-time calculation.

; 1.27
; illustrating the failure of the fermat test on the carmichael numbers
(search-for-primes 5  7)
(search-for-primes 561  562)
(search-for-primes 1728 1730)
(search-for-primes 1105 1106)
(search-for-primes 1729 1730)
(search-for-primes 2465 2466)
(search-for-primes 2821 2822)
(search-for-primes 6601 6602)

; 1.28
; congruence: http://www.math.nyu.edu/faculty/hausner/congruence.pdf
;(define (expmod base exp m)
;  (cond ((= exp 0) 1)
;        ((even? exp)
;         (remainder (square (expmod base (/ exp 2) m))
;                    m))
;        (else
;          (remainder (* base (expmod base (- exp 1) m))
;                     m))))        
;(define (fermat-test n)
;  (define (try-it a)
;      (= (expmod a n n) a))
;        (try-it (+ 1 (random (- n 1)))))

(define (expmod base x m)
  (letrec ((rmdr (lambda (x m) 
                   (if (and (not (or (= x 1) (= x (- m 1)))) 
                            (= (remainder (* x x) m) 1))
                     0 
                     (remainder (* x x) m)))))
    (cond ((= x 0)   1)
          ((even? x) (rmdr (expmod base (/ x 2) m) m))
          (else      (remainder (* base (expmod base (- x 1) m)) m)))))

; miller-rabin test
(define (mr-test n)
  (letrec ((try-it (lambda (a) (= (expmod a (- n 1) n) 1))))
    (try-it (+ (random-integer (- n 2)) 2))))

(mr-test 5)
(mr-test 561)
(mr-test 1105)
(mr-test 1729)
(mr-test 2465)
(mr-test 2821)
(mr-test 6601)
(mr-test (- (expt 2 89) 1))
