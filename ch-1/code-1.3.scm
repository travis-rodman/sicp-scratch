;----------------- 1.3.1 -----------------

(define (cube x) (* x x x))

(define (sum-integers a b)
  (if (> a b)
    0
    (+ a (sum-integers (+ a 1) b))))

(define (sum-cubes a b)
  (if (> a b)
    0
    (+ (cube a) (sum-cubes (+ a 1) b))))

;(sum-integers 9 20)
;(sum-cubes 9 20)

(define (pi-sum a b)
  (if (> a b)
    0
    (+ (/ 1.0 (* a (+ a 2))) (pi-sum (+ a 4) b))))

;(pi-sum 9 20)

(define (sum term a next b)
  (if (> a b)
    0
    (+ (term a)
       (sum term (next a) next b))))

(define (inc n) (+ n 1))
(define (sum-cubes a b)
  (sum cube a inc b))

;(sum-cubes 1 10)

(define (identity x) x)
(define (sum-integers a b)
  (sum identity a inc b))

(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

;(* 8 (pi-sum 1 1000))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

;(integral cube 0 1 0.01)
;.24998750000000042
;(integral cube 0 1 0.001)
;.249999875000001


;----------------- 1.3.2 -----------------

; let from a lambda definition... that is interesting
; big concepts in this section, higher-order functions, lambda and let...
; "No new mechanism is required in the interpreter in order to provide local variables"
; this probably why the let variable values do not escape and 'let over lambda' does not behave in the same way as common lisp

(define (pi-sum a b)
  (sum (lambda (x) (/ 1.0 (* x (+ x 2))))
       a
       (lambda (x) (+ x 4))
       b))

(define (integral f a b dx)
  (* (sum f
          (+ a (/ dx 2.0))
          (lambda (x) (+ x dx))
          b)
     dx))

;((lambda (x y z) (+ x y (square z))) 1 2 3)




