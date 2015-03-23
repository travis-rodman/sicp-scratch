
; basic definitions for functions dealing with compound numbers
(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))
(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom a) (numer y))))
(define (equal-rat?  n d))

(define (make-rat n d) (cons n d))
(define (numer x) (car x))
(define (denom x) (cdr x))
(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))
(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))

; 2.1
(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ (if (< d 0) (* -1 n) n) g) (/ (abs d) g))))

;(make-rat -3 4)
;(make-rat 3 -4)
; continuation of the theme from ch1, single-point modifications
; changing the functionality easily
; the code that uses these functions did not need to change in
; any way for this additional functionality to useable.

; this is leading my thinking towards OO paradigms, it will be 
; interesting to see where this chapter will go with these ideas

; noted, the introduction of list/tree data structures

; abstraction barrier...
