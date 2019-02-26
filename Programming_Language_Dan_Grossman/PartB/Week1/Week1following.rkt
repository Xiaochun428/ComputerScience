#lang racket
(provide (all-defined-out))
"Hello World!"
(define x 3) ; val x = 3
(define y (+ x 2)) ; + is a function , call it here

(define cube1
  (lambda (x)
    (* x (* x x)))) ; x * (x * x)

(define cube2
  (lambda (x)
    (* x x x)))

(define (cube3 x)
  (* x x x))


(define (pow1 x y) ; x to the yth power (y must be nonegative)
  (if (= y 0)
      1
      (* x (pow1 x (- y 1)))))

(define pow2
  (lambda (x)
    (lambda (y)
      (pow1 x y)))) ; currying

(define three-to-the (pow2 3))

; Empty list: null
; Cons constructor: cons
; Access head of list: car
; Access tail of list: cdr
; Check for empty: null?

; sum all the numbers in a list

(define (sum xs)
  (if (null? xs)
      0
      (+ (car xs) (sum (cdr xs)))))

; append
(define (my-append xs ys)
  (if (null? xs)
      ys
      (cons (car xs) (my-append (cdr xs) ys))))

; map
(define (my-map f xs)
  (if (null? xs)
      null
      (cons (f (car xs))
            (my-map f (cdr xs)))))

; letrect practice， this doest work since the language semantics had been
; changed,  error occurs , I use a cariable before it is defined in letrec

(define (silly-mod2 x)
  (letrec
      ([even? (lambda (x) (if (zero? x) #t (odd? (- x 1))))]
       [odd? (lambda (x) (if (zero? x) #f (even? (- x 1))))])
    (if (even? x) 0 1)))




;(define (silly-mod3 x)
 ; (define (even? x) (if (zero? x) #t (odd (- x 1))))
  ;(define (odd? x) (if (zero? x) #f (even? (- x 1))))
  ;(if (even? x) 0 1))



