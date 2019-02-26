#lang racket

(provide (all-defined-out))

; Practice for Week1 Video "Dynamic Typing"

(define xs (list 3 6 9))

(define ys (list 24 33))

(define list1 (list 2 (list 3 5) 6 7 (list (list (list 6)))))

(define list2 (list 2 (list 3 5) 6 "hi" 7 (list (list (list 6)))))

(define list3 (list "hi" (list 2 (list 3 5) 6 7 (list (list (list 6))))))

(define list4 (list #f "hi" "apple"))



(define (sum1 xs)
  (if (null? xs)
      0
      (if (number? (car xs))
          (+ (car xs) (sum1 (cdr xs)))
          (+ (sum1 (car xs)) (sum1 (cdr xs))))))
     
  
(define (sum2 xs)
  (if (null? xs)
      0
      (if (number? (car xs))
          (+ (car xs) (sum2 (cdr xs)))
          (if (list? (car xs))
              (+ (sum2 (car xs)) (sum2 (cdr xs)))
              (sum2 (cdr xs))))))


(define (sum3 xs)
  (if (list? xs)
      (if (null? xs)
      0
      (if (number? (car xs))
          (+ (car xs) (sum3 (cdr xs)))
          (if (list? (car xs))
              (+ (sum3 (car xs)) (sum3 (cdr xs)))
              (sum3 (cdr xs)))))
      0))



; sum4 does the same thing as sum1 by usging "cond"
(define (sum4 xs)
  (cond [(null? xs) 0]
        [(number? (car xs)) (+ (car xs) (sum4 (cdr xs)))]
        [#t (+ (sum4 (car xs)) (sum3 (cdr xs)))]))



; sum5 == sum2 by using "cond"

(define (sum5 xs)
  (cond [(null? xs) 0]
        [(number? (car xs)) (+ (car xs) (sum5 (cdr xs)))]
        [(list? (car xs)) (+ (sum5 (car xs)) (sum5 (cdr xs)))]
        [#t (sum5 (cdr xs))]))


(define  (count-falses xs)
  (cond [(null? xs) 0]
        [(car xs) (count-falses (cdr xs))]
        [#t (+ 1 (count-falses (cdr xs)))]))

