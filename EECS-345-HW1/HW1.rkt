#lang racket
;;;; ***************************************************
;;;; Jiaqi Yang (JXY530)
;;;; EECS 345 Spring 2019
;;;; Assignment 1
;;;; ***************************************************


;;Problem 1: "choose" function takes two numbers and calculates the combination of C(n,k)
(define choose
  (lambda (n k)
    (cond
      [(zero? k)            1]
      [else                 (* (/ n k) (choose (- n 1) (- k 1)))])))

;;Problem 2: "rotate" function takes three atoms and a lat and replaces 1st by 2nd, 2nd by 3rd, 3rd by 1st
(define rotate
  (lambda (a b c lat)
    (cond
      [(null? lat)         '()]
      [(eq? a (car lat))    (cons b (rotate a b c (cdr lat)))]
      [(eq? b (car lat))    (cons c (rotate a b c (cdr lat)))]
      [(eq? c (car lat))    (cons a (rotate a b c (cdr lat)))]
      [else                 (cons (car lat) (rotate a b c (cdr lat)))])))

;;Problem 3: "moveXleft" function takes an atom and a list and shifts the atom location one to the left
(define moveXleft
  (lambda (x lis)
    (cond
    [(null? lis)                  '()]
    [(and (not (null? (cdr lis))) (eq? x (car (cdr lis)))) (cons  x (cons (car lis) (moveXleft x (cdr (cdr lis)))))]
    [else                         (cons (car lis) (moveXleft x (cdr lis)))])))

;;Problem 4: "myfilter" takes a function and a list and return a list that contains elemenst returns true using the function
(define myfilter
  (lambda (f lis)
    (cond
      [(null? lis)        '()]
      [(f (car lis))      (cons (car lis) (myfilter f (cdr lis)))]
      [else               (myfilter f (cdr lis))])))

;;Problem 5: "squareroot" takes two numbers, a value and an iteration and returns squareroot of the value
(define squareroot
  (lambda (x y)
    (cond
      [(zero? y)          x]
      [(zero? (- y 1))   (- x ( / (- (* x x) x) (* 2 x)))]
      [else              (- (squareroot x (- y 1)) ( / (- (* (squareroot x (- y 1)) (squareroot x (- y 1))) x ) (* 2 (squareroot x (- y 1)))))])))

;;Problem 6: "rotate*" function takes three atoms and a list and replace 1st by 2nd, 2nd by 3rd, 3rd by 1st
(define rotate*
  (lambda (a b c lis)
    (cond
      [(null? lis)          '()]
      [(eq? a (car lis))    (cons b (rotate* a b c (cdr lis)))]
      [(eq? b (car lis))    (cons c (rotate* a b c (cdr lis)))]
      [(eq? c (car lis))    (cons a (rotate* a b c (cdr lis)))]
      [(list? (car lis))    (cons (rotate* a b c (car lis)) (rotate* a b c (cdr lis)))]
      [else                 (cons (car lis) (rotate* a b c (cdr lis)))])))

;;Problem 7: "flattenN" function takes a number and a list and removes parentheses deeper than N
(define flattenN
  (lambda (n lis)
    (cond
      [(null? lis)                               '()]
      [(and (zero? (- n 1)) (list? (car lis)))   (append (flattenN n (car lis)) (flattenN n (cdr lis)))]
      [(list? (car lis))                         (cons (flattenN (- n 1) (car lis)) (flattenN n (cdr lis)))]
      [else                                      (cons (car lis) (flattenN n (cdr lis)))])))

;;Problem 8: "outerproduct" function takes two vectors and returns the matrix that is the outerproduct of the vectors 
(define outerproduct
  (lambda (lis1 lis2)
    (cond
      [(null? lis2)       '()]
      [else               (cons (listmultiply (car lis2) lis1) (outerproduct lis1 (cdr lis2)))])))                                   

;;Helper Method for Problem 8: "listmultiply" takes a number and a list and returns a list with all numbers in the original list multiplied by the number
(define listmultiply
  (lambda (n lis)
    (cond
      [(null? lis)        '()]
      [else               (cons (* n (car lis)) (listmultiply n (cdr lis)))])))
      
;;Problem 9: "maxvalue*" function takes a list and returns the largest number in the list
;;Helper Method for Problem 0: "flattenN method from problem7
(define maxvalue*
  (lambda (lis)
    (cond
      [(null? (flattenN 1 lis))                                      'novalue]
      [(null? (cdr (flattenN 1 lis)))                                (car lis)]
      [(> (car (flattenN 1 lis)) (car (cdr (flattenN 1 lis))))       (maxvalue*(cons (car (flattenN 1 lis)) (cdr (cdr (flattenN 1 lis)))))]
      [else                                                          (maxvalue*(cdr (flattenN 1 lis)))])))

;Problem 10: "moveXleft*" takes an atom and a list and shifts the atom location one to the left (Even left out of parentheses except for very first element)
(define moveXleft*
  (lambda (a lis)
    (cond
      [(null? lis)                                                                                                 '()]
      ;;a list include a at first and after it
      [(and (list? (car lis)) (not (null? (cdr lis))) (eq? a (car (car lis))) (eq? a (car (cdr lis))))             (cons a (cons (append (moveXleft* a (cdr (car lis))) (cons (car (cdr lis)) '())) (moveXleft* a (cdr (cdr lis)))))]
      ;;a list don't include a at first but after it
      [(and (list? (car lis)) (not (null? (cdr lis))) (eq? a (car (cdr lis))))                                     (cons (append (moveXleft* a (car lis)) (cons (car (cdr lis)) '())) (moveXleft* a (cdr (cdr lis))))]
      ;;a list includes a at first but not after it
      [(and (list? (car lis)) (eq? a (car (car lis))) )                                                            (cons a (cons (moveXleft* a (cdr (car lis))) (moveXleft* a (cdr lis))))]
      ;;a list don't include a at first or after it
      [(list? (car lis))                                                                                           (cons (moveXleft* a (car lis)) (moveXleft* a (cdr lis)))]
      ;;an atom that is a
      [(and (not (null? (cdr lis))) (eq? a (car (cdr lis))))                                                       (cons a (cons (car lis) (moveXleft* a (cdr (cdr lis)))))]
      ;;an atom that is not a
      [else                                                                                                        (cons (car lis) (moveXleft* a (cdr lis)))])))