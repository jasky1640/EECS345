#lang racket

; 1. choose takes two integers n and k and computer C(n,k)
(define choose
  (lambda (n k)
    (if (zero? k)
        1
        (* (/ n k) (choose (- n 1) (- k 1))))))

; 2. rotate takes three atoms and a list and replaces each a with b, each b with c, and each c with b
(define rotate
  (lambda (a b c lis)
    (cond
      [(null? lis)       '()]
      [(eq? a (car lis)) (cons b (rotate a b c (cdr lis)))]
      [(eq? b (car lis)) (cons c (rotate a b c (cdr lis)))]
      [(eq? c (car lis)) (cons a (rotate a b c (cdr lis)))]
      [else              (cons (car lis)(rotate a b c (cdr lis)))])))

; 3. moveXleft takes a list of atoms and a special atom x, and moves each x one place to the left in the list
(define moveXleft
  (lambda (x lis)
    (cond
      [(null? lis)        lis]
      [(null? (cdr lis))  lis]
      [(eq? x (cadr lis)) (cons x (cons (car lis) (moveXleft x (cddr lis))))]
      [else               (cons (car lis) (moveXleft x (cdr lis)))])))

; 4. myfilter takes a function and a list and returns a list with those elements that pass the function
(define myfilter
  (lambda (f lis)
    (cond
      [(null? lis)   '()]
      [(f (car lis)) (cons (car lis) (myfilter f (cdr lis)))]
      [else          (myfilter f (cdr lis))])))

; 5. squareroot function using Newton's method
;   new = old - (old * old - value) / (2 * old)
(define squareroot
  (lambda (x n)
    (if (zero? n)
        x
        (- (squareroot x (- n 1)) (/ (- (* (squareroot x (- n 1)) (squareroot x (- n 1))) x) (* 2 (squareroot x (- n 1))))))))

; Here is a more efficient version using let.  I would like you to avoid let for now until you get used to functional coding.
(define squareroot2
  (lambda (x n)
    (if (zero? n)
        x
        (let ((old (squareroot2 x (- n 1))))
          (- old (/ (- (* old old) x) (* 2 old)))))))

; Here is an efficient version using functions to prove that you do not need to use let.
(define squareroot3
  (lambda (x n)
    (if (zero? n)
        x
        ((lambda (old) (- old (/ (- (* old old) x) (* 2 old))))
         (squareroot3 x (- n 1))))))

; 6. rotate* takes a list and 3 atoms and replaces every a with b, every b with c, and every c with a
(define rotate*
  (lambda (a b c lis)
    (cond
      [(null? lis) '()]
      [(pair? (car lis)) (cons (rotate* a b c (car lis)) (rotate* a b c (cdr lis)))]
      [(eq? a (car lis)) (cons b (rotate* a b c (cdr lis)))]
      [(eq? b (car lis)) (cons c (rotate* a b c (cdr lis)))]
      [(eq? c (car lis)) (cons a (rotate* a b c (cdr lis)))]
      [else              (cons (car lis) (rotate* a b c (cdr lis)))])))

; 7. flattenN removes all parentheses that are nested more than n deep.
(define flattenN
  (lambda (n lis)
    (cond
      [(null? lis)                      '()]
      [(and (<= n 1) (list? (car lis))) (append (flattenN n (car lis)) (flattenN n (cdr lis)))]
      [(list? (car lis))                (cons (flattenN (- n 1) (car lis)) (flattenN n (cdr lis)))]
      [else                             (cons (car lis) (flattenN n (cdr lis)))])))

; 8. outerproduct computes the outer product of two vectors.
(define outerproduct
  (lambda (v1 v2)
    (if (null? v2)
        '()
        (cons (mymap (lambda (v) (* (car v2) v)) v1) (outerproduct v1 (cdr v2))))))

; mymap takes a function and a list and applies the function to each element of the list (just like the built in map function)
(define mymap
  (lambda (f lis)
    (if (null? lis)
        '()
        (cons (f (car lis)) (mymap f (cdr lis))))))

; 9. maxvalue* takes a list of lists of numbers, and returns the maximum value or 'novalue if there are no numbers in the list
(define maxvalue*
  (lambda (lis)
    (cond
      [(null? lis)                               'novalue]
      [(and (list? (car lis)) (null? (cdr lis))) (maxvalue* (car lis))]
      [(list? (car lis))                         (maxof (maxvalue* (car lis)) (maxvalue* (cdr lis)))]
      [(null? (cdr lis))                         (car lis)]
      [else                                      (maxof (car lis) (maxvalue* (cdr lis)))])))

; a helper function to compute the maximum of two values where the values could be a number or a 'novalue
(define maxof
  (lambda (a b)
    (cond
      [(eq? a 'novalue) b]
      [(eq? b 'novalue) a]
      [(< a b)          b]
      [else             a])))


; 10. moveXleft* moves each x one spot to the left, including parentheses
(define moveXleft*
  (lambda (x lis)
    (cond
      [(null? lis) '()]
      [(and (pair? (car lis)) (eq? x (caar lis)) (null? (cdr lis)))
       (cons x (cons (moveXleft* x (cdar lis)) (moveXleft* x (cdr lis))))]
      [(and (pair? (car lis)) (eq? x (caar lis)) (eq? x (cadr lis)))
       (cons x (cons (append (moveXleft* x (cdar lis)) (cons x '())) (moveXleft* x (cddr lis))))]
      [(and (or (list? (car lis)) (pair? (car lis))) (null? (cdr lis)))
       (cons (moveXleft* x (car lis)) '())]
      [(and (list? (car lis)) (eq? x (cadr lis)))
       (cons (append (moveXleft* x (car lis)) (cons x '())) (moveXleft* x (cddr lis)))]
      [(pair? (car lis))
       (cons (moveXleft* x (car lis)) (moveXleft* x (cdr lis)))]
      [(null? (cdr lis)) lis]
      [(eq? x (cadr lis))
       (cons x (cons (car lis) (moveXleft* x (cddr lis))))]
      [else
       (cons (car lis) (moveXleft* x (cdr lis)))])))

