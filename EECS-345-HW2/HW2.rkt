#lang racket
;;;; ***************************************************
;;;; Jiaqi Yang (JXY530)
;;;; EECS 345 Spring 2019
;;;; Assignment 2
;;;; ***************************************************

; choose takes two integers, say n and k, and returns C(n,k)
; The combinatorial "choose" function from discrete mathematics
; Recall that C(n,k) = n/k * C(n-1,k-1), and C(n,0) = C(n,n) = 1
; > (choose 15 5) 3003
(define choose-cps
  (lambda (n k return)
    (if (zero? k)
        (return 1)
        (choose-cps (- n 1) (- k 1) (lambda (v) (return (* (/ n k) v)))))))

(define choose
  (lambda (n k)
    (choose-cps n k (lambda (v) v))))

; rotate takes three atoms and a list of atoms
; It returns a list that is the same as the input list
; except that each occurrence of the first atom is replaced by the second,
; the second atom is replaced by the third, and the third is replaced by the first
; > (rotate 'a 'b 'c '(a b c d e d c b a)) (b c a d e d a c b)
(define rotate-cps
  (lambda (a b c lis return)
    (cond
      [(null? lis)       (return '())]
      [(eq? (car lis) a) (rotate-cps a b c (cdr lis) (lambda (v) (return (cons b v))))]
      [(eq? (car lis) b) (rotate-cps a b c (cdr lis) (lambda (v) (return (cons c v))))]
      [(eq? (car lis) c) (rotate-cps a b c (cdr lis) (lambda (v) (return (cons a v))))]
      [else              (rotate-cps a b c (cdr lis) (lambda (v) (return (cons (car lis) v))))])))

(define rotate
  (lambda (a b c lis)
    (rotate-cps a b c lis (lambda (v) v))))

; squareroot takes two numbers, a value and an iteration
; The iteration will be an integer greater than or equal to 0
; The method will compute the squareroot of the value using iteration rounds of Newton's method,
; starting with an initial value equal to the input value.
; Newton's method is new = old - ((old * old) - value) / (2 * old)
; > (squareroot 5.0 0) 5.0
; > (squareroot 5.0 1) 3.0
; > (squareroot 5.0 5) 2.236067977499978
;> (squareroot 5 5) 2 514229/2178309
(define squareroot-cps
  (lambda (x i return)
    (if (zero? i)
        (return x)
        (squareroot-cps x (- i 1) (lambda (v) (return (- v (/ (- (* v v) x) (* 2 v)))))))))

(define squareroot
  (lambda (x i)
    (squareroot-cps x i (lambda (v) v))))

; partition takes a list and a value returns a pair of sublists
; The first contains all elements of the inupt list smaller than the value,
; and the other contains all the elements of the input that are larger than the value
; > (partition '(5 4 9 0 4 3 1 2 0 4 9 27 7 85) 8) ((5 4 0 4 3 1 2 0 4 7) (9 9 27 85))
(define partition-cps
  (lambda (lis n return)
    (cond
      [(null? lis)      (return '( () () ))]
      [(< (car lis) n)  (partition-cps (cdr lis) n (lambda (v) (return (list (cons (car lis) (car v)) (cadr v)))))]
      [(> (car lis) n)  (partition-cps (cdr lis) n (lambda (v) (return (list (car v) (cons (car lis) (cadr v))))))]
      [else             (partition-cps (cdr lis) n return)])))

(define partition
  (lambda (lis n)
    (partition-cps lis n (lambda (v) v))))

; rotate* takes three atoms and a list that may contain sublists
; It returns a list that is the same as the input list
; except that each occurrence of the first atom is replaced by the second,
; the second atom is replaced by the third, and the third is replaced by the first
;> (rotate* 'a 'b 'c '(a b ((c d) e) ((f e) d) (c (b (a))))) (b c ((a d) e) ((f e) d) (a (c (b))))
(define rotate*-cps
  (lambda (a b c lis return)
    (cond
      [(null? lis)       (return '())]
      [(list? (car lis)) (rotate*-cps a b c (car lis)
                                      (lambda (v1) (rotate*-cps a b c (cdr lis) (lambda (v2) (return (cons v1 v2))))))]
      [(eq? (car lis) a) (rotate*-cps a b c (cdr lis) (lambda (v) (return (cons b v))))]
      [(eq? (car lis) b) (rotate*-cps a b c (cdr lis) (lambda (v) (return (cons c v))))]
      [(eq? (car lis) c) (rotate*-cps a b c (cdr lis) (lambda (v) (return (cons a v))))]
      [else              (rotate*-cps a b c (cdr lis) (lambda (v) (return (cons (car lis) v))))])))

(define rotate*
  (lambda (a b c lis)
    (rotate*-cps a b c lis (lambda (v) v))))

; myfilter takes a function that is in CPS form and a list
; The input function should return true or false
; myfilter applies the function to each element of the list
; and returns a list that contains only those elements on which the function returns true, in order.
;> (myfilter (lambda (x return) (return (= 0 (remainder x 3)))) '(0 1 2 3 4 5 6)) (0 3 6)
(define myfilter-cps
  (lambda (f lis return)
    (if (null? lis)
        (return '())
        (f (car lis) (lambda (v1)
                       (if v1
                           (myfilter-cps f (cdr lis) (lambda (v2) (return (cons (car lis) v2))))
                           (myfilter-cps f (cdr lis) return)))))))

(define myfilter
  (lambda (f lis)
    (myfilter-cps f lis (lambda (v) v))))

; quicksort takes a list of numbers and returns a sorted version
; If you recall the quick sort algorithm,
; you use the partition function above with the car of the list as the value to partition on
; Then you recursively quicksort each sublist, and append the sorted lists back together
;(with the partition value added back in between the two lists).
;> (quicksort '()) '()
;> (quicksort '(8 1 3 9 6 5 7 2 4 10)) '(1 2 3 4 5 6 7 8 9 10)
(define append-cps
  (lambda (l1 l2 return)
    (if (null? l1)
        (return l2)
        (append-cps (cdr l1) l2 (lambda (v) (return (cons (car l1) v)))))))

(define quicksort-cps
  (lambda (lis return)
    (if (null? lis)
        (return '())
        (partition-cps lis (car lis) (lambda (lis1)
                                   (quicksort-cps (car lis1) (lambda (v1)
                                                               (quicksort-cps (cadr lis1) (lambda (v2)
                                                                                            (append-cps v1 (list (car lis)) (lambda (v3)
                                                                                                                                 (append-cps v3 v2 (lambda (v4) (return v4))))))))))))))

(define quicksort
  (lambda (lis)
    (quicksort-cps lis (lambda (v) v))))

; Write the following function so that it calls a helper function that is in CPS.
; replaceatoms takes two lists. The first list can contain sublists, but the second list is a single list of atoms
; The output should be the first list, but each atom of the first list, from left to right,
; is replaced by the corresponding atom of the second list, until the second list runs out of atoms
; > (replaceatoms '((a ((b) c d) ((((e) f g) (h i)) j (k l))) m n (o p)) '(z y x w v u t s r q p o n m l k j))
; ((z ((y) x w) ((((v) u t) (s r)) q (p o))) n m (l k))
; > (replaceatoms '((a ((b) c d) ((((e) f g) (h i)) j (k l))) m n (o p)) '(z y x w v u))
; ((z ((y) x w) ((((v) u g) (h i)) j (k l))) m n (o p))
(define replaceatoms-cps
  (lambda (lis-x lis-y return)
    (cond
      [(or (null? lis-x) (null? lis-y)) (return lis-x lis-y)]
      [(list? (car lis-x))              (replaceatoms-cps (car lis-x) lis-y (lambda (lis-x1 lis-y1)
                                                                              (replaceatoms-cps (cdr lis-x) lis-y1 (lambda (lis-x2 lis-y2)
                                                                                                (return (cons lis-x1 lis-x2) lis-y2)))))]
      [else                             (replaceatoms-cps (cdr lis-x) (cdr lis-y) (lambda (lis-x1 lis-y1)
                                                                                    (return (cons (car lis-y) lis-x1) lis-y1)))])))

(define replaceatoms
  (lambda (lis1 lis2)
    (replaceatoms-cps lis1 lis2 (lambda (lis1 lis2) lis1))))

; Write the following function using call/cc and a single helper function that uses "normal" recursion instead of tail recursion. 
; suffix takes an atom and a list and returns a list containing all elements that occur after the last occurrence of the atom.
; > (suffix 'x '(a b c))  (a b c)
; > (suffix 'x '(a b x c d x e f)) (e f)
(define suffix-break
  (lambda (x lis break)
    (cond
      [(null? lis)       '()]
      [(eq? (car lis) x) (break (suffix-break x (cdr lis) break))]
      [else              (cons (car lis) (suffix-break x (cdr lis) break))])))

(define suffix
  (lambda (x lis)
    (call/cc
     (lambda (k)
       (suffix-break x lis k)))))

; Write the following function using call/cc and a single helper function that uses "normal" recursion instead of tail recursion. 
; xindex takes an atom and a list containing sublists.
; The output list should be the same as the input list
; except that any sublist (including the main list) that contains the given atom should be emptied of all contents (atoms, lists, etc.),
; and instead, the only content of that sublist should be the index of the first occurrence of the atom in that list.
; > (xindex 'x '((a b c) (d e x f) (((g h) i) j) k (((l m x o)))))
; '((a b c) (3) (((g h) i) j) k (((3))))
; > (xindex 'x '((a b c) (d e x g) (((h i) x) j) x k (((l m x o)))))
; '(4)
; > (xindex 'x '((a b c) (d e x g) (((h i) x) j k ((l m x o)))))
; '((a b c) (3) ((2) j k ((3))))
(define xindex-break
  (lambda (x lis n break)
    (cond
      [(null? lis)       '()]
      [(list? (car lis)) (cons
                          (call/cc
                           (lambda (k)
                             (xindex-break x (car lis) 1 k)))
                          (xindex-break x (cdr lis) (+ 1 n) break))]
      [(eq? (car lis) x) (break (list n))]
      [else              (cons (car lis) (xindex-break x (cdr lis) (+ 1 n) break))])))

(define xindex
  (lambda (x lis)
    (call/cc
     (lambda (k)
       (xindex-break x lis 1 k)))))

    
    