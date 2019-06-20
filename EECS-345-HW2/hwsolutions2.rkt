#lang racket
;; partition takes a list and a pivot and returns two lists: those elements <= pivot and those > pivot
;; run with the initial continuation (lambda (l1 l2) (list l1 l2))
(define partition-cps
  (lambda (p lis return)
    (cond
      ((null? lis) (return '() '()))
      ((< p (car lis)) (partition-cps p (cdr lis) (lambda (l1 l2) (return l1 (cons (car lis) l2)))))
      (else (partition-cps p (cdr lis) (lambda (l1 l2) (return (cons (car lis) l1) l2)))))))

;; The same append as before, we need it for quicksort
(define append-cps
  (lambda (l1 l2 return)
    (if (null? l1)
        (return l2)
        (append-cps (cdr l1) l2 (lambda (l) (return (cons (car l1) l)))))))

;; The quicksort algorithm.  Run with the (lambda (v) v) initial return continuation
(define quicksort-cps
  (lambda (lis return)
    (if (null? lis)
        (return lis)
        (partition-cps (car lis) (cdr lis) (lambda (l1 l2) (quicksort-cps l1 (lambda (s1) (quicksort-cps l2 (lambda (s2) (append-cps s1 (cons (car lis) s2) return))))))))))

;; replaceatoms-cps takes a list that may contain lists and a list of the remaining atoms available to use
(define replaceatoms-cps
  (lambda (lis atomslist return)
    (cond
      ((null? lis) (return '() atomslist))
      ((null? atomslist) (return lis '()))
      ((list? (car lis)) (replaceatoms-cps (car lis) atomslist (lambda (l1 r1) (replaceatoms-cps (cdr lis) r1 (lambda (l2 r2) (return (cons l1 l2) r2))))))
      (else (replaceatoms-cps (cdr lis) (cdr atomslist) (lambda (l r) (return (cons (car atomslist) l) r)))))))

;; The helper function to call the cps routine.
(define replaceatoms
  (lambda (lis atoms)
    (replaceatoms-cps lis atoms (lambda (l r) l))))

;; suffix
(define suffix-break
  (lambda (x lis break)
    (cond
      [(null? lis)       '()]
      [(eq? x (car lis)) (break (suffix-break x (cdr lis) break))] 
      [else              (cons (car lis) (suffix-break x (cdr lis) break))])))

(define suffix
  (lambda (x lis)
    (call/cc
     (lambda (break)
       (suffix-break x lis break)))))

;; xindex takes a list and wipes out the contents of a sublist if there is an x in it
(define xindex
  (lambda (x lis)
    (call/cc
     (lambda (k)
       (xindex-break x lis k 1)))))

(define xindex-break
  (lambda (x lis break index)
    (cond
      [(null? lis)     '()]
      [(list? (car lis)) (cons (call/cc (lambda (k)
                                          (xindex-break x (car lis) k 1)))
                               (xindex-break x (cdr lis) break (+ 1 index)))]
      [(eq? x (car lis)) (break (list index))]
      [else              (cons (car lis) (xindex-break x (cdr lis) break (+ 1 index)))])))