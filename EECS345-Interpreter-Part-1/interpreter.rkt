#lang racket

;Interpreter Project, Part 1
;EECS 345: Programming Language Concepts
;Group 6: Jiaqi Yang, Gary Yao, Zijun Liu

;------------------------------------------------------------------------------------------------------------------------------------------------------------
;How to use
;1. To interpret the parsing output and return a value for the program:
;(interpret "filename.txt")
;2. To test the interpreter program
;(testing)
;if return 'testpassed, the program passes the test 1-20 given by Prof. Connamacher
;otherwise the function will return the nearest unpassed test number
;------------------------------------------------------------------------------------------------------------------------------------------------------------

;require simpleParser.rkt to use the parser function
;Parser function takes a filename and parse the java and C-ish program into parser list
(require "simpleParser.rkt")

;------------------------------------------------------------------------------------------------------------------------------------------------------------
;Main functions
;------------------------------------------------------------------------------------------------------------------------------------------------------------

;Interpret Function calls Parser function in simpleParser.rkt and returns a list of parse tree
(define interpret
  (lambda (filename)
    (evaluate (parser filename) initialstate)))


;;Evaluate Function returns the return value in the final state
;;If the parselis is not fully evaluated, evaluate the rest of parselis
(define evaluate
  (lambda (parselis state)
    (cond
      [(and (null? parselis) (not (exist? 'return state))) (error "Missing return statement")]
      [(and (null? parselis) (exist? 'return state) (eq? (getvalue 'return state) #t)) 'true]
      [(and (null? parselis) (exist? 'return state) (eq? (getvalue 'return state) #f)) 'false]
      [(and (null? parselis) (exist? 'return state)) (getvalue 'return state)]
      [else (evaluate (cdr parselis) (mstate (car parselis) state))])))

;------------------------------------------------------------------------------------------------------------------------------------------------------------
;Helper functions
;------------------------------------------------------------------------------------------------------------------------------------------------------------

;Used in Interprete Function
;The initial state is a list consisting of two empty sublists
;Ex. ((x y) (1 2)) if x=1 and y=2
(define initialstate '(()()))

;Unused method to test what does the parsetree look like
(define testParser
  (lambda (filename)
    (parser filename)))
;------------------------------------------------------------------------------------------------------------------------------------------------------------
;mstate functions
;------------------------------------------------------------------------------------------------------------------------------------------------------------

;main function of mstate series
;redirect to different mstate functions based on the function header (if, while, =, var, return)
(define mstate
  (lambda (exp state)
    (cond
      [(null? exp) (error "Undefined expression, cannot get the state of the expression")]
      ;The format (var x)
      [(and (eq? (func exp) 'var) (= (length exp) 2)) (mstate_declare exp state)]
      ;The format (var x (mvalue))
      [(and (eq? (func exp) 'var) (= (length exp) 3)) (mstate_declare_assign exp state)]
      ;The format (= x (mvalue))
      [(eq? (func exp) '=) (mstate_assign exp state)]
      ;The format (return (mvalue)
      [(eq? (func exp) 'return) (mstate_return exp state)]
      ;The format (if (conditional) (then-statement))
      [(and (eq? (func exp) 'if) (= (length exp) 3)) (mstate_if_then exp state)]
      ;The format (if (conditional) (then-statement) (else-statement))
      [(and (eq? (func exp) 'if) (= (length exp) 4)) (mstate_if_then_else exp state)]
      ;The format (while (conditional) (body-statement))
      [(eq? (func exp) 'while) (mstate_while exp state)]
      ;Unexpected format
      [else (error "undefined expression")])))

;mstate_declare deals with the function header var with the format (var x)
;takes a exp list and a state list and returns the new state
(define mstate_declare
  (lambda (exp state)
    (cond
      [(exist? (variable exp) state) (error "Repeated declaration is not allowed")]
      [else (addnew (variable exp) 'error state)])))

;mstate_declare_assign deals with the function header var with the format (var x (mvalue))
;take a exp list and a state list and returns the new state
(define mstate_declare_assign
  (lambda (exp state)
    (cond
      [(exist? (variable exp) state) (error "Repeated declaration is not allowed")]
      [else (addnew (variable exp) (mvalue (value_assign exp) state) state)])))

;mstate_assign deals with the function header = with the format (= x (mvalue))
(define mstate_assign
  (lambda (exp state)
    (cond
      [(not (exist? (variable exp) state)) (error "Variable is not declared")]
      [else (addnew (variable exp) (mvalue (value_assign exp) state) (remove (variable exp) state))])))

;mstate_return deals with the function header return with the format (return (mvalue))
(define mstate_return
  (lambda (exp state)
    (cond
      [(exist? 'return state) state]
      [else (addnew 'return (mvalue (value_return exp) state) state)])))

;mstate_if_then deals with the funciton header if with the format (if (conditional) (then-statement))
(define mstate_if_then
  (lambda (exp state)
    (cond
      [(eq? (mvalue (conditional exp) state) #t) (mstate (then-statement exp) state)]
      [else state])))
      
;mstate_if_then_else deals with the function header if with the format (if (conditional) (then-statement) (else-statement))
(define mstate_if_then_else
  (lambda (exp state)
    (cond
      [(eq? (mvalue (conditional exp) state) #t) (mstate (then-statement exp) state)]
      [else (mstate (else-statement exp) state)])))

;mstate_while deals with the function header while with the format (while (conditional) (body-statement))
(define mstate_while
  (lambda (exp state)
    (cond
      [(eq? (mvalue (conditional exp) state) #t) (mstate_while exp (mstate (body-statement exp) state))]
      [else state])))
                    
;------------------------------------------------------------------------------------------------------------------------------------------------------------
;Helper functions
;------------------------------------------------------------------------------------------------------------------------------------------------------------

;The first atom in the expression
;ex. var, =, while, return, if
(define func car)

;The second atom in the expression for format (var x) and (= x (mvalue))
(define variable cadr)

;The third atom in the expression for format (var x (mvalue)) and (= x (mvalue))
(define value_assign caddr)

;The second atom in the expression for format (return (mvalue))
(define value_return cadr)

;The sublist of variables in state list
(define variablelist car)

;The sublist of values in state list
(define valuelist cadr)

;The second atom in the expression for format
;(if (conditional) (then-statement)), (if (conditional) (then-statement) (else-statement))
;and (while (conditional) (body-statement))
(define conditional cadr)

;The third atom in the expression for format
;(if (conditional) (then-statement)) and (if (conditional) (then-statement) (else-statement))
(define then-statement caddr)

;The fourth atom in the expression for format if (conditional) (then-statement) (else-statement))
(define else-statement cadddr)

;The thrid atom in the expression for format (while (conditional) (body-statement))
(define body-statement caddr)

;The first atom in variablelist
(define firstvariable
  (lambda (state)
    (car (variablelist state))))

;The first atom in valuelist
(define firstvalue
  (lambda (state)
    (car (valuelist state))))

;The state list without first atoms in both sublists
(define state_tail
  (lambda (state)
    (list (variablelist_tail state) (valuelist_tail state))))

;the rest of variablelist except for first atom
(define variablelist_tail
  (lambda (state)
    (cdr (variablelist state))))

;the rest of valuelist except for first atom
(define valuelist_tail
  (lambda (state)
    (cdr (valuelist state))))

;Test if the given var is already declared in the state list
;Return #t if declared, otherwise #f
(define exist?
  (lambda (var state)
    (cond
      [(null? (variablelist state)) #f]
      [(eq? var (firstvariable state)) #t]
      [else (exist? var (state_tail state))])))

;Add a new variable and its corresponding value in the state list
;takes new variable var, new value, the state list and returns the new state list
(define addnew
  (lambda (var value state)
    (list (cons var (variablelist state)) (cons value (valuelist state)))))

;Remove a variable and its corresponding value in the state list
;takes a variable var and returns the new state list
(define remove
  (lambda (var state)
    (remove-cps var state (lambda (vars values) (list vars values)))))

(define remove-cps
  (lambda (var state return)
    (cond
      [(null? (variablelist state)) (return '() '())]
      [(eq? var (firstvariable state)) (return (variablelist_tail state) (valuelist_tail state))]
      [else (remove-cps var (state_tail state)
                        (lambda (vars values) (return (cons (firstvariable state) vars) (cons (firstvalue state) values))))])))

;Get the value corresponding to the given value in the state list
(define getvalue
  (lambda (var state)
    (getvalue-lis var (variablelist state) (valuelist state))))

(define getvalue-lis
  (lambda (var varlis valuelis)
    (cond
      [(null? varlis) (error "Undecalred variable")]
      [(and (eq? (car varlis) var) (eq? (car valuelis) 'error)) (error "No value assigned to the variable")]
      [(eq? (car varlis) var) (car valuelis)]
      [else (getvalue-lis var (cdr varlis) (cdr valuelis))])))

;------------------------------------------------------------------------------------------------------------------------------------------------------------
;mvalue functions
;------------------------------------------------------------------------------------------------------------------------------------------------------------
; mvalue function takes an expression of numbers and operators and returns the value
; The operators are +, -, *, /, % (where division is integer division)
(define mvalue
  (lambda (exp state)
    (cond
      ;Mathematical operations  +, -, *, /, % (including the unary -)
      [(null? exp) (error 'undefined "undefined expression, cannot get the value of the expression")]
      [(not (list? exp)) (mvalue_test exp state)]
      [(eq? (operator exp) '+) (+ (mvalue_test (left-operand exp) state) (mvalue_test (right-operand exp) state))]
      ;unary -: negative sign, no right operand
      [(and (eq? (operator exp) '-) (null? (cddr exp))) (- 0 (mvalue_test (left-operand exp) state))]
      ;minus -: minus sign, has right operand
      [(eq? (operator exp) '-) (- (mvalue_test (left-operand exp) state) (mvalue_test (right-operand exp) state))]
      [(eq? (operator exp) '*) (* (mvalue_test (left-operand exp) state) (mvalue_test (right-operand exp) state))]
      [(eq? (operator exp) '/) (quotient (mvalue_test (left-operand exp) state) (mvalue_test (right-operand exp) state))]
      [(eq? (operator exp) '%) (remainder (mvalue_test (left-operand exp) state) (mvalue_test (right-operand exp) state))]

      ;Comparison operators ==, !=, <, >, <=, >=
      [(eq? (operator exp) '==) (eq? (mvalue_test (left-operand exp) state) (mvalue_test (right-operand exp) state))]
      [(eq? (operator exp) '!=) (not (eq? (mvalue_test (left-operand exp) state) (mvalue_test (right-operand exp) state)))]
      [(eq? (operator exp) '<) (< (mvalue_test (left-operand exp) state) (mvalue_test (right-operand exp) state))]
      [(eq? (operator exp) '>) (> (mvalue_test (left-operand exp) state) (mvalue_test (right-operand exp) state))]
      [(eq? (operator exp) '<=) (<= (mvalue_test (left-operand exp) state) (mvalue_test (right-operand exp) state))]
      [(eq? (operator exp) '>=) (>= (mvalue_test (left-operand exp) state) (mvalue_test (right-operand exp) state))]

      ;Boolean operators &&, ||, !
      [(eq? (operator exp) '&&) (and (mvalue_test (left-operand exp) state) (mvalue_test (right-operand exp) state))]
      [(eq? (operator exp) '||) (or (mvalue_test (left-operand exp) state) (mvalue_test (right-operand exp) state))]
      ;Same as unary -, ! operator only takes one expression
      [(and (eq? (operator exp) '!) (null? (cddr exp))) (not (mvalue_test (left-operand exp) state))]

      ;Otherwise, Undefined operator
      [else (error "Undefined operator")])))

;mvalue_test tests if the value of the expression is a list of expression, a number, or a variable
(define mvalue_test
  (lambda (exp state)
    (cond
      [(list? exp) (mvalue exp state)]
      [(boolean_exp? exp) (boolean_value exp)]
      [(number? exp) exp]
      [else (getvalue exp state)])))
;------------------------------------------------------------------------------------------------------------------------------------------------------------
;Helper functions
;------------------------------------------------------------------------------------------------------------------------------------------------------------
;Operator in mvalue function, In the first place of the list
(define operator car)

;Left-operand in mvalue function, In the second place of the list
(define left-operand cadr)

;Right-operand in mvalue function, In the third place of the list
(define right-operand caddr)

;Return #t if exp is 'true or 'false, Otherwise return #f
(define boolean_exp?
  (lambda (exp)
    (cond
      [(or (eq? exp 'true) (eq? exp 'false)) #t]
      [else #f])))

;Return #t if exp is 'true or #t, #f if exp is 'false or #f, Otherwise return error
(define boolean_value
  (lambda (exp)
    (cond
      [(eq? exp 'true) #t]
      [(eq? exp 'false) #f]
      [(boolean? exp) exp]
      [else (error "Undefined boolean expression")])))
;------------------------------------------------------------------------------------------------------------------------------------------------------------
;Tester functions
;------------------------------------------------------------------------------------------------------------------------------------------------------------
;Make sure test1.1.txt - test1.20.txt is included in the directory
(define testing
  (lambda ()
    (cond
      [(not (eq? (interpret "test1.1.txt") 150)) (error "test1 not passed")]
      [(not (eq? (interpret "test1.2.txt") -4)) (error "test2 not passed")]
      [(not (eq? (interpret "test1.3.txt") 10)) (error "test3 not passed")]
      [(not (eq? (interpret "test1.4.txt") 16)) (error "test4 not passed")]
      [(not (eq? (interpret "test1.5.txt") 220)) (error "test5 not passed")]
      [(not (eq? (interpret "test1.6.txt") 5)) (error "test6 not passed")]
      [(not (eq? (interpret "test1.7.txt") 6)) (error "test7 not passed")]
      [(not (eq? (interpret "test1.8.txt") 10)) (error "test8 not passed")]
      [(not (eq? (interpret "test1.9.txt") 5)) (error "test9 not passed")]
      [(not (eq? (interpret "test1.10.txt") -39)) (error "test10 not passed")]
      ;Manually test for 1.11 1.12 1.13 1.14
      ;Test 1.11 Variable is not declared
      ;Test 1.12 Undecalred variable
      ;Test 1.13 No value assigned to the variable
      ;Test 1.14 Repeated declaration is not allowed
      [(not (eq? (interpret "test1.15.txt") 'true)) (error "test15 not passed")]
      [(not (eq? (interpret "test1.16.txt") 100)) (error "test16 not passed")]
      [(not (eq? (interpret "test1.17.txt") 'false)) (error "test17 not passed")]
      [(not (eq? (interpret "test1.18.txt") 'true)) (error "test18 not passed")]
      [(not (eq? (interpret "test1.19.txt") 128)) (error "test19 not passed")]
      [(not (eq? (interpret "test1.20.txt") 12)) (error "test20 not passed")]
      [else 'TestPassed!!!!!!!!!!!!!!!!!!!!!!!])))