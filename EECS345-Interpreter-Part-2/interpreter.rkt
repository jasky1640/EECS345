#lang racket

;Interpreter Project, Part 2
;EECS 345: Programming Language Concepts
;Group 6: Jiaqi Yang, Gary Yao, Zijun Liu

;------------------------------------------------------------------------------------------------------------------------------------------------------------
;How to use
;1. To interpret the parsing output and return a value for the program:
;(interpret "filename.txt")
;2. To test the interpreter program
;(testing)
;if return 'testpassed, the program passes the test 1-20 for Part 1, test 1-19 for Part 2 given by Prof. Connamacher
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
    (if (null? state)
      (error "Missing return statement")
      (mstate (car parselis) state (lambda (s) (evaluate (cdr parselis) s)) initialBreak initialContinue initialThrow initialReturn))))

;------------------------------------------------------------------------------------------------------------------------------------------------------------
;Helper functions
;------------------------------------------------------------------------------------------------------------------------------------------------------------

;Used in Interprete Function
;The initial state is a list consisting of one layer with a sublist of variable and a sublist of corresponding value
(define initialstate '( ( () ) ( () ) ) )

;Unused method to test what does the parsetree look like
(define testParser
  (lambda (filename)
    (parser filename)))

;initial definition of return
(define initialReturn
  (lambda (v s) v))

;initial definition of continue
(define initialContinue
  (lambda (s) (error "Not allowed to continue outside of block")))

;initial definition of throw
(define initialThrow
  (lambda (v s) (error "Not allowed to throw outside of block")))

;initial definition of break
(define initialBreak
  (lambda (s) (error "Not allowed to break outside of block")))
;------------------------------------------------------------------------------------------------------------------------------------------------------------
;mstate functions
;------------------------------------------------------------------------------------------------------------------------------------------------------------

;main function of mstate series
;redirect to different mstate functions based on the function header (if, while, =, var, return)
(define mstate
  (lambda (exp state next break continue throw return)
    (cond
      [(null? exp) (error "Undefined expression, cannot get the state of the expression")]
      ;The format (var x)
      [(and (eq? (func exp) 'var) (= (length exp) 2)) (mstate_declare exp state next)]
      ;The format (var x (mvalue))
      [(and (eq? (func exp) 'var) (= (length exp) 3)) (mstate_declare_assign exp state next)]
      ;The format (= x (mvalue))
      [(eq? (func exp) '=)                            (mstate_assign exp state next)]
      ;The format (return (mvalue)
      [(eq? (func exp) 'return)                       (mstate_return exp state next break continue throw return)]
      ;The format (if (conditional) (then-statement))
      [(and (eq? (func exp) 'if) (= (length exp) 3))  (mstate_if_then exp state next break continue throw return)]
      ;The format (if (conditional) (then-statement) (else-statement))
      [(and (eq? (func exp) 'if) (= (length exp) 4))  (mstate_if_then_else exp state next break continue throw return)]
      ;The format (while (conditional) (body-statement))
      [(eq? (func exp) 'while)                        (mstate_while exp state next break continue throw return)]
      ;The format (begin (expressons...)
      [(eq? (func exp) 'begin)                        (mstate_begin exp state next break continue throw return)]
      ;The format (break)
      [(eq? (func exp) 'break)                        (mstate_break exp state next break continue throw return)]
      ;The fomrat (continue)
      [(eq? (func exp) 'continue)                     (mstate_continue exp state next break continue throw return)]
      ;The format (try () .....)
      [(eq? (func exp) 'try)                          (mstate_try exp state next break continue throw return)]
      ;The format (throw x)
      [(eq? (func exp) 'throw)                         (mstate_throw exp state next break continue throw return)]
      ;Unexpected format
      [else                                           (error "undefined expression")])))

;mstate_declare deals with the function header var with the format (var x)
;takes a exp list and a state list and returns the new state
(define mstate_declare
  (lambda (exp state next)
      (next (addnew (variable exp) 'error state))))

;mstate_declare_assign deals with the function header var with the format (var x (mvalue))
;take a exp list and a state list and returns the new state
(define mstate_declare_assign
  (lambda (exp state next)
      (next (addnew (variable exp) (mvalue (value_assign exp) state) state))))

;mstate_assign deals with the function header = with the format (= x (mvalue))
(define mstate_assign
  (lambda (exp state next)
      (next (assignnew (variable exp) (mvalue (value_assign exp) state) state (lambda (v) v)))))

;mstate_begin deals with the funciton header begin with the format (begin (expressions....))
(define mstate_begin
  (lambda (exp state next break continue throw return)
      (addLayer state (lambda (s) (mstate_block (expTail exp) s
                                                 (lambda (s1) (removeLayer s1 (lambda (s2) (next s2))))
                                                 (lambda (s1) (removeLayer s1 (lambda (s2) (break s2))))
                                                 (lambda (s1) (removeLayer s1 (lambda (s2) (continue s2))))
                                                 (lambda (v1 s1) (removeLayer s1 (lambda (s2) (throw v1 s2))))
                                                 (lambda (v1 s1) (removeLayer s1 (lambda (s2) (return v1 s2)))))))))

;mstate_block is a helper function of mstate_begin to deal with the expression list in the format (begin (expressions....))
(define mstate_block
  (lambda (exp state next break continue throw return)
    (cond
      [(null? exp)    (next state)]
      [(eq? (car (car exp)) 'begin) (error "Multiple begin statement in one block")] 
      [else           (mstate (car exp) state (lambda (s) (mstate_block (cdr exp) s next break continue throw return)) break continue throw return)])))

;mstate_break deals with the format (break)
(define mstate_break
  (lambda (exp state next break continue throw return)
    (break state)))

;mstate_continue deals with the format (continue)
(define mstate_continue
  (lambda (exp state next break continue throw return)
    (continue state)))

;mstate_throw deals with the format (throw x)
(define mstate_throw
  (lambda (exp state next break continue throw return)
               (throw (mvalue (throw-statement exp) state) state)))

;mstate_return deals with the function header return with the format (return (mvalue))
(define mstate_return
  (lambda (exp state next break continue throw return)
    (cond
      [(null? (value_return exp))                 (error "Missing return value")]
      [(eq? (mvalue (value_return exp) state) #t) (return 'true state)]
      [(eq? (mvalue (value_return exp) state) #f) (return 'false state)]
      [else                                       (return (mvalue (value_return exp) state) state)])))

;mstate_if_then deals with the funciton header if with the format (if (conditional) (then-statement))
(define mstate_if_then
  (lambda (exp state next break continue throw return)
    (if (mvalue (conditional exp) state)
        (mstate (then-statement exp) state next break continue throw return)
        (next state))))
      
;mstate_if_then_else deals with the function header if with the format (if (conditional) (then-statement) (else-statement))
(define mstate_if_then_else
  (lambda (exp state next break continue throw return)
    (if (mvalue (conditional exp) state)
        (mstate (then-statement exp) state next break continue throw return)
        (mstate (else-statement exp) state next break continue throw return))))

;mstate_while deals with the function header while with the format (while (conditional) (body-statement))
(define mstate_while
  (lambda (exp state next break continue throw return)
    (if (mvalue (conditional exp) state)
        (mstate (body-statement exp) state (lambda (s) (mstate_while exp s next break continue throw return))
                                           (lambda (s) (next s))
                                           (lambda (s) (mstate_while exp s next break continue throw return)) throw return)
        (next state))))

;mstate_try deals with the function header try with the format (try ....)
(define mstate_try
  (lambda (exp state next break continue throw return)
    (cond
      [(null? (finally-block exp)) (addLayer state (lambda (s) (mstate_try_catch (try-statement exp) (catch-statement exp) s
                                                                                  (lambda (s1) (removeLayer s1 (lambda (s2) (next s2))))
                                                                                  (lambda (s1) (removeLayer s1 (lambda (s2) (break s2))))
                                                                                  (lambda (s1) (removeLayer s1 (lambda (s2) (continue s2))))
                                                                                  (lambda (v1 s1) (removeLayer s1 (lambda (s2) (throw v1 s2))))
                                                                                  (lambda (v1 s1) (removeLayer s1 (lambda (s2) (return v1 s2)))))))]
      [(null? (catch-block exp))   (addLayer state (lambda (s) (mstate_try_finally (try-statement exp) (finally-statement exp) s
                                                                                  (lambda (s1) (removeLayer s1 (lambda (s2) (next s2))))
                                                                                  (lambda (s1) (removeLayer s1 (lambda (s2) (break s2))))
                                                                                  (lambda (s1) (removeLayer s1 (lambda (s2) (continue s2))))
                                                                                  (lambda (v1 s1) (removeLayer s1 (lambda (s2) (throw v1 s2))))
                                                                                  (lambda (v1 s1) (removeLayer s1 (lambda (s2) (return v1 s2)))))))]
      [else                        (addLayer state (lambda (s) (mstate_try_catch_finally (try-statement exp) (catch-statement exp) (finally-statement exp) s
                                                                                  (lambda (s1) (removeLayer s1 (lambda (s2) (next s2))))
                                                                                  (lambda (s1) (removeLayer s1 (lambda (s2) (break s2))))
                                                                                  (lambda (s1) (removeLayer s1 (lambda (s2) (continue s2))))
                                                                                  (lambda (v1 s1) (removeLayer s1 (lambda (s2) (throw v1 s2))))
                                                                                  (lambda (v1 s1) (removeLayer s1 (lambda (s2) (return v1 s2)))))))])))

;mstate_try_catch deals with the function header try with the format (try (try-exp) (catch (e) ()) ())
(define mstate_try_catch
  (lambda (try-stmt catch-stmt state next break continue throw return)
    (mstate_block try-stmt state next break continue
                  (lambda (v s) (mstate_catch catch-stmt v s next break continue throw return)) return)))

;mstate_catch is a helper function of mstate_try_catch
(define mstate_catch
  (lambda (catch-stmt value state next break continue throw return)
    (if (null? value)
        (error "Missing thrown value")
        (removeLayer state (lambda (s) (addLayer s
                                                 (lambda (s1) (mstate_declare_assign (list 'var (catch-variable catch-stmt) value) s1
                                                   (lambda (s2) (mstate_block (cadr catch-stmt) s2 next break continue throw return))))))))))
                                  
;mstate_try_finally deals with the function header try with the format (try (try-exp) () (finally (finally-exp))
(define mstate_try_finally
  (lambda (try-stmt finally-stmt state next break continue throw return)
    (mstate_block try-stmt state
                  (lambda (s) (finally-next finally-stmt s next break continue throw return))
                  (lambda (s) (finally-break finally-stmt s next break continue throw return))
                  (lambda (s) (finally-continue finally-stmt s next break continue throw return))
                  (lambda (v s) (finally-throw finally-stmt s v next break continue throw return))
                  (lambda (v s) (finally-return finally-stmt s v next break continue throw return)))))
                  
;mstate_try_catch_finally deals with the function headr try with the format (try (try-exp) (catch (e) ()) (finally (finally-exp))
(define mstate_try_catch_finally
  (lambda (try-stmt catch-stmt finally-stmt state next break continue throw return)
    (mstate_block try-stmt state
               (lambda (s) (finally-next finally-stmt s next break continue throw return))
               (lambda (s) (finally-break finally-stmt s next break continue throw return))
               (lambda (s) (finally-continue finally-stmt s next break continue throw return))
               (lambda (v s) (finally-catch-throw catch-stmt finally-stmt s v next break continue throw return))
               (lambda (v s) (finally-return finally-stmt s v next break continue throw return)))))

;mstate_finally is a helper function of mstate_try_catch_finally series
(define mstate_finally
  (lambda (finally-stmt state next break continue throw return)
    (removeLayer state
                 (lambda (s1) (addLayer s1
                                        (lambda (s2) (mstate_block finally-stmt s2 next break continue throw return)))))))
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

;The expression list for format (begin (expressions...))
(define expTail cdr)

;The second atom in the expression for format (try () (catch () ()) (finally())
(define try-statement cadr)

;The third atom in the expression for format (try () (catch () ()) (finally())
(define catch-block caddr)

;The fourth atom in the expression for format (try () (catch () ()) (finally())
(define finally-block cadddr)

;The statement of catch block for format (try () (catch () ()) (finally())
(define catch-statement cdaddr)

;The statement of finally block for format (try () (catch () ()) (finally())
(define finally-statement
  (lambda (exp)
    (cadr (cadddr exp))))

;The second atom in the format (throw x)
(define throw-statement cadr)

(define catch-variable caar)

;A series of helper functions for next break continue throw catch-throw return in final statement
(define finally-next
  (lambda (finally-stmt state next break continue throw return)
    (mstate_finally finally-stmt state (lambda (s) (next s)) break continue throw return)))

(define finally-break
  (lambda (finally-stmt state next break continue throw return)
    (mstate_finally finally-stmt state next (lambda (s) (break s) continue throw return))))

(define finally-continue
  (lambda (finally-stmt state next break continue throw return)
    (mstate_finally finally-stmt state next break (lambda (s) (continue s) throw return))))

(define finally-throw
  (lambda (finally-stmt state value next break continue throw return)
    (mstate_finally finally-stmt state next break continue (lambda (s) (throw value s)) return)))

(define finally-catch-throw
  (lambda (catch-stmt finally-stmt state value next break continue throw return)
    (mstate_catch catch-stmt value state
               (lambda (s) (finally-next finally-stmt s next break continue throw return))
               (lambda (s) (finally-break finally-stmt s next break continue throw return))
               (lambda (s) (finally-continue finally-stmt s next break continue throw return))
               (lambda (v s) (finally-catch-throw finally-stmt s (lambda (s1) (throw v s1)) next break continue throw return))
               (lambda (v s) (finally-return finally-stmt s v next break continue throw return)))))           

(define finally-return
  (lambda (finally-stmt state value next break continue throw return)
    (mstate_finally finally-stmt state next break continue throw (lambda (s) (return value s)))))
                        
;Test if the given var is already declared in the variable list
;Return #t if declared, otherwise #f
(define exist?
  (lambda (var varlist)
    (cond
      [(null? varlist)         #f]
      [(list? (car varlist))   (or (exist? var (car varlist)) (exist? var (cdr varlist)))]
      [(eq? var (car varlist)) #t]
      [else                    (exist? var (cdr varlist))])))
      
;Add a new variable and its corresponding value in the top layer
;takes new variable var, new value, the state list and returns the new state list
(define addnew
  (lambda (var value state)
    (cond
      [(null? state)                     (error "Variable is not defined")]
      [(exist? var (variableList state)) (error "Repeated declaration is not allowed")]      
      [else                              (list (cons (cons var (firstVariableList state)) (restOfVariableList state))
                                               (cons (cons value (firstValueList state)) (restOfValueList state)))])))

;Helper function for assignnew method
(define assignnew-helper
  (lambda (var value state)
    (cond
      [(null? state)                (error "Varibale is not defined")]
      [(null? (variableList state)) (error "Variable is not defined")]
      [(eq? var (caar state))       (cons (car state) (list (cons value (cdadr state))))]
      [else ((lambda (newState)
             (cons (cons (caar state) (car newState)) (list (cons (caadr state) (cadr newState)))))
             (assignnew-helper var value (cons (cdar state) (list (cdadr state)))))])))

;Assign a value to an existed value
;takes variable var, new value, the state list and returns the new statet list
(define assignnew
  (lambda (var value state return)
    (cond
      [(null? state) (error "Variable is not defined")]
      [(null? (variableList state)) (error "Variable is not defined")]
      [(exist? var (firstVariableList state)) (return (cons (variableList state)
                                                           (list (cons (valueList (assignnew-helper var value (firstState state))) (restOfValueList state)))))]
      [else (assignnew var value (cons (restOfVariableList state) (list (restOfValueList state)))
                       (lambda (s) (return (cons (cons (firstVariableList state) (variableList s)) (list (cons (firstValueList state) (valueList s)))))))])))

;Get the value corresponding to the given value in the state list
(define getvalue
  (lambda (var state)
    (cond    
      [(null? state)                                                         (error "Undeclared variable")]
      [(null? (car state))                                                   (error "Undeclared variable")]
      [(null? (firstVariableList state))                                     (getvalue var (list (restOfVariableList state) (restOfValueList state)))]
      [(and (eq? var (firstVariable state)) (eq? 'error (firstValue state))) (error "Variable is not assigned any value")]
      [(eq? var (firstVariable state))                                       (firstValue state)]
      [else                                                                  (getvalue var (list (cons (restOfFirstVariableList state) (restOfVariableList state))
                                                                                                 (cons (restOfFirstValueList state) (restOfValueList state))))])))
;The list of variable sublists
(define variableList car)

;The list of value sublists
(define valueList cadr)

;The first variable in the top layer variable list
(define firstVariable caaar)

;The rest of variables in the top layer variable list
(define restOfFirstVariableList cdaar)

;The first value in the top layer value list
(define firstValue caaadr)

;The rest of values in the top layer value list
(define restOfFirstValueList cdaadr)

;The variable list in the top layer
(define firstVariableList caar)

;The rest of variable list except for the top layer
(define restOfVariableList cdar)

;The value list in the top layer
(define firstValueList caadr)

;The rest of value list except for the top layer
(define restOfValueList cdadr)

;The list combined top-layer variable sublist and value sublist
(define firstState
  (lambda (state)
    (list (firstVariableList state) (firstValueList state))))
 
;Add a new layer on the top of the state list
(define addLayer
  (lambda (state next)
    (next (list (cons '() (variableList state)) (cons '() (valueList state))))))

;Remove the top-most layer in the state list
(define removeLayer
  (lambda (state next)
    (next (list (restOfVariableList state) (restOfValueList state)))))
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
      ;Test for Part 1
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

      ;Test for Part 2
      [(not (eq? (interpret "test2.1.txt") 20)) (error "test1 not passed")]
      [(not (eq? (interpret "test2.2.txt") 164)) (error "test2 not passed")]
      [(not (eq? (interpret "test2.3.txt") 32)) (error "test3 not passed")]
      [(not (eq? (interpret "test2.4.txt") 2)) (error "test4 not passed")]
      ;Manually test for 2.5
      [(not (eq? (interpret "test2.6.txt") 25)) (error "test6 not passed")]
      [(not (eq? (interpret "test2.7.txt") 21)) (error "test7 not passed")]
      [(not (eq? (interpret "test2.8.txt") 6)) (error "test8 not passed")]
      [(not (eq? (interpret "test2.9.txt") -1)) (error "test9 not passed")]
      [(not (eq? (interpret "test2.10.txt") 789)) (error "test10 not passed")]
      ;Manually test for 2.11 2.12 2.13
      [(not (eq? (interpret "test2.14.txt") 12)) (error "test14 not passed")]
      [(not (eq? (interpret "test2.15.txt") 125)) (error "test15 not passed")]
      [(not (eq? (interpret "test2.16.txt") 110)) (error "test16 not passed")]
      [(not (eq? (interpret "test2.17.txt") 2000400)) (error "test17 not passed")]
      [(not (eq? (interpret "test2.18.txt") 101)) (error "test18 not passed")]
      ;Manually test for 2.19


      [else 'TestPassed!!!!!!!!!!!!!!!!!!!!!!!])))