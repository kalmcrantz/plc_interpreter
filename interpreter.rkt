;Kimberly Almcrantz (kaa97) Jenny Zhao (sxz402)

#lang racket

(require "simpleParser.scm")

;this is the method that should be called to interpret a file
(define evaluate
  (lambda (file)
    (Mstate_stmt_list (parser file) initial_state)))

;returns the state after a list of statements
(define Mstate_stmt_list
  (lambda (slist s)
    (if (null? slist)
        s
        (Mstate_stmt_list (cdr slist) (Mstate_stmt (car slist) s)))))

;returns the state that exists after the stmt is executed
(define Mstate_stmt
  (lambda (stmt s)
    (cond
      ((null? stmt) s)
      ((eq? 'return (operator stmt)) (Mstate_return stmt s))
      ((or (eq? '+ (operator stmt))
           (eq? '- (operator stmt))
           (eq? '/ (operator stmt))
           (eq? '* (operator stmt))
           (eq? '% (operator stmt))
           (eq? '> (operator stmt))
           (eq? '>= (operator stmt))
           (eq? '< (operator stmt))
           (eq? '<= (operator stmt))
           (eq? '== (operator stmt))
           (eq? '!= (operator stmt))) (Mvalue stmt s))
      ((and (eq? 'if (operator stmt)) (null? (cdddr stmt))) (Mstate_if (first_operand stmt) (second_operand stmt) null s))
      ((eq? 'if (operator stmt)) (Mstate_if (first_operand stmt) (second_operand stmt) (third_operand stmt) s))
      ((eq? 'while (operator stmt)) (Mstate_while (first_operand stmt) (second_operand stmt) s))
      ((eq? '= (operator stmt)) (Mstate_assign (first_operand stmt) (second_operand stmt) s))
      ((and (eq? 'var (operator stmt)) (null? (cddr stmt))) (Mstate_declare (first_operand stmt) s))
      ((eq? 'var (operator stmt)) (Mstate_declare_and_assign (first_operand stmt) (second_operand stmt) s)))))

;returns the state of an if-then statement
(define Mstate_if
  (lambda (condition then else s)
    (if (Mvalue condition s)
        (Mstate_stmt then s)
        (Mstate_stmt else s))))

;returns the state of a while loop
(define Mstate_while
  (lambda (condition body s)
    (if (Mvalue condition s)
        (Mstate_while condition body (Mstate_stmt body s))
        s)))

;returns the state after a declaration
(define Mstate_declare
  (lambda (variable s)
    (if (variable_declared? variable (name_list s))
        (raise 'Variable-already-declared)
        (add_binding variable null s))))

;returns the state after an assignment
(define Mstate_assign
  (lambda (variable value s)
    (update_binding variable (Mvalue value s) s)))

;returns the state of a variable declaration and assignment
(define Mstate_declare_and_assign
  (lambda (variable value s)
    (Mstate_assign variable value (Mstate_declare variable s))))

;returns the state that exists after a return statement is executed
(define Mstate_return
  (lambda (stmt s)
    (cond
      ((eq? #t (Mvalue (first_operand stmt) s)) 'true)
      ((eq? #f (Mvalue (first_operand stmt) s)) 'false)
      (else (Mvalue (first_operand stmt) s)))))

;returns the value of 'value' with the current state 's'
(define Mvalue
  (lambda (value s)
    (cond
      ((null? s) (raise 'Value-does-not-exist))
      ((number? value) value)
      ((boolean? value) value)
      ((eq? 'true value) #t)
      ((eq? 'false value) #f)
      ((or (eq? #t value) (eq? #f value)) value)
      ((not(pair? value)) (Mvalue_var value s))
      ((eq? '+ (operator value)) (+ (Mvalue (first_operand value) s) (Mvalue (second_operand value) s)))
      ((and (eq? '- (operator value)) (null? (cddr value)))(- 0 (Mvalue (first_operand value) s)))
      ((eq? '- (operator value)) (- (Mvalue (first_operand value) s) (Mvalue (second_operand value)s)))
      ((eq? '* (operator value))(* (Mvalue (first_operand value) s) (Mvalue (second_operand value)s)))
      ((eq? '/ (operator value))(quotient(Mvalue (first_operand value) s) (Mvalue (second_operand value)s)))
      ((eq? '% (operator value))(remainder(Mvalue (first_operand value) s) (Mvalue (second_operand value)s)))
      ((eq? '&& (operator value)) (and (Mvalue (first_operand value) s) (Mvalue (second_operand value) s)))
      ((eq? '|| (operator value)) (or (Mvalue (first_operand value) s) (Mvalue (second_operand value) s)))
      ((eq? '&& (operator value)) (and (Mvalue (first_operand value) s) (Mvalue (second_operand value) s)))
      ((eq? '|| (operator value)) (or (Mvalue (first_operand value) s) (Mvalue (second_operand value) s)))
      ((eq? '== (operator value))(eq? (Mvalue (first_operand value) s) (Mvalue (second_operand value) s)))
      ((eq? '!= (operator value))(not (eq? (Mvalue (first_operand value) s) (Mvalue (second_operand value) s))))
      ((eq? '< (operator value))(< (Mvalue (first_operand value) s) (Mvalue (second_operand value) s)))
      ((eq? '<= (operator value))(<= (Mvalue (first_operand value) s) (Mvalue (second_operand value) s)))
      ((eq? '>= (operator value))(>= (Mvalue (first_operand value) s) (Mvalue (second_operand value) s)))
      ((eq? '> (operator value))(> (Mvalue (first_operand value) s) (Mvalue (second_operand value) s)))
      ((eq? '! (operator value)) (not (Mvalue (first_operand value) s)))
      (else (raise 'not-valid-operator)))))

;returns the value of the variable with the current state 's'
(define Mvalue_var
  (lambda (value s)
    (cond
      ((eq? value 'true) #t)
      ((eq? value 'false) #f)
      ((or (null? value) (null? (name_list s)) (null? (value_list s))) (raise 'Variable-not-declared))
      ((and (eq? value (car (name_list s))) (null? (car (value_list s)))) (raise 'Variable-not-assigned-a-value))
      ((eq? value (car (name_list s)))(car (value_list s)))
      (else (Mvalue_var value (cons (cdr (name_list s)) (cons (cdr (value_list s)) '())))))))

;adds a binding to the state
;paramters: 'name' of binding and 'value' of binding
(define add_binding
  (lambda (name value s)
    (cons (cons name (name_list s)) (cons (cons value (value_list s)) '()))))

;updates the variable with the value
(define update_binding
  (lambda (variable value s)
    (cond
      ((or (null? value) (null? s)) 'Error)
      ((not (variable_declared? variable (name_list s))) (raise 'Variable-not-declared))
      ((eq? (car (name_list s)) variable) (cons (name_list s) (cons (cons value (cdr (value_list s))) '())))
      (else (add_binding (car (name_list s)) (car (value_list s)) (update_binding variable value (cons (cdr (name_list s)) (cons (cdr (value_list s)) '()))))))))

;returns whether or not the variable is declared in the state s
(define variable_declared?
  (lambda (variable lis)
    (cond
      ((null? lis) #f)
      ((eq? variable (car lis)) #t)
      (else (variable_declared? variable (cdr lis))))))

;returns the list of names of bindings in a state s
(define name_list
  (lambda (s)
    (car s)))

;returns the list of values of bindings in a state s
(define value_list
  (lambda (s)
    (car (cdr s))))

;returns the first element of a list (operator)
(define operator car)

;returns the second element of a list (first operator)
(define first_operand cadr)

;returns the third element of a list (second operator)
(define second_operand caddr)

;returns the fourth element of a list (third operator)
(define third_operand cadddr)

;returns the inital state at the very beginning of a program
(define initial_state '(()()))