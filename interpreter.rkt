;The following mathematical operations are implemented : +, -, *, /, % (including the unary -),
;the following comparison operators are implemented: ==, !=, <, >, <=. >=,
;and the following boolean operators: &&, ||, !.
;Variables may store values of type int as well as true and false.
;You do not have to detect an error if a program uses a type incorrectly (but it is not hard to add the error check).
;You do not have to implement short-circuit evaluation of && or ||, but you are welcome to do so.

;Formally, a parse tree is a list where each sublist corresponds to a statement. The different statements are:
;variable declaration	(var variable) or (var variable value)
;assignment	(= variable expression)
;return	(return expression)
;if statement	(if conditional then-statement optional-else-statement)
;while statement	(while conditional body-statement)

#lang racket

(require "simpleParser.scm")

;this is the method that should be called to interpret a file
(define evaluate
  (lambda (file)
    (Mstate_stmt_list (parser file) '(()()))))

;returns the state of a list of statements
(define Mstate_stmt_list
  (lambda (slist s)
    (if (null? slist)
        s
        (Mstate_stmt_list (cdr slist) (Mstate_stmt (car slist) s)))))

;returns the state that exists after the stmt is executed
;currently only accomodates 'return' statement
(define Mstate_stmt
  (lambda (stmt s)
    (cond
      ((eq? 'return (operator stmt)) (Mstate_return stmt s))
      ((or (eq? '+ (operator stmt))
           (eq? '- (operator stmt))
           (eq? '/ (operator stmt))
           (eq? '* (operator stmt))
           (eq? '% (operator stmt))) (Mvalue stmt s))
      ((or (eq? '> (operator stmt))
           (eq? '>= (operator stmt))
           (eq? '< (operator stmt))
           (eq? '<= (operator stmt))
           (eq? '== (operator stmt))
           (eq? '!= (operator stmt))) (Mboolean stmt s))
      ((eq? 'if (operator stmt)) (Mstate_if (first_operand stmt) (second_operand stmt) (cadddr stmt) s))
      ((eq? 'while (operator stmt)) (Mstate_while (first_operand stmt) (second_operand stmt) s))
      ((eq? '= (operator stmt)) (Mstate_assign (first_operand stmt) (second_operand stmt) s))
      ((and (eq? 'var (operator stmt)) (null? (cddr stmt))) (Mstate_declare (first_operand stmt) s))
      ((eq? 'var (operator stmt)) (Mstate_declare_and_assign (first_operand stmt) (second_operand stmt) s)))))

;returns the state of an if-then statement
(define Mstate_if
  (lambda (condition then else s)
    (if (Mboolean condition s)
        (Mstate_stmt then s)
        (Mstate_stmt else s))))

;returns the state of a while loop
(define Mstate_while
  (lambda (condition body s)
    (if (Mboolean condition s)
        (Mstate_while condition body (Mstate_stmt body s))
        s)))
        ;(Mstate_stmt body s))))

;returns the state after a declaration
(define Mstate_declare
  (lambda (variable s)
    (add_binding variable null s)))

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
    (Mvalue (first_operand stmt) s )))

;returns the value of the condition with the state s
(define Mboolean
  (lambda (condition s)
    (cond
      ((number? condition) condition)
      ((boolean? condition) '222)
      ((not(pair? condition)) (Mvalue_var condition s))
      ((eq? 'true condition) #t)
      ((eq? 'false condition) #f)
      ((eq? '&& (operator condition)) (and (Mboolean (first_operand condition) s) (Mboolean (second_operand condition) s)))
      ((eq? '|| (operator condition)) (or (Mboolean (first_operand condition) s) (Mboolean (second_operand condition) s)))
      ((eq? '== (operator condition))(eq? (Mvalue (first_operand condition) s) (Mvalue (second_operand condition) s)))
      ((eq? '!= (operator condition))(not (eq? (Mvalue (first_operand condition) s) (Mvalue (second_operand condition) s))))
      ((eq? '< (operator condition))(< (Mvalue (first_operand condition) s) (Mvalue (second_operand condition) s)))
      ((eq? '< (operator condition))(< (Mvalue (first_operand condition) s) (Mvalue (second_operand condition) s)))
      ((eq? '<= (operator condition))(<= (Mvalue (first_operand condition) s) (Mvalue (second_operand condition) s)))
      ((eq? '>= (operator condition))(>= (Mvalue (first_operand condition) s) (Mvalue (second_operand condition) s)))
      ((eq? '> (operator condition))(> (Mvalue (first_operand condition) s) (Mvalue (second_operand condition) s))))))

;returns the value of 'value' with the current state 's'
(define Mvalue
  (lambda (value s)
    (cond
      ;((null? s) 'Error)
      ;((number? value) value)
      ((boolean? value) value)
      ((eq? 'true value) #t)
      ((eq? 'false value) #f)
      ((not(pair? value)) (Mvalue_var value s))
      ((eq? '+ (operator value))(+ (Mvalue (first_operand value) s) (Mvalue (second_operand value)s)))
      ((eq? '- (operator value))(- (Mvalue (first_operand value) s) (Mvalue (second_operand value)s)))
      ((eq? '* (operator value))(* (Mvalue (first_operand value) s) (Mvalue (second_operand value)s)))
      ((eq? '/ (operator value))(quotient(Mvalue (first_operand value) s) (Mvalue (second_operand value)s)))
      ((eq? '% (operator value))(remainder(Mvalue (first_operand value) s) (Mvalue (second_operand value)s))))))

;returns the value of the variable with the current state 's'
(define Mvalue_var
  (lambda (value s)
    (cond
      ((or (null? value) (null? (name_list s)) (null? (value_list s))) '(123))
      ((equal? value (car (name_list s))) (car (value_list s)))
      (else (Mvalue_var value (cons (cdr (name_list s)) (cons (cdr (value_list s)) '())))))))

;checks to see if value is a variable or not
(define variable?
  (lambda (x s)
    (if (pair? (car x))
        #t
        #f)))

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
      ((eq? (car (name_list s)) variable) (cons (name_list s) (cons (cons value (cdr (value_list s))) '())))
      (else (add_binding (car (name_list s)) (car (value_list s)) (update_binding variable value (cons (cdr (name_list s)) (cons (cdr (value_list s)) '()))))))))

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


