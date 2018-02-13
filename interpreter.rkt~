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

(require "simpleParser.scm")

;this is the method that should be called to interpret a file
(define evaluate
  (lambda (file)
    (Mvalue 'return (Mstate_stmt_list (parser file) '(()())))))

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
      ((eq? 'return (car stmt)) (Mstate_return stmt s))
      ((eq? 'if (car stmt)) (Mstate_if (cadr stmt) (caddr stmt) (cadddr stmt) s)))))

;returns the state of an if-then statement
(define Mstate_if
  (lambda (condition then else s)
    (if (Mboolean condition s)
        (Mstate_stmt then s)
        (Mstate_stmt else s))))

;returns the value of the condition with the state s
(define Mboolean
  (lambda (condition s)
    condition))

;returns the value of 'value' with the current state 's'
(define Mvalue
  (lambda (value s)
    (cond
      ((null? s) 'Error)
      ((number? value) value)
      ((eq? value (car (name_list s))) (car (value_list s)))
      (else (Mvalue value (cons (cdr (name_list s)) (cons (cdr (value_list s)) '())))))))

;returns the state that exists after a return statement is executed
(define Mstate_return
  (lambda (stmt s)
    (add_binding 'return (Mvalue (first_operand stmt) s) s)))

;adds a binding to the state
;paramters: 'name' of binding and 'value' of binding
(define add_binding
  (lambda (name value s)
    (cons (cons name (value_list s)) (cons (cons value (value_list s)) '()))))

;returns the list of names of bindings in a state s
(define name_list
  (lambda (s)
    (car s)))

;returns the list of values of bindings in a state s
(define value_list
  (lambda (s)
    (car (cdr s))))

;returns the first element of a list (operator)
(define operator
  (lambda (lis)
    (if (null? lis)
        lis
        (car lis))))

;returns the second element of a list (first operator)
(define first_operand
  (lambda (lis)
    (cond
      ((null? lis) lis)
      ((null? (cdr lis)) '())
      (else (cadr lis)))))

;returns the third element of a list (second operator)
(define second_operand
  (lambda (lis)
    (cond
      ((null? lis?) lis)
      ((null? (cdr lis)) '())
      ((null? (cddr lis)) '())
      (else (caddr lis)))))