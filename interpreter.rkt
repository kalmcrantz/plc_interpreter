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
    (add_specific_layer (add_binding_in_layer name value (list (top_name_list s) (top_value_list s))) (remove_layer s))))

;updates the variable with the value
(define update_binding
  (lambda (variable value s)
    (cond
      ((or (null? value) (null? s)) (raise 'No-given-variable))
      ((variable_in_top_layer? variable s) (add_specific_layer (update_binding_in_layer variable value (list (top_name_list s) (top_value_list s))) (remove_layer s)))
      (else (add_specific_layer (list (top_name_list s) (top_value_list s)) (remove_layer s))))))
       
(define update_binding_in_layer
  (lambda (variable value layer)
    (cond
      ((null? layer) (raise 'Variable-not-declared))
      ((eq? (car (name_list layer)) variable) (list (name_list layer) (cons value (cdr (value_list layer)))))
      (else (add_binding_in_layer (car (name_list layer)) (car (value_list layer)) (update_binding_in_layer variable value (list (cdr (name_list layer)) (cdr (value_list layer)))))))))

(define add_binding_in_layer
  (lambda (variable value layer)
    (if (null? layer)
        (list (list variable) (list value))
        (list (cons variable (name_list layer)) (cons value (value_list layer))))))

(define variable_in_top_layer?
  (lambda (variable s)
    (cond
      ((or (null? s) (null? (top_name_list s))) #f)
      (else (variable_declared? variable (top_name_list s))))))

;adds a layer to the state
(define add_empty_layer
  (lambda (s)
    (cons (cons '() (name_list s)) (cons (cons '() (value_list s)) '()))))

(define add_specific_layer
  (lambda (layer s)
    (if (null? s)
        (list (cons (name_list layer) '()) (cons (value_list layer) '()))
        (list (cons (name_list layer) (name_list s)) (cons (value_list layer) (value_list s))))))

;removes the top layer of the state
(define remove_layer
  (lambda (s)
    (if (null? s)
        s
        (list (cdr (car s)) (cdr (car (cdr s)))))))

;gets the name list of the top layer
(define top_name_list
  (lambda (s)
    (car (car s))))

;gets the value list of the top layer
(define top_value_list
  (lambda (s)
    (car (car (cdr s)))))
     
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