;Kimberly Almcrantz (kaa97) Jenny Zhao (sxz402)

#lang racket

(require "functionParser.scm")

(define evaluate
  (lambda (file)
    (if (list? (evaluateWrapper file))
        (raise 'Invalid-return)
        (evaluateWrapper file))))

;this is the method that should be called to interpret a file
(define evaluateWrapper
  (lambda (file)
    (call/cc
     (lambda (return)
       (call/cc
        (lambda (break)
         (call/cc
          (lambda (continue)
            (call/cc
             (lambda (throw)
              (Mstate_stmt_list (parser file) initial_state return continue break throw)))))))))))

;returns the state after a list of statements
(define Mstate_stmt_list
  (lambda (slist s return continue break throw)
       (if (null? slist)
        s
        (Mstate_stmt_list (rest-of slist) (Mstate_stmt (pop slist) s return continue break throw) return continue break throw))))

;returns the state that exists after the stmt is executed
(define Mstate_stmt
  (lambda (stmt s return continue break throw)
    (cond
      ((null? stmt) s)
      ((eq? 'function (operator stmt)) (Mstate_function_declaration (rest-of stmt) s))
      ((eq? 'return (operator stmt)) (return (Mstate_return stmt s return)))
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
      ((eq? 'try (operator stmt)) (Mstate_finally  (third_operand stmt) (call/cc (lambda (break) (Mstate_try_catch (first_operand stmt) (second_operand stmt) s return continue break throw))) return continue break throw))
      ((eq? 'throw (operator stmt)) (remove_layer (throw (Mstate_declare_and_assign 'temp (Mvalue (first_operand stmt) s) (add_empty_layer s)))))
      ((eq? 'begin (operator stmt))  (Mstate_begin (rest-of stmt) s return continue break throw))
      ((eq? 'break (operator stmt)) (break (remove_layer s)))
      ((eq? 'continue (operator stmt)) (continue (remove_layer s)))
      ((and (eq? 'if (operator stmt)) (null? (else-stmt stmt))) (Mstate_if (first_operand stmt) (second_operand stmt) null s return continue break throw))
      ((eq? 'if (operator stmt)) (Mstate_if (first_operand stmt) (second_operand stmt) (third_operand stmt) s return continue break throw))
      ((eq? 'while (operator stmt)) (Mstate_whileWrapper (first_operand stmt) (second_operand stmt) s return throw)) 
      ((eq? '= (operator stmt)) (Mstate_assign (first_operand stmt) (second_operand stmt) s))
      ((and (eq? 'var (operator stmt)) (null? (var-value stmt))) (Mstate_declare (first_operand stmt) s))
      ((eq? 'var (operator stmt)) (Mstate_declare_and_assign (first_operand stmt) (second_operand stmt) s)))))

(define Mstate_function_declaration
  (lambda (stmt s)
    (Mstate_declare_and_assign (function_name stmt) (make_closure stmt s) s)))

;creates a closure for a function
;list containing the variables in scope, the formal parameters, and the body of the function
(define make_closure
  (lambda (stmt s)
    (list (get_in_scope s) (list (function_formal_params stmt) (function_body stmt)))))

(define get_in_scope
  (lambda (s)
    s))

;body: the body of the try
;catch: a list in which the first element is the list of parameters for the catch/throw, and the second element is the body
(define Mstate_try_catch
  (lambda (body catch s return continue break throw)
    (cond
      ((null? body) (break s))
      (else (Mstate_catch catch (call/cc (lambda (throw1) (Mstate_try_catch (rest-of body) catch (Mstate_stmt (pop body) s return continue break throw1) return continue break throw1))) return continue break throw)))))

(define Mstate_catch
  (lambda (body s return continue break throw)
     (cond
      ((null? body) s)
      ((eq? 'throw (rest-of body)) (raise 'invalid-catch))
      (else (Mstate_stmt_list (second_operand body) (rename_variable 'temp (pop (first_operand body)) s) return continue break throw)))))

(define Mstate_finally
  (lambda (body s return continue break throw)
    (cond
    ((null? body) s)
    (else (Mstate_stmt_list (first_operand body) s return continue break throw)))))
      

;returns the state after a block of code enclosed in curly brackets
(define Mstate_begin
  (lambda (body s return continue break throw)
    (cond
      ((null? body) s)
      (else (remove_layer (Mstate_stmt_list body (add_empty_layer s) return continue break throw))))))

;returns the state of an if-then statement
(define Mstate_if
  (lambda (condition then else s return continue break throw)
    (if (Mvalue condition s)
        (Mstate_stmt then s return continue break throw)
        (Mstate_stmt else s return continue break throw))))

;returns the state of a while loop
(define Mstate_while 
  (lambda (condition body s return break throw)
         (if (Mvalue condition s)
             (Mstate_while condition body (call/cc (lambda (continue) (Mstate_stmt body s return continue break throw))) return break throw)
          s)))

(define Mstate_whileWrapper
  (lambda (condition body s return throw)
    (call/cc
     (lambda (break)
       (Mstate_while condition body s return break throw)))))

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
  (lambda (stmt s return)
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
      ((variable_in_top_layer? value s) (lookup_in_layer value (get_top_layer s)))
      (else (Mvalue_var value (temp_remove_layer s))))))

(define lookup_in_layer
  (lambda (value layer)
    (cond
      ((null? layer) (raise 'Variable-doesn't-exist))
      ((and (eq? value (car (name_list layer))) (null? (car (value_list layer)))) (raise 'Variable-not-assigned-a-value))
      ((eq? value (car (name_list layer)))(car (value_list layer)))
      (else (lookup_in_layer value (cons (cdr (name_list layer)) (cons (cdr (value_list layer)) '())))))))

;adds a binding to the state
;paramters: 'name' of binding and 'value' of binding
(define add_binding
  (lambda (name value s)
    (add_specific_layer (add_binding_in_layer name value (list (top_name_list s) (top_value_list s))) (temp_remove_layer s))))

;updates the variable with the value
(define update_binding
  (lambda (variable value s)
    (cond
      ((or (null? value) (null? s)) (raise 'No-given-variable))
      ((variable_in_top_layer? variable s) (add_specific_layer (update_binding_in_layer variable value (list (top_name_list s) (top_value_list s))) (temp_remove_layer s)))
      (else (add_specific_layer (list (top_name_list s) (top_value_list s)) (update_binding variable value (temp_remove_layer s)))))))
       
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

(define get_top_layer
  (lambda (s)
    (if (null? s)
        s
        (list (top_name_list s) (top_value_list s)))))

;removes the top layer of the state
(define remove_layer
  (lambda (s)
    (if (or (null? s) (null? (cdr (car s))))
        (raise 'Cannot-remove-layer)
        (list (cdr (car s)) (cdr (car (cdr s)))))))

(define temp_remove_layer
  (lambda (s)
    (if (or (null? s))
        '((())(()))
        (list (cdr (car s)) (cdr (car (cdr s)))))))

(define remove_first_binding
  (lambda (layer)
    (if (null? layer)
        layer
        (list (cdr (name_list layer)) (cdr (value_list layer))))))

;gets the name list of the top layer
(define top_name_list
  (lambda (s)
    (car (car s))))

;gets the value list of the top layer
(define top_value_list
  (lambda (s)
    (car (car (cdr s)))))

(define rename_variable
  (lambda (old_name new_name s)
    (cond
      ((or (null? s) (null? (name_list s)) (null? (top_name_list s))) '((())(())))
      ((variable_in_top_layer? old_name s) (add_specific_layer (rename_variable_in_layer old_name new_name (get_top_layer s)) (temp_remove_layer s)))
      (else (add_specific_layer (get_top_layer s) (rename_variable old_name new_name (temp_remove_layer s)))))))      

(define rename_variable_in_layer
  (lambda (old_name new_name layer)
    (cond
      ((or (null? layer) (null? (name_list layer))) '(()()))
      ((eq? old_name (car (name_list layer))) (list (cons new_name (cdr (name_list layer))) (value_list layer)))
      (else (add_binding_in_layer (car (name_list layer)) (car (value_list layer)) (rename_variable_in_layer old_name new_name (remove_first_binding layer)))))))
     
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

;value of variable
(define var-value cddr)
 
;else statement of if 
(define else-stmt cdddr)

;retrives rest of list
(define rest-of cdr)

;retrives first element from list
(define pop car)
  
;returns the first element of a list (operator)
(define operator car)

;returns the second element of a list (first operator)
(define first_operand cadr)

;returns the third element of a list (second operator)
(define second_operand caddr)

;returns the fourth element of a list (third operator)
(define third_operand cadddr)

;returns the inital state at the very beginning of a program
(define initial_state '((())(())))

(define function_name car)
(define closure_in_scope car)
(define function_formal_params cadr)
(define function_body caddr)
