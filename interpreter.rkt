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

;returns the state of an if-then statement
(define Mstate_if
  (lambda (condition then else state)
    (if (Mboolean condition s)
        (Mstate_stmt then s)
        (Mstate_stmt else s))))

;returns the value of the condition with the state s
(define Mboolean
  (lambda (condition s)))

;returns the state of a list of statements
(define Mstate_stmt_list
  (lambda (slist s)
    (if (null? slist)
        s
        (Mstate_stmt_list (cdr slist) (Mstate (car slist) s)))))