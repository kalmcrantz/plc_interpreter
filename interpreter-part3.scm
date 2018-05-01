; If you are using racket instead of scheme, uncomment these two lines, comment the (load "simpleParser.scm") and uncomment the (require "simpleParser.scm")
 #lang racket
 (require "classParser.scm")
;(load "simpleParser.scm")


; An interpreter for the simple language that uses call/cc for the continuations.  Does not handle side effects.
(define call/cc call-with-current-continuation)


; The functions that start interpret-...  all return the current environment.
; The functions that start eval-...  all return a value

; The main function.  Calls parser to get the parse tree and interprets it with a new environment.  The returned value is in the environment.
(define interpret
  (lambda (file class)
    (scheme->language
     (call/cc
      (lambda (return)
        (interpret-function '(funcall main) (create-class-layer (parser file)) class class return
                                  (lambda (env) (myerror "Break used outside of loop")) (lambda (env) (myerror "Continue used outside of loop"))
                                  (lambda (v env) (myerror "Uncaught exception thrown"))))))))

; interprets a list of statements.  The environment from each statement is used for the next ones.
(define interpret-statement-list
  (lambda (statement-list environment class this return break continue throw)
    (if (null? statement-list)
        environment
        (interpret-statement-list (cdr statement-list) (interpret-statement (car statement-list) environment class this return break continue throw) class this return break continue throw))))

; interpret a statement in the environment with continuations for return, break, continue, throw
(define interpret-statement
  (lambda (statement environment class this return break continue throw)
    (cond
      ((eq? 'function (statement-type statement)) (interpret-function-declaration statement environment class this))
      ((eq? 'static-function (statement-type statement)) (interpret-function-declaration statement environment class this))
      ((eq? 'class (statement-type statement)) (interpret-class-declaration statement environment class this))
      ((eq? 'funcall (statement-type statement)) (interpret-function-no-return statement environment class this return break continue throw))
      ((eq? 'return (statement-type statement)) (interpret-return statement environment class this return throw))
      ((eq? 'var (statement-type statement)) (interpret-declare statement environment class this throw))
      ((eq? '= (statement-type statement)) (interpret-assign statement environment class this throw))
      ((eq? 'if (statement-type statement)) (interpret-if statement environment class this return break continue throw))
      ((eq? 'while (statement-type statement)) (interpret-while statement environment class this return throw))
      ((eq? 'continue (statement-type statement)) (continue environment class this))
      ((eq? 'break (statement-type statement)) (break environment class this))
      ((eq? 'begin (statement-type statement)) (interpret-block statement environment class this return break continue throw))
      ((eq? 'throw (statement-type statement)) (interpret-throw statement environment class this throw))
      ((eq? 'try (statement-type statement)) (interpret-try statement environment class this return break continue throw))
      (else (myerror "Unknown statement:" (statement-type statement))))))

(define get-class-from-instance
  (lambda (instance environment this)
    (cond
      ((eq? 'this instance) (get-class-from-instance this environment this))
      (else (car (lookup-variable instance environment))))))

(define interpret-dot-variable
  (lambda (statement environment class this)
    (cond
      ((eq? 'this (cadr statement)) (get-variable-value-from-instance this (operand2 statement) environment this))
      ((eq? 'super (cadr statement)) (get-variable-value-from-instance this (operand2 statement) environment this))
      ((list? (cadr statement)) (get-variable-value-from-class (cadr (cadr statement)) (caddr statement) environment))
      (else (get-variable-value-from-instance (operand1 statement) (operand2 statement) environment this)))))

(define get-variable-value-from-class
  (lambda (class variable environment)
    (find-value-from-variable variable (car (cadr (get-class-closure-from-class class environment))) (cadr (cadr (get-class-closure-from-class class environment))))))

(define find-value-from-variable
  (lambda (variable variables values)
    (cond
      ((null? variables) (myerror "error: variable does not exist:" variable))
      ((eq? (car variables) variable) (car values))
      (else (find-value-from-variable variable (cdr variables) (cdr values))))))
    

(define get-variable-value-from-instance
  (lambda (instance variable environment this)
    (unbox (retrieve-index (get-variable-index variable (get-class-from-instance instance environment this) environment this)
                           (get-variable-values-from-closure (get-instance-closure instance environment this))))))

(define retrieve-index
  (lambda (index lis)
    (cond
      ((zero? index) (car lis))
      ((null? lis) (raise "Retrive index error"))
      (else (retrieve-index (- index 1) (cdr lis))))))

(define get-variable-index
  (lambda (variable class environment this)
    (find-index-in-list variable (car (cadr (lookup class environment class this))))))

(define find-index-in-list
  (lambda (variable lis)
    (cond
      ((null? lis) (raise "Variable not in list"))
      ((eq? (car lis) variable) 0)
      (else (+ 1 (find-index-in-list variable (cdr lis)))))))

(define get-instance-closure
  (lambda (instance environment this)
    (cond
      ((eq? 'this instance) (lookup-variable this environment))
      (else (lookup-variable instance environment)))))

(define get-variable-values-from-closure
  (lambda (closure)
    (cadr closure)))
    
;creates a new instance of a class
(define interpret-instance-declaration
  (lambda (className environment class this)
    (create-instance-closure className environment class this)))

;
(define create-instance-closure
  (lambda (className environment class this)
    (cons className (cdr (cadr (lookup className environment class this))))))

;returns the state after a class is declared
(define interpret-class-declaration
  (lambda (statement environment class this)
    (insert-in-base-layer (get-declare-var statement) (create-class-closure statement environment class this) environment)))

;creates a class closure
;looks like: '((parent class) (instance fields of class) ((function names of class) (function closures))
(define create-class-closure
  (lambda (statement environment class this)
    (cons (get-parent statement) (cons (create-instance-fields statement class this) (create-function-list statement class this)))))

;gets the parent of a class
(define get-parent
  (lambda (statement)
    (operand2 statement)))

;gets the list of instance fields of a class
(define create-instance-fields
  (lambda (statement class this)
    (create-fields-from-frame (topframe (interpret-statement-list (get-class-body statement) (newenvironment) class this 'a 'b 'c 'd)))))

(define create-fields-from-frame
  (lambda (frame)
    (list (create-field-variables-from-frame frame) (create-field-values-from-frame frame))))

;helper for above method
(define create-field-variables-from-frame
  (lambda (frame)
    (cond
      ((null? (store frame)) '())
      ((list? (lookup-in-frame (car (variables frame)) frame)) (create-field-variables-from-frame (remove-top-binding frame)))
      (else (cons (car (variables frame)) (create-field-variables-from-frame (remove-top-binding frame)))))))

(define create-field-values-from-frame
  (lambda (frame)
    (cond
      ((null? (store frame)) '())
      ((list? (lookup-in-frame (car (variables frame)) frame)) (create-field-values-from-frame (remove-top-binding frame)))
      (else (cons (car (store frame)) (create-field-values-from-frame (remove-top-binding frame)))))))

;gets the instance of the left hand side of a dot operator
(define getInstance
  (lambda (statement environment class this)
    (cond
      ((eq? 'new (operator statement)) (statement statement))
      (else statement))))

;haha I have no idea what im doing but maybe this one works
(define getInstance2
  (lambda (statement environment class this)
    (cond
      ((eq? (operator statement) 'funcall) (dotFunc statement environment class this))
      (else (lookup statement environment this)))))

(define dotFunc
  (lambda (statement environment class this)
    (cond
      ((eq? (operator (operand1 statement)) 'new) ((cadr interpret-instance-declaration (cadddr statement))))
      ((eq? (operand1 statement) 'this) (lookup (operand2 statement) (car (caddr (lookup (car this) environment this))) (cadr this)))
      (else (lookup (operand2 statement) (car (caddr (lookup (car (lookup (operand1 statement) environment this)) environment this)))
                    (cadr (lookup (operand1 statement) environment this)))))))

;removes the first binding in a frame
; Ex: ((a b) (1 2)) --> ((b) (2))
(define remove-top-binding
  (lambda (frame)
    (if (null? (variables frame))
        frame
        (list (cdr (variables frame)) (cdr (store frame))))))

;returns a list of functions and their closures in the form ((function names) (function closures) 
(define create-function-list
  (lambda (statement class this)
    (create-functions-from-frame (topframe (interpret-statement-list (get-class-body statement) (newenvironment) class this 'a 'b 'c 'd))
                              (lambda (a b) (cons (list a b) '())))))

;helper for above function
(define create-functions-from-frame
  (lambda (frame return)
    (cond
      ((null? (store frame)) (return '() '()))
      ((list? (lookup-in-frame (car (variables frame)) frame))
       (create-functions-from-frame (remove-top-binding frame) (lambda (a b) (return (cons (car (variables frame)) a) (cons (car (store frame)) b)))))
      (else (create-functions-from-frame (remove-top-binding frame) return)))))

;returns the class body of a class declaration statement
(define get-class-body
  (lambda (statement)
    (operand3 statement)))

; creates the class layer for a program to hold class definitions
(define create-class-layer
  (lambda (stmt-list)
    (interpret-statement-list stmt-list (newenvironment)
                              (lambda (env) (myerror "Return used outside of method")) (lambda (env) (myerror "Class used outside of method"))
                              (lambda (env) (myerror "This used outside of method")) (lambda (env) (myerror "Break used outside of loop"))
                              (lambda (env) (myerror "Continue used outside of loop")) (lambda (v env) (myerror "Uncaught exception thrown")))))

; returns the value of a function
; statement: (funcall function_name parameters)
(define interpret-function
  (lambda (statement environment class this return break continue throw)
    (car (call/cc
     (lambda (return1)
       (if (list? (operand1 statement))
           (interpret-function-dot statement environment class this return1 break continue throw)
           (interpret-statement-list (cadr (lookup-in-frame (operand1 statement) (caddr (lookup class environment class this))))
                                 (get-function-environment statement class this environment throw)
                                 class this return1 break continue throw)))))))

;statement: (funcall (dot instance_name function_name) parameters)
(define interpret-function-dot
  (lambda (statement environment class this return break continue throw)
    (interpret-statement-list (get-function-body-from-call statement class this environment throw)
                              (get-function-environment (cons (car statement) (cons (caddr (cadr statement)) (cddr statement))) class this environment throw)
                              class (pick-instance-or-this (cadr statement) this) return break continue throw)))

(define pick-instance-or-this
  (lambda (statement this)
    (if (or (eq? 'this (cadr statement)) (eq? 'super (cadr statement)))
        this
        (cadr statement))))

; returns the state of a function (needed if the function is called but want to ignore return)
(define interpret-function-no-return
  (lambda (statement environment class this return break continue throw)
    (cdr (call/cc
     (lambda (return1)
       (if (list? (operand1 statement))
           (interpret-function-dot statement environment class this return1 break continue throw)
           (interpret-statement-list (cadr (lookup (operand1 statement) environment)) (get-function-environment statement environment throw)
                                 return1 break continue throw)))))))

;statement: (funcall (dot a/super/this/new f) 3 5) = a.f(3, 5)
(define get-function-body-from-call
  (lambda (statement class this environment throw)
    (cond
      ((eq? 'this (cadr (cadr statement))) (get-function-body-from-class (caddr (cadr statement)) (get-class-from-instance this environment this) this environment))
      ((eq? 'super (cadr (cadr statement))) (get-function-body-from-class (caddr (cadr statement)) (get-class-from-instance this environment this) this environment))
      ((list? (cadr (cadr statement))) (get-function-body-from-class (caddr (cadr statement)) (cadr (cadr (cadr statement))) this environment))
      (else (get-function-body-from-class (caddr (cadr statement)) class this environment)))))


(define get-function-body-from-class
  (lambda (function class this environment)
    (cadr (lookup-in-frame function (get-function-list-from-class class this environment)))))

(define get-function-parameters-from-class-closure
  (lambda (statement class this environment)
    (car (lookup-in-frame (operand1 statement) (get-function-list-from-class class this environment)))))

(define get-function-list-from-class
  (lambda (class this environment)
    (caddr (lookup class environment class this))))

(define get-function-names-from-class
  (lambda (class this environment)
    (car (get-function-list-from-class class this environment))))

(define get-class-closure-from-class
  (lambda (class environment)
    (lookup-variable class environment)))

(define get-variables-in-class
  (lambda (class environment)
    (car (cadr (get-class-closure-from-class class environment)))))

; get the formal parameters for a function
(define get-formal-parameters
  (lambda (statement environment class this)
    (car (lookup (operand1 statement) environment class this))))

; get the actual parameters from a function call
(define get-actual-parameters
  (lambda (statement)
    (cddr statement)))

; returns the environment for a function by doing the following in order:
; 1. finds what's in scope by determining how many layers deep the scope is
; 2. adds a layer to the scope
; 3. binds the formal parameters to the actual parameters and adds the bindings to the scope
(define get-function-environment
  (lambda (statement class this environment throw)
    (add-bindings (get-function-parameters-from-class-closure statement class this environment) (get-actual-parameters statement)
                  (add-functions-from-class-to-env class class this
                        (create-environment environment (get-function-layer-num-from-class-closure statement class this environment))) environment class this throw)))

(define add-functions-from-class-to-env
  (lambda (className class this environment)
    (cons (get-function-list-from-class className this environment) environment)))
  
(define get-function-layer-num-from-class-closure
  (lambda (statement class this environment)
    (caddr (lookup-in-frame (operand1 statement) (get-function-list-from-class class this environment)))))

; adds the bindings of the formal parameters to the actual parameters and adds them to the new environment
; oldenvironment is used to evaluate expressions in the actual parameters
(define add-bindings
  (lambda (formal actual newenvironment oldenvironment class this throw)
    (cond
      ((and (null? formal) (null? actual)) newenvironment)
      ((or (null? formal) (null? actual)) (myerror "Invalid function call"))
      (else (add-bindings (cdr formal) (cdr actual) (insert (car formal) (eval-expression (car actual) oldenvironment class this throw) newenvironment) oldenvironment class this throw)))))

; adds the function definition to the environment
; TODO: This will need to be changed so that its inserted in the class and not the base layer
(define interpret-function-declaration
  (lambda (statement environment class this)
    (insert-in-base-layer (get-declare-var statement) (create-function-closure statement environment) environment)))

; returns the number of layers in a function is declared
; function a() {
;    function b() {
;    }
; }
; function a is 1 layer in
; function b is 2 layers in
(define layers-in
  (lambda (environment statement)
    (cond
      ((eq? 'main (operand1 statement)) 1)
      ((null? environment) 1)
      (else (+ 1 (layers-in (pop-frame environment) statement))))))

; creates a closure for a function
; 1st element: formal parameters
; 2nd element: body of function
; 3rd element: how many layers in the environment this function declaration is in (needed to find the function environment)
(define create-function-closure
  (lambda (statement environment)
    (cons (operand2 statement) (cons (operand3 statement) (cons (layers-in environment statement) '())))))

; creates the environment for a function by returning the specified number of layers
; ex: if the environment had 5 layers, it would be represented as ((FIFTH LAYER) (FOURTH) (THIRD) (SECOND) (FIRST))
; this method with layers = 3, would return ((THIRD) (SECOND) (FIRST))
(define create-environment
  (lambda (environment layers)
    (if (zero? layers)
        '()
        (myappend (create-environment (myreverse (remainingframes (myreverse environment))) (- layers 1)) (cons (topframe (myreverse environment)) '())))))

(define myappend
  (lambda (l1 l2)
    (if (null? l1)
        l2
        (cons (car l1) (myappend (cdr l1) l2)))))

(define myreverse
  (lambda (lis)
    (if (null? lis)
        lis
        (myappend (myreverse (cdr lis)) (cons (car lis) '())))))

; Calls the return continuation with the given expression value
(define interpret-return
  (lambda (statement environment class this return throw)
    (return (cons (eval-expression (get-expr statement) environment class this throw) environment))))

; Adds a new variable binding to the environment.  There may be an assignment with the variable
(define interpret-declare
  (lambda (statement environment class this throw)
    (if (exists-declare-value? statement)
        (insert (get-declare-var statement) (eval-expression (get-declare-value statement) environment class this throw) environment)
        (insert (get-declare-var statement) 'novalue environment))))

; Updates the environment to add an new binding for a variable
(define interpret-assign
  (lambda (statement environment class this throw)
    (update (get-assign-lhs statement) (eval-expression (get-assign-rhs statement) environment class this throw) environment this)))

; We need to check if there is an else condition.  Otherwise, we evaluate the expression and do the right thing.
(define interpret-if
  (lambda (statement environment class this return break continue throw)
    (cond
      ((eval-expression (get-condition statement) environment class this throw) (interpret-statement (get-then statement) environment class this return break continue throw))
      ((exists-else? statement) (interpret-statement (get-else statement) environment class this return break continue throw))
      (else environment))))

; Interprets a while loop.  We must create break and continue continuations for this loop
(define interpret-while
  (lambda (statement environment class this return throw)
    (call/cc
     (lambda (break)
       (letrec ((loop (lambda (condition body environment)
                        (if (eval-expression condition environment class this throw)
                            (loop condition body (interpret-statement body environment class this return break (lambda (env) (break (loop condition body env))) throw))
                         environment))))
         (loop (get-condition statement) (get-body statement) environment))))))

; Interprets a block.  The break, continue, and throw continuations must be adjusted to pop the environment
(define interpret-block
  (lambda (statement environment class this return break continue throw)
    (pop-frame (interpret-statement-list (cdr statement)
                                         (push-frame environment)
                                         class
                                         this
                                         return
                                         (lambda (env) (break (pop-frame env)))
                                         (lambda (env) (continue (pop-frame env)))
                                         (lambda (v env) (throw v (pop-frame env)))))))

; We use a continuation to throw the proper value. Because we are not using boxes, the environment/state must be thrown as well so any environment changes will be kept
(define interpret-throw
  (lambda (statement environment class this throw)
    (throw (eval-expression (get-expr statement) environment class this throw) environment)))

; Interpret a try-catch-finally block

; Create a continuation for the throw.  If there is no catch, it has to interpret the finally block, and once that completes throw the exception.
;   Otherwise, it interprets the catch block with the exception bound to the thrown value and interprets the finally block when the catch is done
(define create-throw-catch-continuation
  (lambda (catch-statement environment class this return break continue throw jump finally-block)
    (cond
      ((null? catch-statement) (lambda (ex env) (throw ex (interpret-block finally-block env class this return break continue throw)))) 
      ((not (eq? 'catch (statement-type catch-statement))) (myerror "Incorrect catch statement"))
      (else (lambda (ex env)
              (jump (interpret-block finally-block
                                     (pop-frame (interpret-statement-list 
                                                 (get-body catch-statement) 
                                                 (insert (catch-var catch-statement) ex (push-frame env))
                                                 class
                                                 this
                                                 return 
                                                 (lambda (env2) (break (pop-frame env2))) 
                                                 (lambda (env2) (continue (pop-frame env2))) 
                                                 (lambda (v env2) (throw v (pop-frame env2)))))
                                     return break continue throw)))))))

; To interpret a try block, we must adjust  the return, break, continue continuations to interpret the finally block if any of them are used.
;  We must create a new throw continuation and then interpret the try block with the new continuations followed by the finally block with the old continuations
(define interpret-try
  (lambda (statement environment class this return break continue throw)
    (call/cc
     (lambda (jump)
       (let* ((finally-block (make-finally-block (get-finally statement)))
              (try-block (make-try-block (get-try statement)))
              (new-return (lambda (v) (begin (interpret-block finally-block environment return break continue throw) (return v))))
              (new-break (lambda (env) (break (interpret-block finally-block env return break continue throw))))
              (new-continue (lambda (env) (continue (interpret-block finally-block env return break continue throw))))
              (new-throw (create-throw-catch-continuation (get-catch statement) environment return break continue throw jump finally-block)))
         (interpret-block finally-block
                          (interpret-block try-block environment new-return new-break new-continue new-throw)
                          return break continue throw))))))

; helper methods so that I can reuse the interpret-block method on the try and finally blocks
(define make-try-block
  (lambda (try-statement)
    (cons 'begin try-statement)))

(define make-finally-block
  (lambda (finally-statement)
    (cond
      ((null? finally-statement) '(begin))
      ((not (eq? (statement-type finally-statement) 'finally)) (myerror "Incorrectly formatted finally block"))
      (else (cons 'begin (cadr finally-statement))))))

; Evaluates all possible boolean and arithmetic expressions, including constants and variables.
(define eval-expression
  (lambda (expr environment class this throw)
    (cond
      ((number? expr) expr)
      ((eq? expr 'true) #t)
      ((eq? expr 'false) #f)
      ((not (list? expr)) (lookup expr environment class this))
      (else (eval-operator expr environment class this throw)))))

; Evaluate a binary (or unary) operator.  Although this is not dealing with side effects, I have the routine evaluate the left operand first and then
; pass the result to eval-binary-op2 to evaluate the right operand.  This forces the operands to be evaluated in the proper order in case you choose
; to add side effects to the interpreter
(define eval-operator
  (lambda (expr environment class this throw)
    (cond
      ((eq? 'new (statement-type expr)) (interpret-instance-declaration (cadr expr) environment class this))
      ((eq? 'dot (statement-type expr)) (interpret-dot-variable expr environment class this))
      ((eq? '! (operator expr)) (not (eval-expression (operand1 expr) environment class this throw)))
      ((eq? 'funcall (operator expr)) (call/cc
                                       (lambda (return)
                                         (interpret-function expr environment class this return (lambda (v) (myerror "cannot break here"))
                                                             (lambda (v) (myerror "cannot continue here")) throw))))
      ((and (eq? '- (operator expr)) (= 2 (length expr))) (- (eval-expression (operand1 expr) environment class this throw)))
      (else (eval-binary-op2 expr (eval-expression (operand1 expr) environment class this throw) environment class this throw)))))
;(lambda (v) (myerror "cannot throw here"))

; Complete the evaluation of the binary operator by evaluating the second operand and performing the operation.
(define eval-binary-op2
  (lambda (expr op1value environment class this throw)
    (cond
      ((eq? '+ (operator expr)) (+ op1value (eval-expression (operand2 expr) environment class this throw)))
      ((eq? '- (operator expr)) (- op1value (eval-expression (operand2 expr) environment class this throw)))
      ((eq? '* (operator expr)) (* op1value (eval-expression (operand2 expr) environment class this throw)))
      ((eq? '/ (operator expr)) (quotient op1value (eval-expression (operand2 expr) class this environment throw)))
      ((eq? '% (operator expr)) (remainder op1value (eval-expression (operand2 expr) class this environment throw)))
      ((eq? '== (operator expr)) (isequal op1value (eval-expression (operand2 expr) class this environment throw)))
      ((eq? '!= (operator expr)) (not (isequal op1value (eval-expression (operand2 expr) class this environment throw))))
      ((eq? '< (operator expr)) (< op1value (eval-expression (operand2 expr) class this environment throw)))
      ((eq? '> (operator expr)) (> op1value (eval-expression (operand2 expr) class this environment throw)))
      ((eq? '<= (operator expr)) (<= op1value (eval-expression (operand2 expr) class this environment throw)))
      ((eq? '>= (operator expr)) (>= op1value (eval-expression (operand2 expr) class this environment throw)))
      ((eq? '|| (operator expr)) (or op1value (eval-expression (operand2 expr) class this environment throw)))
      ((eq? '&& (operator expr)) (and op1value (eval-expression (operand2 expr) class this environment throw)))
      (else (myerror "Unknown operator:" (operator expr))))))

; Determines if two values are equal.  We need a special test because there are both boolean and integer types.
(define isequal
  (lambda (val1 val2)
    (if (and (number? val1) (number? val2))
        (= val1 val2)
        (eq? val1 val2))))


;-----------------
; HELPER FUNCTIONS
;-----------------

; These helper functions define the operator and operands of a value expression
(define operator car)
(define operand1 cadr)
(define operand2 caddr)
(define operand3 cadddr)

(define exists-operand2?
  (lambda (statement)
    (not (null? (cddr statement)))))

(define exists-operand3?
  (lambda (statement)
    (not (null? (cdddr statement)))))

; these helper functions define the parts of the various statement types
(define statement-type operator)
(define get-expr operand1)
(define get-declare-var operand1)
(define get-declare-value operand2)
(define exists-declare-value? exists-operand2?)
(define get-assign-lhs operand1)
(define get-assign-rhs operand2)
(define get-condition operand1)
(define get-then operand2)
(define get-else operand3)
(define get-body operand2)
(define exists-else? exists-operand3?)
(define get-try operand1)
(define get-catch operand2)
(define get-finally operand3)

(define catch-var
  (lambda (catch-statement)
    (car (operand1 catch-statement))))


;------------------------
; Environment/State Functions
;------------------------

; create a new empty environment
(define newenvironment
  (lambda ()
    (list (newframe))))

; create an empty frame: a frame is two lists, the first are the variables and the second is the "store" of values
(define newframe
  (lambda ()
    '(() ())))

; add a frame onto the top of the environment
(define push-frame
  (lambda (environment)
    (cons (newframe) environment)))

; remove a frame from the environment
(define pop-frame
  (lambda (environment)
    (cdr environment)))

; some abstractions
(define topframe car)
(define remainingframes cdr)

; does a variable exist in the environment?
(define exists?
  (lambda (var environment)
    (cond
      ((null? environment) #f)
      ((exists-in-list? var (variables (topframe environment))) #t)
      (else (exists? var (remainingframes environment))))))

; does a variable exist in a list?
(define exists-in-list?
  (lambda (var l)
    (cond
      ((null? l) #f)
      ((eq? var (car l)) #t)
      (else (exists-in-list? var (cdr l))))))

; Looks up a value in the environment.  If the value is a boolean, it converts our languages boolean type to a Scheme boolean type
(define lookup
  (lambda (var environment class this)
    (lookup-variable var environment)))
  
; A helper function that does the lookup.  Returns an error if the variable does not have a legal value
(define lookup-variable
  (lambda (var environment)
    (let ((value (lookup-in-env var environment)))
      (if (eq? 'novalue value)
          (myerror "error: variable without an assigned value:" var)
          value))))

; Return the value bound to a variable in the environment
(define lookup-in-env
  (lambda (var environment)
    (cond
      ((null? environment) (myerror "error: undefined variable" var))
      ((exists-in-list? var (variables (topframe environment))) (lookup-in-frame var (topframe environment)))
      (else (lookup-in-env var (cdr environment))))))

; Return the value bound to a variable in the frame
(define lookup-in-frame
  (lambda (var frame)
    (cond
      ((not (exists-in-list? var (variables frame))) (myerror "error: undefined variable" var))
      (else (language->scheme (get-value (indexof var (variables frame)) (store frame)))))))

; Get the location of a name in a list of names
(define indexof
  (lambda (var l)
    (cond
      ((null? l) 0)  ; should not happen
      ((eq? var (car l)) 0)
      (else (+ 1 (indexof var (cdr l)))))))

; Get the value stored at a given index in the list
(define get-value
  (lambda (n l)
    (cond
      ((and (zero? n) (box? (car l))) (unbox (car l)))
      ((zero? n) (car l))
      (else (get-value (- n 1) (cdr l))))))

; Adds a new variable/value binding pair into the environment.  Gives an error if the variable already exists in this frame.
(define insert
  (lambda (var val environment)
    (if (exists-in-list? var (variables (car environment)))
        (myerror "error: variable is being re-declared:" var)
        (cons (add-to-frame var val (car environment)) (cdr environment)))))

(define insert-in-base-layer
  (lambda (var val environment)
    (cond
      ((and (null? (remainingframes environment)) (exists-in-list? var (variables (car environment)))) (myerror "error: variable is being re-declared:" var))
      ((null? (remainingframes environment)) (cons (add-to-frame var val (car environment)) (cdr environment)))
      (else (cons (topframe environment) (insert-in-base-layer var val (remainingframes environment)))))))

; Changes the binding of a variable to a new value in the environment.  Gives an error if the variable does not exist.
(define update
  (lambda (var val environment this)
    (cond
      ((list? var) (update-instance-field var val environment this))
      ((exists? var environment) (update-existing var val environment))
      (else (myerror "error: variable used but not defined:" var)))))

;var: (dot instance variable)
(define update-instance-field
  (lambda (var val environment this)
    (if (exists-in-list? (caddr var) (get-variables-in-class (get-class-from-instance (cadr var) environment this) environment))
        (update-instance-field-for-real (cadr var) (caddr var) val environment this)
        (myerror "error: variable not defined: " var))))

(define update-instance-field-for-real
  (lambda (instance variable value environment this)
    (cond
      ((null? environment) (myerror "instance does not exist: " instance))
      ((eq? 'this instance) (update-instance-field-for-real this variable value environment this))
      ((exists-in-list? instance (variables (car environment))) (cons (update-instance-field-in-frame instance value (topframe environment)
                                              (get-variable-index variable (get-class-from-instance instance environment this) environment instance)) (remainingframes environment)))
      (else (cons (topframe environment) (update-instance-field-for-real instance variable value (remainingframes environment) this))))))

(define update-instance-field-in-frame
  (lambda (instance value frame variable-index)
    (list (variables frame) (update-instance-field-in-frame-store instance value (variables frame) (store frame) variable-index))))

; needs to return the vallist, so a list of instance closures
; ((A ()) (B (1 2)) (C (4 5)))
(define update-instance-field-in-frame-store
  (lambda (instance value varlist vallist index)
    (cond
      ((eq? instance (car varlist)) (cons (update-field-in-closure instance value (unbox (car vallist)) index) (cdr vallist)))
      ((null? varlist) (myerror "variable not declared:" instance))
      (else (cons (car vallist) (update-instance-field-in-frame-store instance value (cdr varlist) (cdr vallist) index))))))

; needs to return the instance closure with the updated field
; looks like: (A (1 2))
(define update-field-in-closure
  (lambda (instance value closure index)
    (box (list (car closure) (update-field-in-value-list instance value (cadr closure) index)))))

; needs to return the value-list updated
(define update-field-in-value-list
  (lambda (instance value value-list index)
    (cond
      ((zero? index) (begin (set-box! (car value-list) value) value-list))
      (else (cons (car value-list) (update-field-in-value-list instance value (cdr value-list) (- 1 index)))))))
; Add a new variable/value pair to the frame.
(define add-to-frame
  (lambda (var val frame)
    (list (cons var (variables frame)) (cons (scheme->language (box val)) (store frame)))))

; Changes the binding of a variable in the environment to a new value
(define update-existing
  (lambda (var val environment)
    (if (exists-in-list? var (variables (car environment)))
        (cons (update-in-frame var val (topframe environment)) (remainingframes environment))
        (cons (topframe environment) (update-existing var val (remainingframes environment))))))
; Changes the binding of a variable in the frame to a new value.
(define update-in-frame
  (lambda (var val frame)
    (list (variables frame) (update-in-frame-store var val (variables frame) (store frame)))))

; Changes a variable binding by placing the new value in the appropriate place in the store
(define update-in-frame-store
  (lambda (var val varlist vallist)
    (cond
      ((eq? var (car varlist)) (begin (set-box! (car vallist) val) vallist))
      (else (cons (car vallist) (update-in-frame-store var val (cdr varlist) (cdr vallist)))))))

; Returns the list of variables from a frame
(define variables
  (lambda (frame)
    (car frame)))

; Returns the store from a frame
(define store
  (lambda (frame)
    (cadr frame)))

; Functions to convert the Scheme #t and #f to our languages true and false, and back.

(define language->scheme
  (lambda (v) 
    (cond 
      ((eq? v 'false) #f)
      ((eq? v 'true) #t)
      (else v))))

(define scheme->language
  (lambda (v)
    (cond
      ((eq? v #f) 'false)
      ((eq? v #t) 'true)
      (else v))))



; Because the error function is not defined in R5RS scheme, I create my own:
(define error-break (lambda (v) v))
(call-with-current-continuation (lambda (k) (set! error-break k)))

(define myerror
  (lambda (str . vals)
    (letrec ((makestr (lambda (str vals)
                        (if (null? vals)
                            str
                            (makestr (string-append str (string-append " " (symbol->string (car vals)))) (cdr vals))))))
      (error-break (display (string-append str (makestr "" vals)))))))