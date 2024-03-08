;Anurag Upadhya
;Case ID: axu60

#lang racket

(require "simpleParser.rkt")

;interpret takes a syntax tree and an empty state and calls interpret-statements
(define interpret
  (lambda (filename)
    (interpret-statements (parser filename) '())))

;interpret-statements takes a statement list and calls functions depending on the type of statement
(define interpret-statements
  (lambda (statements state)
    (cond ((null? statements) state)
          (else
           (cond
             ((eq? 'return (caar statements)) (interpret-return (car statements) state)) ;if statement is a return statement
             ((eq? 'var (caar statements)) (interpret-var statements state)) ;if statement starts with var
             ((eq? '= (caar statements)) (interpret-assignment statements state)) ;if statement is an assign statement
             ((eq? 'if (caar statements)) (interpret-if statements state)) ;if statement is an if statement
             ((eq? 'while (caar statements)) (interpret-while statements state)) ;if statement is a while statement
             (else state)))))) ;if none of the above, then just return the state list

;bind-variable binds the given variable to the given value and adds it to the state list
(define bind-variable
  (lambda (variable value state)
    (cons (list variable value) state)))

;eval-math-expr evaluates mathematical statements and returns the output of the statements
(define eval-math-expr
  (lambda (statements state)
    (cond
      ((number? statements) statements)
      ((not (list? statements)) (lookup-variable statements state))
      (else (cond
              ((eq? '+ (car statements)) (+ (eval-math-expr (cadr statements) state) (eval-math-expr (caddr statements) state)))
              ((eq? '- (car statements)) (- (eval-math-expr (cadr statements) state) (eval-math-expr (caddr statements) state)))
              ((eq? '* (car statements)) (* (eval-math-expr (cadr statements) state) (eval-math-expr (caddr statements) state)))
              ((eq? '/ (car statements)) (/ (eval-math-expr (cadr statements) state) (eval-math-expr (caddr statements) state)))
              ((eq? '% (car statements)) (modulo (eval-math-expr (cadr statements) state) (eval-math-expr (caddr statements) state)))
              (else (interpret-statements statements state)))))))

;eval-boolean-expr evaluates boolean statements and returns true or false
(define eval-boolean-expr
  (lambda (statement state)
    (cond
      ((eq? '== (car statement)) (if (eq? (eval-math-expr (cadr statement) state) (eval-math-expr (caddr statement) state))
                                            #t
                                            #f))
      ((eq? '!= (car statement)) (if (eq? (eval-math-expr (cadr statement) state) (eval-math-expr (caddr statement) state))
                                            #f
                                            #t))
      ((eq? '< (car statement)) (if (< (eval-math-expr (cadr statement) state) (eval-math-expr (caddr statement) state))
                                            #t
                                            #f))
      ((eq? '> (car statement)) (if (> (eval-math-expr (cadr statement) state) (eval-math-expr (caddr statement) state))
                                            #t
                                            #f))
      ((eq? '<= (car statement)) (if (<= (eval-math-expr (cadr statement) state) (eval-math-expr (caddr statement) state))
                                            #t
                                            #f))
      ((eq? '>= (car statement)) (if (>= (eval-math-expr (cadr statement) state) (eval-math-expr (caddr statement) state))
                                            #t
                                            #f))
      ((eq? '== (car statement)) (if (eq? (eval-math-expr (cadr statement) state) (eval-math-expr (caddr statement) state))
                                            #t
                                            #f))
      ((eq? '|| (car statement)) (if (or (eval-boolean-expr (cadr statement) state) (eval-boolean-expr (caddr statement) state))
                                            #t
                                            #f))
      ((eq? '&& (car statement)) (if (and (eval-boolean-expr (cadr statement) state) (eval-boolean-expr (caddr statement) state))
                                            #t
                                            #f))
      ((eq? '! (car statement)) (if (eval-boolean-expr (cdr statement) state)
                                           #f
                                           #t))
      (else (error 'interpret "Invalid comparison operator")))))
             
;interpret-var assigns the value to variable, if no value is assigned then it is automatically assigned 0
(define interpret-var
  (lambda (statements state)
    (if (null? (cddar statements))
        (interpret-statements (cdr statements) (bind-variable (cadar statements) 0 state))
        (interpret-statements (cdr statements) (bind-variable (cadar statements) (eval-math-expr (caddar statements) state) state)))))

;interpret-assignment binds a new value to a variable
(define interpret-assignment
  (lambda (statements state)
    (interpret-statements (cdr statements) (bind-variable (cadar statements) (eval-math-expr (caddar statements) state) state))))

;interpret-if checks the if condition. If true, it iterates once over the body, else it moves to the next if-else statement, if not present it either iterates once over the else loop if present, else it skips the loop 
(define interpret-if
  (lambda (statements state)
    (cond
      ((eq? (eval-boolean-expr (cadar statements) state) #t) (interpret-statements (cdr statements) (eval-math-expr (list (caddar statements)) state)))
      ((not (null? (cdddar statements))) (interpret-statements (cdr statements) (interpret-statements (cdddar statements) state)))
      (else (interpret-statements (cdr statements) state)))))

;interpret-while checks the while condition. If true it iterates over the body until the condition is false, if false it skips the body of the loop
(define interpret-while
  (lambda (statements state)
    (if (eq? (eval-boolean-expr (cadar statements) state) #t)
        (interpret-statements statements (eval-math-expr (cddar statements) state))
        (interpret-statements (cdr statements) state))))

;interpret-return returns the return value
(define interpret-return
  (lambda (statement state)
    (if (number? (cadr statement))
        (cadr statement)
        (eval-math-expr (cadr statement) state))))

;lookup-variable is used to lookup the value of a variable, it throws an error if the variable is not found in the state list
(define lookup-variable
  (lambda (variable state)
    (cond
      ((null? state) (error 'interpret (format "Variable ~a not declared" variable)))
      ((eq? (caar state) variable) (cadar state))
      (else (lookup-variable variable (cdr state))))))
