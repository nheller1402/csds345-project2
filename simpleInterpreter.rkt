#lang racket
(require "simpleParser.rkt")

;;MAIN
;calls Mstate for the total state of the program on the parsed version of the file
(define interpret
  (lambda (filename)
    (M_state (parser filename) empty next)))

;;STATE FUNCTIONS
;abstractions for state functions
(define empty '(()()) )
(define current car) 
(define next cdr)

;stmts is a list of statements and state is a list of states
;when state becomes singular (return statement or end of file), it is returned
(define M_state
  (lambda (stmts state next)
    (cond
      [(null? stmts) state] ;no statements left (end of recursion)
      [(not (list? state)) state] ;state is singular (return statement/end of recursion)
      [(list? (current stmts)) (M_state (next stmts) (M_state (current stmts) state next) next)] ;current statement is more than one, split
      [(eq? (current stmts) 'var) (M_declare stmts state)]
      [(eq? (current stmts) '=) (M_assign stmts state)] 
      [(eq? (current stmts) 'return) (M_return stmts state)]
      [(eq? (current stmts) 'if) (M_if stmts state)]
      [(eq? (current stmts) 'while) (M_while stmts state next)]
      [else (error 'stmterror "Unknown Statement")])))

;variable abstractions
(define var_name cadr)
(define var_value caddr)
;declares a variable
(define M_declare
  (lambda (stmt state)
    (cond
      [(declared? (var_name stmt) (state_vars state)) error 'declareerror "Variable already declared"]
      [(not (null? (cddr stmt))) (add_var (var_name stmt) (M_value (var_value stmt) state) state)]
      [else (add_var (var_name stmt) (void) state)])))
; State access abstractions
(define state_vars car)
(define state_vals cadr)
;assigns a variable
(define M_assign
  (lambda (stmt state)
    (cond
      [(not (declared? (var_name stmt) (state_vars state))) (error 'assignerror "Undeclared Variable")]
      [else (add_var (var_name stmt) (M_value (var_value stmt) state) (remove_var (var_name stmt) state))])))

;defines a return value
;replaces #t and #f
(define ret_val cadr)
(define M_return
  (lambda (stmt state)
    (cond
      [(number? (M_value (ret_val stmt) state)) (M_value (ret_val stmt) state)]        
      [(M_bool (ret_val stmt) state) 'true]
      [else 'false])))

; If-statement & while-loop abstractions
(define condition cadr)
(define stmt1 caddr)
(define elif cdddr)
(define stmt2 cadddr)
(define loop_body cddr)
; Returns a state that results after the execution of an if statement.
(define M_if
  (lambda (stmt state)
    (if (M_bool (condition stmt) state)
        (M_state (stmt1 stmt) state next)
        (if (null? (elif stmt))
            state
            (M_state (stmt2 stmt) state next)))))

; Returns a state that results after the execution of a while loop.
(define M_while
  (lambda (stmt state next)
    (loop (condition stmt) (car (loop_body stmt)) state next)))


;helper function for goto constructs
(define loop
  (lambda (cond body state next)
    (if (M_bool cond state)
        (M_state body state (lambda (state1) (loop cond body state1 next)))
        next)))


;;STATE HELPER FUNCTIONS
(define declared?
  (lambda (var varlist)
    (cond
      [(null? varlist) #f]
      [(eq? var (current varlist)) #t]
      [else (declared? var (next varlist))])))

;adds a variable and a value to tables
(define add_var
  (lambda (var val state)
    (cond
      [(declared? var (state_vals state)) (error 'declareerror "Variable already declared")]
      [else (cons (cons var (state_vars state)) (cons (cons val (state_vals state)) null))])))

; Removes a variable and its corresponding value from the state, if present.
; Otherwise, the state is unchanged.
(define remove_var
  (lambda (var state)
    (remove_var_helper var (state_vars state) (state_vals state))))

;considered abstracting cons here, but it's used just for combining lists, its intended purpose
;not worth abstracting
(define remove_var_helper
  (lambda (var varlist vallist)
    (cond
      [(null? varlist) (cons varlist (cons vallist null))]
      [(eq? var (current varlist)) (cons (next varlist) (cons (next vallist) null))]
      [else (cons (cons (current varlist) (car (remove_var_helper var (next varlist) (next vallist))))
                  (cons (cons (current vallist) (ret_val (remove_var_helper var (next varlist) (next vallist)))) null))])))

;;EVALUATION FUNCTIONS
;abstractions for eval functions
(define operator car)
(define leftoperand cadr)
(define rightoperand caddr)
;;evaluates a boolean expression
;not required, but I implemented the shortcircuit operators && and || correctly
;although I couldn't think of a way that didn't use if so it's slightly bad scheme practice
(define M_bool
  (lambda (expr state)
    (cond
      [(eq? expr 'true) #t]
      [(eq? expr 'false) #f]
      [(var? expr) (findvar expr state)]
      [(eq? (operator expr) '! ) (not (M_bool (leftoperand expr) state))]                                         ;Unary !
      [(eq? (operator expr) '& ) (and (M_bool (leftoperand expr) state)       (M_bool  (rightoperand expr) state))]  ;and
      ;[(eq? (pre_op expr) '|) (or  (M_bool (l_operand expr) state)       (M_bool  (r_operand expr) state))]  ;or
      ;typing '| causes an error I can't fix (| seems to indicate end of file?) so normal | isn't implemented only short-circuit

      [(and (eq? (operator expr) '&&) (not (M_bool (leftoperand expr) state))) #f]                                   ;shortcircuit and
      [(eq? (operator expr) '&&) (M_bool (rightoperand expr) state)]
      [(and (eq? (operator expr) '||) (M_bool (leftoperand expr) state)) #t]                                         ;shortcircuit or
      [(eq? (operator expr) '||) (M_bool (rightoperand expr) state)]
      [(eq? (operator expr) '==) (eq? (M_value (leftoperand expr) state)      (M_value (rightoperand expr) state))]  ;equals
      [(eq? (operator expr) '!=) (not (eq? (M_value (leftoperand expr) state) (M_value (rightoperand expr) state)))] ;inequals
      [(eq? (operator expr) '< ) (<   (M_value (leftoperand expr) state)      (M_value (rightoperand expr) state))]  ;less
      [(eq? (operator expr) '> ) (>   (M_value (leftoperand expr) state)      (M_value (rightoperand expr) state))]  ;greater
      [(eq? (operator expr) '<=) (<=  (M_value (leftoperand expr) state)      (M_value (rightoperand expr) state))]  ;less or equals
      [(eq? (operator expr) '>=) (>=  (M_value (leftoperand expr) state)      (M_value (rightoperand expr) state))]  ;greater or equals
      [else (error 'operatorerror "Unknown Operator")])))

;evaluates an expression
(define M_value
  (lambda (expr state)
    (cond
      [(number? expr) expr]                                                                                      ;number
      [(var? expr) (findvar expr state)]                                                                         ;var
      [(and (eq? (operator expr) '-) (null? (cddr expr))) (- 0 (M_value (leftoperand expr) state))]                  ;unary -
      [(or (eq? (operator expr) '+) (eq? (operator expr) '-))(M_value_ashelper expr state)]                            ;add/sub
      [(or (or (eq? (operator expr) '*) (eq? (operator expr) '/)) (eq? (operator expr) '%)) (M_value_mdhelper expr state)];mult/div/mod
      [else (M_bool expr state)]))) ;If not arithmetic, boolean

;While this doesn't actually have an impact on priority, it helps me to put these together
;as per normal order of operations.
(define M_value_ashelper
  (lambda (expr state)
    (cond
      [(eq? (operator expr) '+) (+ (M_value (leftoperand expr) state) (M_value (rightoperand expr) state))]              
      [(eq? (operator expr) '-) (- (M_value (leftoperand expr) state) (M_value (rightoperand expr) state))])))
;Modulus is divisionesque so it goes here too
(define M_value_mdhelper
  (lambda (expr state)
    (cond
      [(eq? (operator expr) '*) (*         (M_value (leftoperand expr) state) (M_value (rightoperand expr) state))]              
      [(eq? (operator expr) '/) (quotient  (M_value (leftoperand expr) state) (M_value (rightoperand expr) state))]
      [(eq? (operator expr) '%) (remainder (M_value (leftoperand expr) state) (M_value (rightoperand expr) state))])))

;;HELPER FUNCTIONS
;makes sure something is a valid variable name (not numeric or a list or null)
;numeric names *might* be allowed by the specification, but I chose to avoid them
(define var?
  (lambda (x)
    (not (or (number? x) (or (pair? x) (null? x))))))

;;HELPER FUNCTIONS

;searches variable list for a variable and returns its value
;uses two abstractions
(define getvars car)
(define getvals cadr)
(define findvar
  (lambda (var state)
    (findvarhelper var (getvars state) (getvals state))))

(define findvarhelper
  (lambda (var varlist vallist)
    (cond
      [(null? varlist) (error 'declarerror "Undeclared Variable")]
      [(and (eq? var (car varlist)) (void? (car vallist))) (error 'assignerror "Unassigned Variable")]
      [(eq? var (car varlist)) (car vallist)] ;returns val associated with var
      [else (findvarhelper var (cdr varlist) (cdr vallist))])))
