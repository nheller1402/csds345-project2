#lang racket
(require "functionParser.rkt")

;;MAIN
;calls Mstate for the total state of the program on the parsed version of the file
(define interpret
  (lambda (filename)
    (call/cc
      (lambda (return)
        (syntax-tree (parser filename) empty return
                                  (lambda (env) (error 'error "continue used outside of loop"))
                                  (lambda (env) (error 'error "break used outside of loop"))
                                  (lambda (v env) (error 'error "throw outside of loop")))))))

(define syntax-tree
  (lambda (stmts state return continue break throw)
    (call/cc
     (lambda (return)
       (if (null? stmts)
           (evaluate_function state return continue break throw)
           (syntax-tree (next_stmt stmts) (M_state (current stmts) state return continue break throw) return continue break throw))))))

(define evaluate_function
  (lambda (state return continue break throw)
    (evaluate_call_body (cadr (findvar 'main state)) state return break continue throw)))

;;STATE FUNCTIONS
;abstractions for state functions
(define empty '(()()))
(define current car) 
(define next_stmt cdr)

;stmts is a list of statements and state is a list of states
;when state becomes singular (return statement or end of file), it is returned
(define M_state
  (lambda (stmts state return continue break throw)
    (cond
      [(eq? (current stmts) 'continue) (continue state)]
      [(eq? (current stmts) 'break) (break (pop_block state))]
      [(eq? (current stmts) 'throw) (throw (add_var 'exception (M_value (cadr stmts) state) state))]
      [(eq? (current stmts) 'try) (M_try stmts state return continue break throw)]
      [(eq? (current stmts) 'catch) (M_catch stmts state return continue break throw)]
      [(eq? (current stmts) 'finally) (M_finally stmts state return continue break throw)]
      [(eq? (current stmts) 'begin) (M_block stmts state return continue break throw)]
      [(eq? (current stmts) 'var) (M_declare stmts state)]
      [(eq? (current stmts) '=) (M_assign stmts state)] 
      [(eq? (current stmts) 'return) (M_return stmts state)]
      [(eq? (current stmts) 'if) (M_if stmts state return continue break throw)]
      [(eq? (current stmts) 'while) (M_while stmts state return continue break throw)]
      [(eq? (current stmts) 'function) (M_function stmts state return break continue throw)]
      [(eq? (current stmts) 'funcall) (M_call stmts state return break continue throw)]
      [else (error 'stmterror "Unknown Statement")])))


;block abstractions
(define outer_block cddr)
(define return_block caddr)

; Creates a new layer for the state.
(define create_block
  (lambda (state)
    (list '() '() state)))

; Pops the front (most recent) layer from the state.
(define pop_block
  (lambda (state)
    (if (null? (outer_block state))
        (error 'breakerror "No outer block")
        (return_block state))))

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
;use of set-box! means it can have side effects
(define M_assign
  (lambda (stmt state)
    (call/cc
     (lambda (end)
       (assign_var!
        (var_name stmt) (M_value (var_value stmt) state) state state)))))
;end is the initial full state that gets passed back
;state is the current working state for recursion
(define assign_var!
  (lambda (var value state end)
    (cond
      [(equal? state empty) (error 'varerror "Variable not declared: ~a" var)]
      [(null? (state_vars state)) (assign_var! var value (pop_block state) end)];not in current scope, check outer
      [(eq? var (car (state_vars state))) (begin (set-box! (car (state_vals state)) value) end)]
      [else (assign_var! var value (cons (cdr (state_vars state)) (cons (cdr (state_vals state)) (cddr state))) end)])))

;defines a return value
;replaces #t and #f
(define ret_val cadr)
(define M_return
  (lambda (stmt state)
    (cond
      [(number? (M_value (ret_val stmt) state)) (M_value (ret_val stmt) state)]        
      [(M_bool (ret_val stmt) state) 'true]
      [else 'false])))

;block function to change states for blocks
(define M_block
  (lambda (stmt state return continue break throw)
    (cond
      [(null? stmt) (pop_block state)]
      [(eq? (current stmt) 'begin) (M_block (next_stmt stmt) (create_block state) return continue break throw)]
      [else (M_block (next_stmt stmt) (M_state (current stmt) state return continue break throw) return continue break throw)])))

;returns state after try block
(define M_try
  (lambda (stmts state return continue break throw)
    (cond
      ((and (null? (catch-block stmts)) (null? (finally-block stmts))) (error 'error "try without catch and finally"))
      (else (M_finally (finally-block stmts) (M_catch (catch-block stmts) (call/cc (lambda (throw) (try-block stmts (cadr stmts) state return continue break throw))) return continue break throw) return continue break throw)))))

;helper function to check for return and breaks in try loop
(define try-block
  (lambda (try stmt state return continue break throw)
    (cond
      ((null? stmt) state)
      ((eq? 'return (car (current stmt))) (M_state (current stmt) (M_finally (finally-block try) state return continue break throw) return continue break throw))
      ((eq? 'break (car (current stmt))) (M_state (current stmt) (M_finally (finally-block try) state return continue break throw) return continue break throw))
      (else (try-block try (next_stmt stmt) (M_state (current stmt) state return continue break throw) return continue break throw)))))

;returns state after catch block
(define M_catch
  (lambda (stmts state return continue break throw)
    (cond
         ((null? stmts) state)
         ((and (eq? (current stmts) 'catch) (declared? 'exception (car state))) (M_catch (caddr stmts) (rename-exception-variable (caadr stmts) state) return continue break throw))
         ((eq? (current stmts) 'catch) state)
         (else (M_state (current stmts) state return continue break throw)))))

;renames exception variable to e
(define rename-exception-variable
  (lambda (exception state)
    (cond
      ((null? state) '())
      ((eq? (caar state) 'exception) (cons (cons 'e (cdar state)) (cdr state)))
      (else state))))

;helper function to return statements in catch block
(define catch-block
  (lambda (stmt)
    (cond
      ((null? (caddr stmt)) '())
      (else (caddr stmt)))))

;returns state after finally block
(define M_finally
  (lambda (stmt state return continue break throw)
    (cond
      ((null? stmt) state)
      (else (M_finally (next_stmt stmt) (M_state (current stmt) state return continue break throw) return continue break throw)))))

;helper function to return statements in finally block
(define finally-block
  (lambda (stmt)
    (cond
      ((null? (cadddr stmt)) '())
      (else (cadr (cadddr stmt))))))

; If-statement & while-loop abstractions
(define condition cadr)
(define stmt1 caddr)
(define elif cdddr)
(define stmt2 cadddr)
(define loop_body cddr)

; Returns a state that results after the execution of an if statement.
(define M_if
  (lambda (stmt state return continue break throw)
    (if (M_bool (condition stmt) state)
        (M_state (stmt1 stmt) state return continue break throw)
        (if (null? (elif stmt))
            state
            (M_state (stmt2 stmt) state return continue break throw)))))


; Returns a state that results after the execution of a while loop.
(define M_while
  (lambda (stmt state return continue break throw)
    (call/cc
     (lambda (break)
       (if (M_bool (condition stmt) state)
           (M_state stmt (call/cc (lambda (continue) (M_state (car (loop_body stmt)) state return continue break throw))) return continue break throw)
           state)))))


;;STATE HELPER FUNCTIONS
(define declared?
  (lambda (var varlist)
    (cond
      [(null? varlist) #f]
      [(eq? var (current varlist)) #t]
      [else (declared? var (next_stmt varlist))])))

;adds a variable and a value to tables
(define add_var
  (lambda (var val state)
    (cond
      [(declared? var state) (error 'declerror "Variable already declared: ~a" var)]
      [(null? (cddr state)) (list (cons var (state_vars state)) (cons (box val) (state_vals state)))]
      [else (list (cons var (state_vars state)) (cons (box val) (state_vals state)) (pop_block state))])))

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
      [(eq? var (current varlist)) (cons (next_stmt varlist) (cons (next_stmt vallist) null))]
      [else (cons (cons (current varlist) (car (remove_var_helper var (next_stmt varlist) (next_stmt vallist))))
                  (cons (cons (current vallist) (ret_val (remove_var_helper var (next_stmt varlist) (next_stmt vallist)))) null))])))

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
    (cond
      [(equal? state empty) (error 'varerror "Variable not declared: ~a" var)]
      [(null? (getvars state)) (findvar var (pop_block state))]
      [(and (eq? var (car (getvars state))) (void? (unbox (car (getvals state))))) (error 'varerror "Variable not assigned: ~a" var)]
      [(eq? var (car (getvars state))) (unbox (car (getvals state)))]
      [else (findvar var (cons (cdr (getvars state)) (cons (cdr (getvals state)) (cddr state))))])))

(define M_function
  (lambda (stmt state return break continue throw)
    (cond
      ((null? (cdddr stmt)) state)
      (else (add_var (cadr stmt) (cddr stmt) state)))))

(define M_call
  (lambda (stmt state)
    (evaluate_call_body (cadr (findvar (cadr stmt) state)) (addParameter (findvar (cadr stmt) state) (evaluateParameter (cddr stmt) state) (append (cons empty (findScope (cadr stmt) state)) state)))))

(define findScope
  (lambda (function state)
    (cond
      ((null? state) (error 'findScope "scope not found"))
      ((declared? function (car state)) state)
      (else (findScope function (cdr state))))))

(define evaluate_call_body
  (lambda (body state return continue break throw)
    (cond
      ((null? body) state)
      ((eq? (caar body) 'return) (return state))
      ((eq? (car body) 'var) (evaluate_call_body (cdr body) (function_declaration (car body) state throw) return break continue throw))
      (else (evaluate_call_body (rest body) (M_state (car body) state return break continue throw) return break continue throw)))))

(define function_declaration
  (lambda (stmt state)
    (if (declared? (cadr stmt) (car state))
        (M_assign stmt state)
        (M_declare stmt state))))

(define addParameter
  (lambda (parameter value state)
    (cond
      ((and (null? parameter) (null? value)) state)
      ((or (null? parameter) (null? value)) (error 'error "Mismatched parameters and arguments"))
      (else (addParameter (cdr parameter) (cdr value) (add_var (car parameter) (car value) state))))))

(define evaluateParameter
  (lambda (parameter state)
    (if (null? parameter)
        empty
        (cons (M_value (car parameter) state) (evaluateParameter (cdr parameter) state)))))

