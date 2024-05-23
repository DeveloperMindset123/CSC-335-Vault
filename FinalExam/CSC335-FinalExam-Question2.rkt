;; CSC 335 PM Section Final Exam Question 2
;; Authors : Ayan Das, Peng Gao
;; Email (Ayan) : adas006@citymail.cuny.edu, Email (Peng Gao) : pgao000@citymail.cuny.edu

;; Global environment initialization
(define my-global-env '())



;; Function to lookup variables in the environment
(define (my-lookup var env)
  (cond ((null? env)
         (begin
           (display "Unbound variable: ")
           (display var) (newline) #f))
        ((eq? (caar env) var)
         (cdar env))
        (else
         (my-lookup var (cdr env)))))



;; Function to extend the environment with new bindings
(define (my-extend-env vars vals env)
  (if (null? vars)
      env
      (my-extend-env
       (cdr vars)
       (cdr vals)
       (cons
        (cons
         (car vars)
         (car vals))
        env))))


;; Function to update the global environment
(define
  (my-update-env! var val)
  (let
      ((binding
        (assoc var my-global-env)))
    (if binding
        (set-cdr! binding val)
        (set! my-global-env
              (cons
               (cons
                var
                val)
               my-global-env)))))


;; Function to create closures
(define
  (my-make-closure
   params
   body
   env)
  (list 'closure params body env))



;; Custom arithmetic functions
(define (my-add a b)
  (if
   (and (number? a)
           (number? b))
      (+ a b)
      (begin
        (display "Non-numeric argument to +")
        (newline) #f)))



(define (my-sub a b)
  (if
   (and (number? a)
        (number? b))
      (- a b)
      (begin
        (display "Non-numeric argument to -")
        (newline) #f)))



(define (my-mul a b)
  (if
   (and
    (number? a)
    (number? b))
      (* a b)
      (begin
        (display "Non-numeric argument to *")
        (newline) #f)))



(define (my-div a b)
  (if
   (and
       (number? a)
       (number? b))
      (/ a b)
      (begin
        (display "Non-numeric argument to /")
        (newline) #f)))



;; Custom greater than

(define (my-gt a b)
  (if
   (and
    (number? a)
    (number? b))
      (> a b)

      (begin
        (display "Non-numeric argument to >")
        (newline) #f)))

;; Helper function to fold arguments (reduce)
(define
  (fold-left proc initial lst)
  (if (null? lst)
      initial
      (fold-left proc (proc initial
                            (car lst))
                 (cdr lst))))


;; Evaluator function
(define (my-eval expr)
  (cond
    ((symbol? expr)
     (my-lookup expr my-global-env))
        ((number? expr)
         expr)
        ((pair? expr)
         (let ((op (car expr))
               (args
                (cdr expr)))

           (cond
             ((eq? op 'lambda)
                  (my-make-closure (car args) (cadr args) my-global-env))
                 ((eq? op 'quote)
                  (cadr expr))
                 ((eq? op 'let)
                  (my-eval-let (cadr expr) (caddr expr)))
                 ((eq? op 'cond)
                  (my-eval-cond (cdr expr)))
                 (else
                  (let ((proc (my-eval op))
                        (evaluated-args (map my-eval args)))
                    (my-apply proc evaluated-args))))))
        (else
         (begin
           (display "Unknown expression type: ")
           (display expr)
           (newline) #f))))



;; Function to apply procedures

(define (my-apply proc args)
  (cond ((eq?
          (car proc) 'primitive)
         ((cadr proc) args))
        ((and (pair? proc)
              (eq? (car proc) 'closure))
         (let ((params (cadr proc))
               (body (caddr proc))
               (closure-env
                (cadddr proc)))
           (let ((extended-env
                  (my-extend-env
                   params
                   args
                   closure-env)))
             (my-eval-body body extended-env))))
        (else
         (begin
           (display "Unknown procedure type: ")
           (display proc)
           (newline) #f))))



;; Function to evaluate the body of a closure with a specific environment
(define (my-eval-body body env)
  (let ((saved-env my-global-env))
    (set! my-global-env env)
    (let ((result
           (my-eval body)))
      (set! my-global-env saved-env)
      result)))



;; Example procedures for testing
(define (my-define-func
         name
         params
         body)
  (my-update-env! name
                  (my-make-closure
                   params
                   body
                   my-global-env)))



;; Define basic arithmetic procedures without Scheme primitives
(my-update-env! '+ (list 'primitive (lambda (args) (fold-left my-add 0 args))))
(my-update-env! '* (list 'primitive (lambda (args) (fold-left my-mul 1 args))))
(my-update-env! '- (list 'primitive (lambda (args) (if (null? (cdr args))
                                                         (my-sub 0 (car args))
                                                         (fold-left my-sub (car args) (cdr args))))))
(my-update-env! '/ (list 'primitive (lambda (args) (if (null? (cdr args))
                                                         (my-div 1 (car args))
                                                         (fold-left my-div
                                                                    (car args)
                                                                    (cdr args))))))



;; Define the custom comparison operator '>'
(my-update-env! '>'
                (list 'primitive
                      (lambda (args)
                        (if (and
                             (number? (car args))
                             (number? (cadr args)))
                            (my-gt (car args) (cadr args))
                            (begin
                              (display "Non-numeric argument to >")
                              (newline) #f)))))



;; Custom my-car and my-cdr functions to avoid Scheme primitives
(define (my-car pair)
  (if (pair? pair)
      (car pair)
      (begin (display "my-car: not a pair") (newline) #f)))



(define (my-cdr pair)
  (if (pair? pair)

      (cdr pair)

      (begin (display "my-cdr: not a pair") (newline) #f)))



;; Custom error function

(define (error msg . args)

  (define (display-args args)

    (if (not (null? args))

        (begin

          (display (car args))

          (display-args (cdr args)))))

  (display msg)

  (display-args args)

  (newline)

  (void))  ; calling void to indicate an error



;; Custom functions to build and access lists

(define (my-cons x y)

  (cons x y))



(define (my-list . elements)

  (if (null? elements) '()

      (my-cons (car elements) (apply my-list (cdr elements)))))



;; Function to evaluate let expressions

(define (my-eval-let bindings body)

  (let ((extended-env (my-extend-env (map my-car bindings)

                                     (map (lambda (binding) (my-eval (my-cadr binding)))

                                          bindings)

                                     my-global-env)))

    (let ((saved-env my-global-env))

      (set! my-global-env extended-env)

      (let ((result (my-eval body)))

        (set! my-global-env saved-env)

        result))))



;; Function to evaluate cond expressions

(define (my-eval-cond clauses)

  (if (null? clauses)

      #f

      (let ((clause (car clauses)))

        (if (my-eval (my-car clause))

            (my-eval (my-cadr clause))

            (my-eval-cond (cdr clauses))))))



;; Example: using lexical scope

(my-update-env! 'x 10)

(my-define-func 'make-adder '(y)

  '(lambda (z) (+ x y z)))

(define add5 (my-eval '(make-adder 5)))

(my-update-env! 'add5 add5)  ;; Update the global environment with add5

(display (my-eval '(add5 3)))  ; Should return 18

(newline)





;; Example where old TLS and new TLS differ



;; Define a function that captures the environment

(my-update-env! 'x 1)

(my-define-func 'capture-env '() 'x)



;; Call the function to capture the current value of x (should capture 1 in old TLS)

(display (my-eval '(capture-env)))  ; Old TLS would return 1

(newline)



;; Update x

(my-update-env! 'x 100)



;; Call the function again

(display (my-eval '(capture-env)))  ; New TLS would return 100, Old TLS would still return 1

(newline)



;; proof by induction
;;
;; Base case: For the atom expressions, in our new tls they're the symbols and numbers, the evaluator will look up the atom in the
;; environment or just return the number itself if it's a number. This is consistent with lexical scoping as each symbol refers to
;; the binding in the environment.
;;
;; By referencing the binding in the environment, it satisfies the condition for lexical/statical scoping, which saves the environment
;;
;; Inductive case:
;; For the more complex expressions such as functions, the evaluator first checks the operator and then the operand. If the operator
;; is a procedure, it applies the procedure. If the operator is a closure, it ends the environment and matches the parameter with
;; their arugment values, and finally evaluates the code in the extended environment. Doing so ensures the correct variable bindings,
;; adhering to the lexcical scoping rules.
;;
;; Therefore, by induction, the redesigned interpreter correctly implements
;; lexical scoping for all expressions.



;; Custom my-symbol? function to avoid conflict with built-in symbol?

(define (my-symbol? expr)

  (and (not (pair? expr)) (not (number? expr))))



;; Check if a list contains only symbols

(define (list-of-symbols? lst)

  (if (null? lst)

      #t

      (and (my-symbol? (car lst)) (list-of-symbols? (cdr lst)))))



;; Check if a list of bindings is valid

(define (valid-bindings? bindings env)

  (if (null? bindings)

      #t

      (let ((binding (car bindings)))

        (and (pair? binding)

             (my-symbol? (car binding))

             (my-check-syntax (cadr binding) env)

             (valid-bindings? (cdr bindings) env)))))



;; Helper function to check a list of expressions

(define (every pred lst)

  (cond ((null? lst) #t)

        ((pred (car lst)) (every pred (cdr lst)))

        (else #f)))



;; Syntax checker function
; Brief Proof as shown below
;Base case: 
;for Atomic expressions, numbers are always valid, so it returns true for numbers
;Symbols should be defined in the environment, so the syntax checker returns #t if found

;inductive case:
;quote expressions should have 1 argument, the syntax checker checks if the length of the cdr of the expression is 1. For lambda expressions, the expression should have a list of symbols as parameters and a valid body. The syntax checker verifies this recursively. Let expressions should have valid bindings and body. Syntax checker will verify all the bindings and check the body recursively. cond expressions should be presented with valid clauses, the syntax checker will verify each clause by checking the test expression and the resultant expression. for functions, both operator and operands should be valid, the syntax checker checks that recursively

; Thus, by induction, the syntax checker verifies the syntax of all expressions either recursively or through other means according to lexical scoping rules. 

(define (my-check-syntax expr env)

  (cond

    ;; Numbers are always valid

    ((number? expr) #t)

    

    ;; Symbols should be defined in the environment

    ((my-symbol? expr) (if (my-lookup expr env) #t (begin (display "Unbound variable: ") (display expr) (newline) #f)))

    

    ;; Pairs need further checks

    ((pair? expr)

     (let ((op (car expr)))

       (cond

         ;; quote expressions

         ((eq? op 'quote)

          (if (= (length (cdr expr)) 1) #t (begin (display "Syntax error in quote: ") (display expr) (newline) #f)))

         

         ;; lambda expressions

         ((eq? op 'lambda)

          (let ((params (cadr expr))

                (body (caddr expr)))

            (if (list-of-symbols? params)

                (my-check-syntax body (my-extend-env params params env))

                (begin (display "Invalid parameters in lambda: ") (display expr) (newline) #f))))

         

         ;; let expressions

         ((eq? op 'let)

          (let ((bindings (cadr expr))

                (body (caddr expr)))

            (if (valid-bindings? bindings env)

                (my-check-syntax body (my-extend-env (map car bindings) (map car bindings) env))

                (begin (display "Syntax error in let: ") (display expr) (newline) #f))))

         

         ;; cond expressions

         ((eq? op 'cond)

          (let loop ((clauses (cdr expr)))

            (if (null? clauses)

                #t

                (let ((clause (car clauses)))

                  (if (and (pair? clause)

                           (my-check-syntax (car clause) env)

                           (my-check-syntax (cadr clause) env))

                      (loop (cdr clauses))

                      (begin (display "Syntax error in cond clause: ") (display clause) (newline) #f))))))

         

         ;; Function applications

         ((my-check-syntax op env)

          (if (every (lambda (arg) (my-check-syntax arg env)) (cdr expr))

              #t

              (begin (display "Syntax error in function application: ") (display expr) (newline) #f)))

         

         ;; Default case for unknown expressions

         (else (begin (display "Unknown expression type: ") (display expr) (newline) #f)))))

    

    ;; Default case for unknown expressions

    (else (begin (display "Unknown expression type: ") (display expr) (newline) #f))))



;; Testing the syntax checker

(display (my-check-syntax '(let ((x 5)) (+ x 3)) my-global-env)) ; Should be #t
(newline) ; checks the let expression where x is bound to 5 and then (+ x 3) is evaluated within the body of let

(display (my-check-syntax '(lambda (x y) (+ x y)) my-global-env)) ; Should be #t
(newline) ; checks the lambda expression that takes 2 parameters x and y

(display (my-check-syntax '(cond ((> x 0) 1) (else 0)) my-global-env)) ; Should be #t if > is defined
(newline) ; checks the cond expression that checks two cases

(display (my-check-syntax '(+ x y) my-global-env)) ; Should be #f if x and y are unbound
(newline) ; checks an expression that adds x and y

