;; ---------------------------------------------------------------------------
;; 1.  Write and prove correct a program to input an LD number (as previously defined) which returns a list of all lists of
;; 1s and 2s which map to that number.  Use this program to check your solution to the takehome part of the midterm. 
;; ---------------------------------------------------------------------------

; Name(s) : Ayan Das, Peng Gao
; CSC 335 PM Section, email : adas006@citymail.cuny.edu

; P.S. I know I wrote pre and post condition for simple function, that's per the teams annoucement, since they aren't as important, you can skim over or simply ignore them altogether if it's too much to read.


; pre-condition : lst is a list and elem is any valid valid scheme val
; post-condition : Returns a new list with 'elem' appended to the end of the lst
(define (append-to-end lst elem)
  (append lst (list elem)))


; pre-condition : func is a function that takes one argument and returns a list, lst is a list of elements.
; post-condition : returns a new list by applying 'func' to each element of 'lst' and concactenating the results.
(define (flatmap func lst)
  (cond
    ((null? lst) '())  ;; If the list is empty, return an empty list
    (else (append (func (car lst)) (flatmap func (cdr lst))))))

; Extensive Proof:
; Base case:
; 1. Empty List
; --> If 'lst' is empty (null? lst), remaining is also empty.
; --> The function returns 'list prefix' if 'sublist' is false, otherwise, it returns '().
; --> This correctly handles the base of generating permutation on an empty list
; Inductive Case
; Handling Sublist elements
; --> When 'sublist' is true and 'new-seq' is empty, the function appends the current element to 'new-seq' and continues with the rest of the remaining.
; --> When the current element is '2', the function creates two branches
; When the current element is 2, the function creates two branches
; --> 1. Continuing the append elements to 'new-seq' with 2.
; --> 2. Creating permutations of 'new-seq' using 'flatmap' and appending them to 'prefix'
; For other elements, it appends them to 'new-seq' and continues with the rest of 'remaining'

; Handling main sublist elements
; When sublist is false, the function appends 1 to prefix and creates two branches:
; 1. Continuing without a sublist
; 2. Starting with a new sublist
; For other elements, it appends them to prefix and continues without a sublist.
(define (generate-permutations lst)
  (define (permute prefix new-seq sublist remaining)
    (cond
      ((null? remaining)
       (if sublist '() (list prefix)))  
      (sublist
       (cond
         ((null? new-seq)
          (permute prefix (append-to-end new-seq (car remaining)) #t (cdr remaining)))
         ((eq? 2 (car remaining))
          (append
           (permute prefix (append-to-end new-seq 2) #t (cdr remaining))
           (flatmap
            (lambda (item) (permute (append-to-end prefix item) '() #f (cdr remaining)))
            (generate-permutations new-seq))))
         (else (permute prefix (append-to-end new-seq (car remaining)) #t (cdr remaining)))))
      (else
       (cond
         ((eq? 1 (car remaining))
          (append
           (permute (append-to-end prefix 1) '() #f (cdr remaining))
           (permute prefix '() #t (cdr remaining))))
         (else (permute (append-to-end prefix (car remaining)) '() #f (cdr remaining)))))))
  (permute '() '() #f lst))

;; Example usage
(length (generate-permutations '(9 1 8 7 2 3 0 8 6 6 1 2 7 8 7 4 9 6 9 1 2 3 2 2 4 3 1 2 1 6 2 9 0 6 2 1 2 1 2 1 6 0 1)))
(newline)

;; ---------------------------------------------------------------------------
;; Helper Functions (For Question 2 and 3)
;; ---------------------------------------------------------------------------

; pre-condition : x and y are valid Scheme values
; post-condition : Returns a pair (cons cell) with x as the car and y as the cdr.
(define (my-cons x y)
  (cons x y))

; pre-condition : z must be a pair
; post-condition : Returns the car (first element) of the pair 'z'. Throws an error if 'z' is not a pair.
(define (my-car z)
  (if (pair? z)
      (car z)
      (error "my-car: expected a pair, got " z)))

; pre-condition : z must be a pair
; post-condition : Returns the cdr (second element) of the pair 'z'. Throws an error if z is not a pair
(define (my-cdr z)
  (if (pair? z)
      (cdr z)
      (error "my-cdr: expected a pair, got " z)))

; Pre-condition: z must be a list with at least two elements.
; Post-condition: Returns the second element of the list z.
(define (my-cadr z)
  (my-car (my-cdr z)))

; Pre-condition: z must be a list with at least three elements.
; Post-condition: Returns the third element of the list z.
(define (my-caddr z)
  (my-car (my-cdr (my-cdr z))))

; Pre-condition: z must be a list with at least four elements.
; Post-condition: Returns the fourth element of the list z.
(define (my-cadddr z)
  (my-car (my-cdr (my-cdr (my-cdr z)))))

; Pre-condition: elements can be any valid Scheme values.
; Post-condition: Returns a list containing all elements.
(define (my-list . elements)
  (if (null? elements) '()
      (my-cons (my-car elements) (apply my-list (my-cdr elements)))))

 ;Pre-condition: x can be any valid Scheme value.
; Post-condition: Returns #t if x is a pair, otherwise #f.
(define (my-pair? x)
  (pair? x))

; Pre-condition: condition, then-clause, and else-clause can be any valid Scheme values.
; Post-condition: Evaluates and returns then-clause if condition is true, otherwise evaluates and returns else-clause.
(define (my-if condition then-clause else-clause)
  (if condition then-clause else-clause))


; Pre-condition: clauses must be a list of conditions and corresponding expressions.
; Post-condition: Evaluates and returns the expression for the first true condition. If no condition is true, returns #f.
(define (my-cond . clauses)
  (if (null? clauses)
      #f
      (let ((clause (car clauses)))
        (if (eval (car clause))
            (eval (cadr clause))
            (apply my-cond (cdr clauses))))))

; Pre-condition: pred is a predicate function, and lst is a list of elements.
; Post-condition: Returns #t if pred returns #t for all elements in lst, otherwise #f.
(define (every pred lst)
  (if (null? lst)
      #t
      (and (pred (car lst)) (every pred (cdr lst)))))

; Pre-condition: None.
; Post-condition: Returns an empty list ().
(define (void)
  '())

; Pre-condition : s1 and s2 can be any valid scheme values.
; Post-condition : Returns a pair with 's1' as the car and 's2' as the cadr
(define build
  (lambda (s1 s2)
    (my-cons s1 (my-cons s2 (quote ())))))

; pre-condition : x is a pair
; post-condition : returns the car (first element) of the pair x
(define first my-car)

; pre-condition : x is a list with at least two elements
; post-condition : Returns the car (first elemnt) of pair x
(define second my-cadr)

; Pre-condition: x is a list with at least three elements.
; Post-condition: Returns the third element of the list x.
(define third my-caddr)

; Pre-condition: x can be any valid Scheme value.
; Post-condition: Returns #t if x is an atom (not a pair and not null), otherwise #f.
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

;; Error implementation 
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


;; ---------------------------------------------------------------------------
;; 2. Write and prove correct a syntax checker for TLS-scheme, as specified in HW 11.
;; ---------------------------------------------------------------------------

(define (atom? x)
  (and (not (pair? x)) (not (null? x))))

(define (simple-check expr)
  (cond
    ((number? expr) #t)
    ((boolean? expr) #t)
    ((symbol? expr) #t)
    ((my-pair? expr)
     (case (my-car expr)
       ((quote) (and (my-pair? (my-cdr expr)) (null? (my-cddr expr))))
       ((lambda) (and (my-pair? (my-cdr expr))
                      (my-pair? (my-cadr expr))
                      (every symbol? (my-cadr expr))
                      (simple-check (my-caddr expr))))
       ((cond) (every (lambda (clause)
                        (and (my-pair? clause)
                             (simple-check (my-car clause))
                             (simple-check (my-cadr clause))))
                      (my-cdr expr)))
       (else (every simple-check (my-cdr expr)))))
    (else #f)))

; Extensive Proof
; Goal : To prove that 'extended-syntax-checker' correctly checks the syntax of a given expression.
; Proof By Indunction:
; 1. Base case : If 'expr' is a number, boolean or symbol, 'extended-syntax-checker' returns true.
; 2. Inductive Case : Assume 'extended-syntax-checker' works for expression of size n or smaller.
; if expr is a pair:
; --> if my-car expr is a quote, extended-syntax-checker checks if the structure is valid.
; --> if my-car expr is lambda, extended-syntax-checker verifies that the parameters are symbols and recursively checks the body
; --> if my-car expr is cond, extended-syntax-checker recursively checks each clause
; --> For all other cases, extended-syntax-checker checks the arity of the function.
; By the inductive hyphothesis, since all sub-expressions are of size n or smaller, they are correctly checked.
; Therefore, extended-syntax-checker correctly checks expression of size n+1.
(define (extended-syntax-checker expr env)
  (cond
    ((number? expr) #t)
    ((boolean? expr) #t)
    ((symbol? expr) #t)
    ((my-pair? expr)
     (case (my-car expr)
       ((quote) (and (my-pair? (my-cdr expr)) (null? (my-cddr expr))))
       ((lambda) (let ((params (my-cadr expr))
                       (body (my-caddr expr)))
                   (and (every symbol? params)
                        (extended-syntax-checker body (append params env)))))
       ((cond) (every (lambda (clause)
                        (and (my-pair? clause)
                             (extended-syntax-checker (my-car clause) env)
                             (extended-syntax-checker (my-cadr clause) env)))
                      (my-cdr expr)))
       (else (let ((fn (my-car expr))
                   (args (my-cdr expr)))
               (and (extended-syntax-checker fn env)
                    (every (lambda (arg) (extended-syntax-checker arg env)) args)
                    (let ((arity (get-arity fn env)))
                      (eq? (length args) arity)))))))
     (else #f)))

; Pre-condition: sym is a symbol, and env is a list representing the environment.
; Post-condition: Returns #t if sym is an element of env, otherwise returns #f.
(define (bound? sym env)
  (member sym env))

(define (get-arity fn env)
  (cond
    ((eq? fn 'cons) 2)
    ((eq? fn 'car) 1)
    ((eq? fn 'cdr) 1)
    ((eq? fn 'null?) 1)
    ((eq? fn 'eq?) 2)
    ((eq? fn 'atom?) 1)
    ((eq? fn 'zero?) 1)
    ((eq? fn 'add1) 1)
    ((eq? fn 'mul) 2)
    ((eq? fn 'sub1) 1)
    ((eq? fn 'number?) 1)
    ((eq? fn 'square) 1)
    ((eq? fn '+) 2)
    ((eq? fn '-) 2)
    ((eq? fn '*) 2)
    ((eq? fn '/) 2)
    (else #f)))


;; ---------------------------------------------------------------------------
;; 3. Write and prove correct an interpreter for TLS extended by let*.
;; ---------------------------------------------------------------------------

; Extensive Proof:
; Goal: To prove that *let* correctly interprets a let* expression by evaluating the bindings sequentially.

; 1. Evaluation of Bindings:
; 'let*' allows for sequential bindings, where each binding can reference previous bindings.
; 'let*-eval' evaluates each binding in sequence, extending the environment with each new binding.

; 2. Evaluation of Body:
; Once all bindings are evaluated, the body is evaluated in the extended environment.
; This ensures that each binding can reference the previous bindings, preserving the semantics of 'let*'.

; 3. Correctness:
; Since 'let*' evaluates the bindings sequentially and then evaluates the body in the extended environment, it correctly interprets the 'let*' expression.
(define *let 
  (lambda (e table) 
    (let* 
    ((bindings (my-cadr e))
     (vars (map my-car bindings))
     (vals (map my-cadr bindings))
     (body (my-caddr e))
     (lambda-expr (my-cons (list (quote lambda) vars body) vals)))
      (meaning lambda-expr table))))

(define *let*
  (lambda (e table)
    (let*-eval (my-cadr e) table (my-caddr e))))

(define let*-eval
  (lambda (bindings table body)
    (if (null? bindings)
        (meaning body table)
        (let* ((binding (my-car bindings))
               (var (my-car binding))
               (val (meaning (my-cadr binding) table)))
          (let*-eval (my-cdr bindings) (extend-table (new-entry (list var) (list val)) table) body)))))

(define lookup-in-table
  (lambda (name table table-f)
    (cond 
      ((null? table) (table-f name))
      (else (lookup-in-entry name
                             (my-car table)
                             (lambda (name)
                               (lookup-in-table name
                                                (my-cdr table)
                                                table-f)))))))

(define extend-table my-cons)

(define lookup-in-entry
  (lambda (name entry entry-f)
    (lookup-in-entry-helper name
                          (names entry)
                          (vals entry)
                          entry-f)))

(define lookup-in-entry-helper
  (lambda (name names vals entry-f)
    (cond
      ((null? names) (entry-f name))
      ((eq? (my-car names) name) (my-car vals))
      (else (lookup-in-entry-helper name
                                  (my-cdr names)
                                  (my-cdr vals)
                                  entry-f)))))

(define new-entry build)

(define names
  (lambda (entry) (my-car entry)))

(define vals
  (lambda (entry) (my-cadr entry)))

(define value
  (lambda (e)
    (meaning e (quote ()))))

; Extensive Proof:
; Goal : To prove that 'meaning' correctly evaluates any given expression according to the rules of the interpreter.
; Proof by Structural Induction:
; 1. Base Case
; --> For atomic expressions (numbers, booleans, symbols), 'meaning' returns the correct value or looks up the value in the environment.
; --> This is trivially correct.

; 2. Inductive Case
; --> Assume 'meaning' works for expression of size 'n' or smaller.
; --> For an expression 'expr' of size 'n+1'.
; --> expression-to-action determines the type of expression and calls the appropriate action.
; --> each action (e.g. *const, *quote, *lambda, *application) correctly evaluates the expression by either returning a constant value, looking up a value, or recursively evaluating subexpressions.
; By the IH, since all subexpressions are of size 'n' or smaller, they are correctly evaluated.
; Therefore, 'meaning' correctly evaluates expressions of size n+1.
(define meaning
  (lambda (e table)
    ((expression-to-action e) e table)))

; pre and post condition not included because these were code provided to us
(define expression-to-action
  (lambda (e)
    ; Pre-condition: e is a valid Scheme expression.
    ; Post-condition: Returns the appropriate action (function) based on whether e is an atom or a list.
    (cond
      ((:atom? e) (atom-to-action e))
      (else (list-to-action e)))))

(define atom-to-action
  (lambda (e)
    ; Pre-condition: e is an atomic Scheme expression.
    ; Post-condition: Returns the appropriate action function for atomic expressions.
    (cond
      ((number? e) *const)
      ((eq? e #t) *const)
      ((eq? e #f) *const)
      ((eq? e (quote cons)) *const)
      ((eq? e (quote car)) *const)
      ((eq? e (quote cdr)) *const)
      ((eq? e (quote null?)) *const)
      ((eq? e (quote eq?)) *const)
      ((eq? e (quote atom?)) *const)
      ((eq? e (quote zero?)) *const)
      ((eq? e (quote add1)) *const)
      ((eq? e (quote mul)) *const)
      ((eq? e (quote sub1)) *const)
      ((eq? e (quote number?)) *const)
      ((eq? e (quote +)) *const)
      ((eq? e (quote -)) *const)
      ((eq? e (quote *)) *const)
      ((eq? e (quote /)) *const)
      (else *identifier))))

(define list-to-action
  (lambda (e)
    ; Pre-condition: e is a valid Scheme list expression.
    ; Post-condition: Returns the appropriate action function for list expressions.
    (cond
      ((:atom? (my-car e))
       (cond 
         ((eq? (my-car e) (quote quote))
          *quote)
         ((eq? (my-car e) (quote lambda))
          *lambda)
         ((eq? (my-car e) (quote cond))
          *cond)
         ((eq? (my-car e) (quote let)) 
          *let)
         ((eq? (my-car e) (quote let*))
          *let*)
         (else *application)))
      (else *application))))

(define *const
  (lambda (e table)
    ; Pre-condition: e is a constant expression, and table is the current environment.
    ; Post-condition: Returns the constant value or builds a primitive function.
    (cond 
      ((number? e) e)
      ((eq? e #t) #t)
      ((eq? e #f) #f)
      (else (build (quote primitive) e)))))

(define *quote
  (lambda (e table)
    ; Pre-condition: e is a quote expression, and table is the current environment.
    ; Post-condition: Returns the quoted expression.
    (text-of e)))

(define text-of my-cadr)
; Pre-condition: e is a quote expression with at least two elements.
; Post-condition: Returns the second element of the quote expression.

(define *identifier
  (lambda (e table)
    ; Pre-condition: e is an identifier, and table is the current environment.
    ; Post-condition: Returns the value of the identifier from the environment.
    (lookup-in-table e table initial-table)))

(define initial-table
  (lambda (name)
    ; Pre-condition: name is any symbol.
    ; Post-condition: Returns the initial environment (empty).
    (my-car (quote ()))))

(define *lambda
  (lambda (e table)
    ; Pre-condition: e is a lambda expression, and table is the current environment.
    ; Post-condition: Returns a closure containing the lambda expression and the environment.
    (build (quote non-primitive)
           (my-cons table (my-cdr e)))))

(define table-of first)
; Pre-condition: closure is a valid closure created by *lambda.
; Post-condition: Returns the environment part of the closure.

(define formals-of second)
; Pre-condition: closure is a valid closure created by *lambda.
; Post-condition: Returns the formal parameters part of the closure.

(define body-of third)
; Pre-condition: closure is a valid closure created by *lambda.
; Post-condition: Returns the body part of the closure.

(define evcon
  (lambda (lines table)
    ; Pre-condition: lines is a list of cond clauses, and table is the current environment.
    ; Post-condition: Evaluates and returns the result of the first true clause.
    (cond 
      ((else? (question-of (my-car lines)))
       (meaning (answer-of (my-car lines))
                table))
      ((meaning (question-of (my-car lines))
                table)
       (meaning (answer-of (my-car lines))
                table))
      (else (evcon (my-cdr lines) table)))))

(define else?
  (lambda (x)
    ; Pre-condition: x is any expression.
    ; Post-condition: Returns #t if x is the symbol 'else', otherwise #f.
    (cond
      ((:atom? x) (eq? x (quote else)))
      (else #f))))

(define question-of first)
; Pre-condition: clause is a valid cond clause.
; Post-condition: Returns the condition part of the clause.

(define answer-of second)
; Pre-condition: clause is a valid cond clause.
; Post-condition: Returns the expression part of the clause.

(define *cond
  (lambda (e table)
    ; Pre-condition: e is a cond expression, and table is the current environment.
    ; Post-condition: Evaluates and returns the result of the first true clause.
    (evcon (cond-lines-of e) table)))

(define cond-lines-of my-cdr)
; Pre-condition: e is a cond expression with at least one clause.
; Post-condition: Returns the list of cond clauses.

(define evlis
  (lambda (args table)
    ; Pre-condition: args is a list of expressions, and table is the current environment.
    ; Post-condition: Evaluates and returns the list of evaluated expressions.
    (cond
      ((null? args) (quote ()))
      (else
       (my-cons (meaning (my-car args) table)
                (evlis (my-cdr args) table))))))

(define *application
  (lambda (e table)
    ; Pre-condition: e is an application expression, and table is the current environment.
    ; Post-condition: Evaluates the function and arguments, then applies the function to the arguments.
    (myapply
     (meaning (function-of e) table)
     (evlis (arguments-of e) table))))

(define function-of my-car)
; Pre-condition: e is an application expression.
; Post-condition: Returns the function part of the application.

(define arguments-of my-cdr)
; Pre-condition: e is an application expression.
; Post-condition: Returns the arguments part of the application.

(define primitive?
  (lambda (l)
    ; Pre-condition: l is a list.
    ; Post-condition: Returns #t if l represents a primitive function, otherwise #f.
    (eq? (first l) (quote primitive))))

(define non-primitive?
  (lambda (l)
    ; Pre-condition: l is a list.
    ; Post-condition: Returns #t if l represents a non-primitive function (closure), otherwise #f.
    (eq? (first l) (quote non-primitive))))

(define myapply
  (lambda (fun vals)
    ; Pre-condition: fun is a function (primitive or non-primitive), and vals is a list of arguments.
    ; Post-condition: Applies the function to the arguments and returns the result.
    (cond
      ((primitive? fun)
       (myapply-primitive
        (second fun) vals))
      ((non-primitive? fun)
       (myapply-closure
        (second fun) vals)))))

(define myapply-primitive
  (lambda (name vals)
    ; Pre-condition: name is a primitive function, and vals is a list of arguments.
    ; Post-condition: Applies the primitive function to the arguments and returns the result.
    (cond
      ((eq? name (quote cons))
       (my-cons (first vals) (second vals)))
      ((eq? name (quote car))
       (my-car (first vals)))
      ((eq? name (quote cdr))
       (my-cdr (first vals)))
      ((eq? name (quote null?))
       (null? (first vals)))
      ((eq? name (quote eq?))
       (eq? (first vals) (second vals)))
      ((eq? name (quote atom?))
       (:atom? (first vals)))
      ((eq? name (quote zero?))
       (zero? (first vals)))
      ((eq? name (quote add1))
       (if (number? (first vals))
           ((lambda (x) (+ x 1)) (first vals))
           (error "add1: expected a number, got " (first vals))))
      ((eq? name (quote mul))
       (if (and (number? (first vals)) (number? (second vals)))
           (* (first vals) (second vals))
           (error "mul: expected numbers, got " (first vals) " and " (second vals))))
      ((eq? name (quote sub1))
       (if (number? (first vals))
           ((lambda (x) (- x 1)) (first vals))
           (error "sub1: expected a number, got " (first vals))))
      ((eq? name (quote number?))
       (number? (first vals)))
      ((eq? name (quote +))
       (if (and (number? (first vals)) (number? (second vals)))
           (+ (first vals) (second vals))
           (error "+: expected numbers, got " (first vals) " and " (second vals))))
      ((eq? name (quote -))
       (if (and (number? (first vals)) (number? (second vals)))
           (- (first vals) (second vals))
           (error "-: expected numbers, got " (first vals) " and " (second vals))))
      ((eq? name (quote *))
       (if (and (number? (first vals)) (number? (second vals)))
           (* (first vals) (second vals))
           (error "*: expected numbers, got " (first vals) " and " (second vals))))
      ((eq? name (quote /))
       (if (and (number? (first vals)) (number? (second vals)))
           (/ (first vals) (second vals))
           (error "/: expected numbers, got " (first vals) " and " (second vals)))))))

(define :atom?
  (lambda (x)
    ; Pre-condition: x is any Scheme expression.
    ; Post-condition: Returns #t if x is an atom, otherwise #f.
    (cond 
      ((atom? x) #t)
      ((null? x) #f)
      ((eq? (my-car x) (quote primitive))
       #t)
      ((eq? (my-car x) (quote non-primitive))
       #t)
      (else #f))))

(define myapply-closure
  (lambda (closure vals)
    ; Pre-condition: closure is a valid closure created by *lambda, and vals is a list of arguments.
    ; Post-condition: Applies the closure to the arguments and returns the result.
    (meaning (body-of closure)
             (extend-table
              (new-entry
               (formals-of closure)
               vals)
              (table-of closure)))))

(define (my-eval expr)
  (value expr))


;; ---------------------------------------------------------------------------
;; Test Cases
;; ---------------------------------------------------------------------------

; Testing functions one by one
(define (test-value)
  (let ((input '(+ 1 2))
        (expected-output 3))
    (let ((actual-output (value input)))
      (if (equal? actual-output expected-output)
          (display "Test test-value passed\n")
          (display (string-append "Test test-value failed: expected " (write-to-string expected-output) " but got " (write-to-string actual-output) "\n"))))))

(define (test-lambda)
  (let ((input '((lambda (x) (add1 x)) 5))
        (expected-output 6))
    (let ((actual-output (value input)))
      (if (equal? actual-output expected-output)
          (display "Test test-lambda passed\n")
          (display (string-append "Test test-lambda failed: expected " (write-to-string expected-output) " but got " (write-to-string actual-output) "\n"))))))

(define (test-nested-lambda)
  (let ((input '((lambda (x) ((lambda (y) (mul x y)) 2)) 3))
        (expected-output 6))
    (let ((actual-output (value input)))
      (if (equal? actual-output expected-output)
          (display "Test test-nested-lambda passed\n")
          (display (string-append "Test test-nested-lambda failed: expected " (write-to-string expected-output) " but got " (write-to-string actual-output) "\n"))))))

(define (test-let)
  (let ((input '(let ((x 5) (y 10)) (mul x y)))
        (expected-output 50))
    (let ((actual-output (value input)))
      (if (equal? actual-output expected-output)
          (display "Test test-let passed\n")
          (display (string-append "Test test-let failed: expected " (write-to-string expected-output) " but got " (write-to-string actual-output) "\n"))))))

(define (test-let*)
  (let ((input '(let* ((x 5) (y (add1 x))) (mul x y)))
        (expected-output 30))
    (let ((actual-output (value input)))
      (if (equal? actual-output expected-output)
          (display "Test test-let* passed\n")
          (display (string-append "Test test-let* failed: expected " (write-to-string expected-output) " but got " (write-to-string actual-output) "\n"))))))

(define (test-cond)
  (let ((input '(cond ((zero? 0) 1) ((zero? 1) 2) (else 3)))
        (expected-output 1))
    (let ((actual-output (value input)))
      (if (equal? actual-output expected-output)
          (display "Test test-cond passed\n")
          (display (string-append "Test test-cond failed: expected " (write-to-string expected-output) " but got " (write-to-string actual-output) "\n"))))))

(define (test-simple-check)
  (let ((input '(lambda (x) (add1 x)))
        (expected-output #t))
    (let ((actual-output (simple-check input)))
      (if (equal? actual-output expected-output)
          (display "Test test-simple-check passed\n")
          (display (string-append "Test test-simple-check failed: expected " (write-to-string expected-output) " but got " (write-to-string actual-output) "\n"))))))

(define (test-extended-syntax-checker)
  (let ((input '(lambda (x) (add1 x)))
        (env '())
        (expected-output #t))
    (let ((actual-output (extended-syntax-checker input env)))
      (if (equal? actual-output expected-output)
          (display "Test test-extended-syntax-checker passed\n")
          (display (string-append "Test test-extended-syntax-checker failed: expected " (write-to-string expected-output) " but got " (write-to-string actual-output) "\n"))))))

(display (my-eval '(cond ((zero? 5) (add1 10)) (else (sub1 10)))))
(newline)
(display (my-eval '(let ((a 5)) (let ((a 10)) (add1 a)))))
(newline)
(display (my-eval '(let ((a 5)) (let ((a 10)) (add1 b)))))
(newline)

;; Run tests one by one
(test-value)
(test-lambda)
(test-nested-lambda)
(test-let)
(test-let*)
(test-cond)
(test-simple-check)
(test-extended-syntax-checker)



                                  
