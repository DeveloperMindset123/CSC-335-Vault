;; CSc 335
;; Spring 2024
;; Project

;; Released April 16 2024
;; Due by 23:59:59 May 18 2024, via email 


;; I ask for complete developments and proved, working code for three problems:

;; 1.  Write and prove correct a program to input an LD number (as previously defined) which returns a list of all lists of
;; 1s and 2s which map to that number.  Use this program to check your solution to the takehome part of the midterm. 

;; 2.  Write and prove correct a syntax checker for TLS-scheme, as specified in HW 11.

; As part of additional context, note that the following is the tls-scheme starter code that has been provided:

; CSc 335
; first scheme interpreter


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; tls-scheme, from chapter 10 of tls

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; need to allow redefinition of initial bindings in r5rs as delivered
; by drracket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



; auxiliary functions

#|
build, first, second and third and atom are basic utility functions to manipulate lists and check atomic values
|#
(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 (quote ())))))

; Note that this has been defined earlier within tls itself refer to the notes from allChapters --> as the name suggest, used to extract the first element within a list
(define first car)


; as the name suggest, used to retrieve the second element within the list
(define second cadr)

; as the name suggest, used to retrieve the third element within the list
(define third caddr)

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))



; environments implemented as tables
#|
Functions such as lookup-in-table, extend-table, lookup-in-entry, new-entry and vals manage environments, which are mapping of variable names to their values.
|#

; purpose : to find the values associated with a name in a table (environment)
; explanation : checks if name exists in the current table, if not recursively checks the rest of the table
(define lookup-in-table
  (lambda (name table table-f)
    (cond 
      ((null? table) (table-f name))
      (else (lookup-in-entry name
                             (car table)
                             (lambda (name)
                               (lookup-in-table name
                                                (cdr table)
                                                table-f)))))))

;TODO: this function needs to be defined, refer to the tls to see how the function was implemented.
; purpose : to add a new entry to the table
; explanation : uses a cons to add a new entry to the front of the table

(define extend-table cons)


; purpose : to find the values assocaited with a name in a single entry of the table
; explanation : uses a helper function to look for name in a specific entry (a pair of lists : names and values)
(define lookup-in-entry
  (lambda (name entry entry-f)
    (lookup-in-entry-help name entry
                          ;(names entry) --> this parameter value is no longer being used, lookup-in-entry-help takes in 3 parameters instead
                         ; (vals entry) --> this parameter value is no longer being used, lookup-in-entry-help takes in 3 parameter to follow the pair format of (name value)
                          entry-f)))


; purpose : helper function for lookup-in-tnry
; explanation : recursively checks each name in the list until it finds a match or runs out of names
(define lookup-in-entry-help
  (lambda (
     name 
     ; instead, define a new parameter named entries
     entries 
     ;names --> commented it out since we will not store the names in a seperate list of it's own
     ;vals  --> commented it out since we will not store the corresponding vals in a seperate lists of it's own
     entry-f)
    (cond
      ((null? names) (entry-f name))
      ((eq? (car names) name) (car vals))
      (else (lookup-in-entry-help name
                                  (cdr names)
                                  (cdr vals)
                                  entry-f)))))



; purpose : to crate a new-entry
; explanation : uses build to craete a new entry, which is a pair of name and value lists.
; define the function logic for new-entry, right now it is simply initialized
(define new-entry
     ; use map and cons to create pairs from the lists of names and values
     (lambda (names values)
     ; recall that we build lists using cons, so we are iterate --> ntoe the following in regards to the map primitive in scheme --> map is a built in function that takes a function and a list as it's argument, and returns the list that results by applying the function to every element of the list, in this case, the function we are passing in is cons and the lists it is being applied to are names and values, particularly creating pairs based on each of their individual elements.
          (map cons names values)
     )
)

; purpose : to extract names and values from an entry
; explanatiion : names returns a list of names, vals returns the list of values from an entry (since the pair are in name value format, think similar to a hashmap)
#| --> these functions are no longer needed since we are no longer worried about storing names and vals on seperate lists of their own.
(define names
  (lambda (entry) (car entry)))

(define vals
  (lambda (entry) (cadr entry)))
|#


; the top level of the interpreter
#|
- value and meaning serves as the entry points for evaluating expressions
|#

; purpose : entry point to evaluate an expression
; explanation : calls meaning with an expression 'e' on the empty table (meaning is a helper function in this case)
(define value
  (lambda (e)
    (meaning e (quote () ))))

; purpose : to determine the meaning of an expression
; explanation : Dispatches the expression to the appropraite action based on it's type
(define meaning
  (lambda (e table)
    ((expression-to-action e) e table)))


; supporting functions for the intepeter

; syntax-directed dispatch on expression
#|
- The functions (*const, *quote, *identifier, *lambda, *cond, *application) define the semantics for different types of expressions. --> in context to programming, semantics refers to pieces of code.
|#

; purpose : this determines what action to take based on the expression type, if atom, call on the atom-to-action function and otherwsie, call on the list-to-action function --> in other words, checks if the expression is an atom or a list and dispatches accordingly.
(define expression-to-action
  (lambda (e)
    (cond 
    ; recall that we are making use of the utillity function atom --> which is anything that's not a pair and not null
      ((atom? e) (atom-to-action e))
      (else (list-to-action e)))))

; purpose : function to handle if expression (e) happens to be an atom --> in order for the atom-to-action function to be called, it must first pass the atom? check, and then it will check and compare 
; explanation : matches the atom to a known constant or identifier and dispatches to the appropriate handler --> so in this case, it essentially goes down the list using cond to determine whether expression matches any of the primitive or not, if so, return *const, and otherwise, return *identifier
(define atom-to-action
  (lambda (e)
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
      ; Add the square primitive
      ((eq? e (quote square)) *const)
      (else *identifier))))

; purpose : to handles lists in expressions
; explanation : checks the first element fo the list to determine the type of expression and dispatches accordingly, this function gets executed in the instance that the expression doesn't happen to be an atom
(define list-to-action
  (lambda (e)
    (cond
    ; conditional statement used to check if the car of e returns an atom value, if so, apply the following conditionals (perhaps this should follow the same conditional statements as atom-to-action within the cond block?)
      ((atom? (car e))
       (cond 
       ; if the car of the expression results in an atom, apply the following conditional checks, and if it doesn't happen to be any of them, apply the *application pointer --> note that if the expression happens to match quote, it calls on the quote pointer, if it happesn to match lambda, it calls on the lambda pointer and if it happens to match cond, it calls on the cond pointer.
         ((eq? (car e) (quote quote))
          *quote)
         ((eq? (car e) (quote lambda))
          *lambda)
         ((eq? (car e) (quote cond))
          *cond)
         (else *application)))
      (else *application))))


; operational semantics -- the definitions of the action functions
; purpose : function to handle the const values (which is returned based to atom-to-action function) --> 
(define *const
  (lambda (e table)
  ; *const takes in 2 parameters, expression and the existing table, checks if the expression happens to be anumber, or boolean value, and otherwise, build it as a primitive instead --> in simpler terms, returns the value of the constant or tags it as a primitive instead.
    (cond 
      ((number? e) e)
      ((eq? e #t) #t)
      ((eq? e #f) #f)
      (else (build (quote primitive) e)))))

; purpose : to handle quoted expressions
; explanation : returns the secon part of the quoted expressions. --> (TODO: hint, text-of should follow the logic of the second utils function)
(define *quote
  (lambda (e table)
    (text-of e)))

(define text-of second)


; purpose : to handle identifiers by looking them up at a table.
; explanation : looks up the value of the identifier in the table
(define *identifier
  (lambda (e table)
    (lookup-in-table e table initial-table)))


; note that as (car (quote ())) throws an error, this definition
; amounts to saying that looking anything up in the initial table
; is impossible.
(define initial-table
  (lambda (name)
    (car (quote ()))))

; purpose : to handle lambda expressions
; explanation : constructs a non-primitive lambda function with the given body and environment
(define *lambda
  (lambda (e table)
    (build (quote non-primitive)
           (cons table (cdr e)))))

(define table-of first)

(define formals-of second)

(define body-of third)


; cond is a special form that takes any number of 
; cond-lines ...  if it sees an else-line, it treats
; that cond-line as if its question part were true.

; purpose : to handle conditional expressions
; explanation : evaluates each condition in turn until one is true, then returns the corresponding value.
(define evcon
  (lambda (lines table)
    (cond 
      ((else? (question-of (car lines)))
       (meaning (answer-of (car lines))
                table))
      ((meaning (question-of (car lines))
                table)
       (meaning (answer-of (car lines))
                table))
      (else (evcon (cdr lines) table)))))


(define else?
  (lambda (x)
    (cond 
      ((atom? x) (eq? x (quote else)))
      (else #f))))

(define question-of first)

(define answer-of second)


(define *cond 
  (lambda (e table)
    (evcon (cond-lines-of e) table)))

(define cond-lines-of cdr)


; purpose : to evaluate a list of arguments
; explanation : evaluates each argument in the list and returns a list of the results.
(define evlis
  (lambda (args table)
    (cond 
      ((null? args) (quote ()))
      (else
       (cons (meaning (car args) table)
             (evlis (cdr args) table))))))


; puprose : to handle function applications
;  applies the function to it's arguments after evaliating both
(define *application
  (lambda (e table)
    (myapply
     (meaning (function-of e) table)
     (evlis (arguments-of e) table))))

(define function-of car)

(define arguments-of cdr)




(define primitive?
  (lambda (l)
    (eq? (first l) (quote primitive))))

(define non-primitive?
  (lambda (l)
    (eq? (first l) (quote non-primitive))))


; purpose : to apply function to it's arguments
; explanation : checks if the function is primtive or non-primtiive and applies it accordingly
(define myapply
  (lambda (fun vals)
    (cond
    ; if the function is ineed a primitive, call on the myapply-primitive function
      ((primitive? fun)
       (myapply-primitive
        (second fun) vals))
        ; in the case that the function happens to be a non-primitive, call on the function myapply-closire
      ((non-primitive? fun)
       (myapply-closure
        (second fun) vals)))))


; purpose : to apply a primtiive function
; explanation: matches the name to a known primitive operation and applies it to it's arguments.
(define myapply-primitive
  (lambda (name vals)
    (cond
      ((eq? name (quote cons))
       (cons (first vals) (second vals)))
      ((eq? name (quote car))
       (car (first vals)))
      ((eq? name (quote cdr))
       (cdr (first vals)))
      ((eq? name (quote null?))
       (null? (first vals)))
      ((eq? name (quote eq?))
       (eq? (first vals) (second vals)))
      ((eq? name (quote atom?))
       (:atom? (first vals)))
      ((eq? name (quote zero?))
       (zero? (first vals)))
      ((eq? name (quote add1))
      ; defines the function logic for add1 as a lambda function
       ((lambda (x) (+ x 1)) (first vals)))
      ((eq? name (quote mul))
       (* (first vals) (second vals)))
      ((eq? name (quote sub1))
       (sub1 (first vals)))
;;;; TODO: deliberate error: ask class to figure out how to repair it.  
      
      ((eq? name (quote number?))
       (number? (first vals)))
       ; added square primitive
       ((eq? name (quote square))
       ((lambda (x) (* x x)) (first vals))
       )
       
       )))


(define :atom?
  (lambda (x)
    (cond 
      ((atom? x) #t)
      ((null? x) #f)
      ((eq? (car x) (quote primitive))
       #t)
      ((eq? (car x) (quote non-primitive))
       #t)
      (else #f))))

; purpose : to apply a non-primitive (lambda) function.
(define myapply-closure
  (lambda (closure vals)
    (meaning (body-of closure)
             (extend-table
              (new-entry
               (formals-of closure)
               vals)
              (table-of closure)))))
; explanation : evaluates the body of the lambda with the provided arguments in the extended environment.

#|
As part of the project requirement, it will be helpful to complete hw 11 --> once complete, add it as part of the explanation for the project
 Homework 11


What you will gain from this assignment is a much deeper understanding of the TLS interpreter
than you can possibly gain by just reading it.  After doing a good job on this, you will be
in much better shape for the TLS component of the final.


Problems
;--------
;
1.  Add a new primitive to TLS
We can add a new primitive named square --> look at the comments in the function to see how it was implemented.


;
2.  Change the representation of bindings in the system to explicit pairs of the form (name value)
Refer to the code block  for the functions lookup-in-entry, lookup-in-entry-help, new-entry, extend-table, initial-table to see the modifications that has been made.

Understanding what the question means when it asks us to change the representation of bindings in the system to explciit pairs of the form:
--> Changing the representation of bindings in the system to explciit pairs of the form '(name value)', means that instead of having seperate lists for variable names and their corresponding values, each binding is represented as a pair (a two element list or a cons cell), where the first element is the variable name and the second element is the value itself, similar to a hashmap.

--> prior to the change in the representaiton of bindings is:
(define names (quote (a b c))) ; we have a list for names
(define vals (quote (1 2 3))) ; we have a list for vals

--> representaiton after the change has been implemented
(define bindings '((a . 1) (b . 2) (c . 3)))

Why is this change even neccessary?
- clarity and simplicity : using explicit pairs makes it clear which value corresponds to which variable name.
- efficiency : Accessing a value given it's name can be more straightforward because each pair directly associates a name with it's value.
- consistency : Many programming language and data structure uses key-value pairs (hashamps in javascript and dictionaries in python)
;
3.  Write, and prove correct, a syntax checker for TLS-Scheme.  Use the inductive definition of TLS-Scheme
     given in class.  You will need to give a complete specification for your program, which should
     be written in R5RS.  
;
     I suggest starting with a simpler program - let's call it simple-check - which does not check for
     arity correctness of applications or unbound variables.  When you understand how to do this, then
     extend your solution to reject programs which either

       a. contain unbound symbols, or
       b. contain applications in which the number of arguments in some phrase (f x1 ... xn) does not
          match the number of formal parameters given in the definition of f

;
     Thus the occurrence of a symbol, say x, in an expression when there is no wrapping occurrence of
     (lambda (x...) ...), would be cause your program to reject the input, and any occurrence of the
     kind
;
       ((lambda (x y) ... ) p), or 

       ((lambda (f x) ... (f arg) ...  )
          (lambda (x y) ...)
          val-for-x)
;
    (in which f, bound to a closure, is applied to a number of arguments inconsistent with the closure)
    would also cause your program to reject the input.  

;
;
 4.   Add let to TLS, and prove that the resulting interpreter (for the language TLS-let, that is, TLS with let) is correct.  
|#


;; 3.  Write and prove correct an interpreter for TLS extended by let*.

#|
The purpose of an scheme interpreter is simple : 
- it is used to understand at a low level how scheme code is interpreted and executed. By constructing an interpreter, you get to see this step-by-step process of evaluating expressions, managing environments, applying functions and handling various forms of data.

Environment subsystem --> to manage variable bindings (environments) and look up variable values
Entry constructors and selectors --> to create and manipulate entries in the environment.
Top level of an interpreter --> the main functions that start the evaluation process.
Expression Dispatch --> To classify and dispatch expressions to the correct evaluation function.
Action Functions --> To evaluate specific types of expressions.
Applying functions --> To handle the application of functions to arguments.

If we were to put everything together, we would get the following flow:
When you evaluate a Scheme Expression using this interpreter, here's what happens step by step:
1. start evaluation : you call value with an expression. This initializes the evaluation with an empty environment.
2. determine meaning : value calls meaning, which decides what kind of expression it is (atom or list) using expression-to-action
3. Handle expressions : Depending on the type of expression that is being provided in the parameter:
          - Atoms : atom-to-action handles constants and identifier.
          - Lists : list-to-action handles function applications and special forms.
4. evaluate component : For complex expressions (like function applications), sub-expressions (like functions and arguments) are evaluated recursively --> as we can see, the root of the scheme interpreter relies on recursion.
5. Apply functions : If it's a function application, '*application' and 'myapply' handle applying the function to it's arguments.
6. Return Result : The final result of the expression is returned.
|#