;; Homework 11


; What you will gain from this assignment is a much deeper understanding of the TLS interpreter
; than you can possibly gain by just reading it.  After doing a good job on this, you will be
; in much better shape for the TLS component of the final.
;
;
;
;
; Problems
;--------
;
; 1.  Add a new primitive to TLS
;
; 2.  Change the representation of bindings in the system to explicit pairs of the form (name value)
;
;
; 3.  Write, and prove correct, a syntax checker for TLS-Scheme.  Use the inductive definition of TLS-Scheme
;     given in class.  You will need to give a complete specification for your program, which should
;     be written in R5RS.  
;
;     I suggest starting with a simpler program - let's call it simple-check - which does not check for
;     arity correctness of applications or unbound variables.  When you understand how to do this, then
;     extend your solution to reject programs which either
;
;       a. contain unbound symbols, or
;       b. contain applications in which the number of arguments in some phrase (f x1 ... xn) does not
;          match the number of formal parameters given in the definition of f
;
;
;     Thus the occurrence of a symbol, say x, in an expression when there is no wrapping occurrence of
;     (lambda (x...) ...), would be cause your program to reject the input, and any occurrence of the
;     kind
;
;       ((lambda (x y) ... ) p), or 
;
;       ((lambda (f x) ... (f arg) ...  )
;          (lambda (x y) ...)
;          val-for-x)
;
;    (in which f, bound to a closure, is applied to a number of arguments inconsistent with the closure)
;    would also cause your program to reject the input.  
;
;
;
; 4.   Add let to TLS, and prove that the resulting interpreter (for the language TLS-let, that is, TLS with let) is correct.  
;





