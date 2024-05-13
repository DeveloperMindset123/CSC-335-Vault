;; Tenth Homework Set
;; CSc 335


; (Due to Friedman and Felleisen, the authors of TLS)

; An L-expression is an S-expression which is either:
;   (AND l1 l2), or
;   (OR l1 l2), or
;   (NOT l1), or
;   an arbitrary symbol, which we call a variable

; Here l1 and l2 are arbitrary L-expressions, so this is an inductive definition once 
; we add 'and nothing else is an L-expression'

; (a) Write and certify a function lexp? which checks whether an S-expression is an L-expression.

; (b) Write and certify a function covered? of an L-expression lexp and a list of symbols los that tests
;     whether all the variables in lexp are in los.

; (c) For the evaluation of L-expressions we need association lists, or alists.  An alist for
;     L-expressions is a list of (variable, value) pairs.  The variable component is always a symbol, and
;     the value component is either the number 0 (for false) or 1 (for true). Write and certify a function
;     lookup of the symbol var and the association list al, so that (lookup var al) returns the 
;     value of the first pair in al whose car is eq? to var.

; (d) If the list of symbols in an alist for L-expressions contains all the variables of an L-expression
;     lexp, then lexp is called _closed_, and can be evaluated with respect to this alist.  Write and certify the
;     function value of an L-expression lexp and an alist al, which, after verifying that lexp is closed,
;     determines whether lexp means true or false.  If lexp is not closed in al, then (value lexp al)
;     should return the symbol not-covered.
