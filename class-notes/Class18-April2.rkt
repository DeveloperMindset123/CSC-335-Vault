; consider the following function
; Continue the notes absed on what's written on Mciro

#|
- In Scheme, defining a funciton with "*" in its name, like (subst* tree old new), does not offer any special meaning

|#
(define (subst* tree old new)  ;in this case, this function takes in 3 paramter values, tree, old and new
  (cond ((null? tree)  ;check if the current tree is null or not
         tree)  ;output the tree