;; Lecuter notes for 04/09/2024:

; 04/10/2024: We will have a review session to go over the homework problems, so focus on that moving forward
; The EC problem --> would be very hard to solve by attempting to figuring out the logic in advance, a back and forth appraoch between logic and code is needed to solve this problem
; The problem wouldn't be considered a real world problem.

; This may be morphed into project --> referencing to the EC
; consider the following function defined from the class notes
; enumeration
(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

; if we have an interpreter, we call it aexp, you give an inductive definition, build a DS/interpreter that reflects that definition.
; class of algebraic expression is quite straightforward, instead of using familiar arithmetic symbols, he is using @, # and !
; until we have an interpreter defined, there holds to meaning to the symbols defined

; Some examples of expressions belonging to Aexp : 0,1,2,3, ..., (1 @ 2), (1 # 2), ...
; we can read the expression op@ can be read it as (_@_)

; we are also asked to be experts with BNF expressions --> refer to april 9 notes on teams
; a component must belong to the class, equivalent to saying it's the recursive occurence of the BNF form of the Aexp.
; Components (think of components in react, parts that make up a whole)

;example of compoennts would be the following:
;the compoents of, say:
((1 ! (2 # 3))@(4 ! 5))

; would be
1,2,3,4,5
(1 ! (2 # 3))
; etc.


; Request to read the first section of chapter 2, since it will be helpful
; It's useful to setup Aexp as a data strcuture. Data structures have consturctors, selectors and classifiers -- here, we use these to define a user interface:
; consider the following example in scheme
(define (make-@ el ez)
  (list el '#ez))

(define (make-! el ez)
  (list el '!ez))  ; creates a new list with 2 elements, the first element is the value of el, which is passed as an argument to the function. we are simply appending a value to the atom value forming a list


        ; also look into the rat number package --> related to constructors

;the homework problem asks us to redo this interpreter without using the base 10 (meaning not calculating the length) --> referencing to the homework problem that has been assigned.
        ;refer to the following to understand the difference between list and qutote: https://stackoverflow.com/questions/34984552/what-is-the-difference-between-quote-and-list


;selectors
(define (first-operand e)
  (car e))  ;recall that car is used to extract the leftmost element in list, another way of syaing is to retrieve the first element within a list

; we can also define classifiers
(define (#-exp? e)
  (eq? (operator e) '#))  ; --> we can repeat this process for @ and ! as well

; consider if there's such a thing as base 1 numbers
; a plus function, such as myPlus, can be implemented as a repeated implementation (such as the incrementing aspect)
; in multiplication, rather than repeated incrementaiton, we will do repeated adding (since the underlying algorithm for multiplication is adding by a constant several times)
; So what would myPlus be for the stroke notation? --> need to look into this part as well. It will most likely be cons 1

#| Understanding what stroke is in regards to Scheme (not the same as medical stroke) --> in Scheme, stroke typically refers to a function or operation that
 --> is used to apply a transformation or operations to each element of a list, similar to how a stroke in painting can be used to apply color or effect to each part of a canvas
 Although there's no built-in funciton named "Stroke" in Scheme, however the concept is often implemented using higher oreder function 
 what is the proof look like in Aexp? Consider this since this is part of the homework --> refernce to the value funciton definition.
 For strucutural induction, the basis case is the case which does not require recursion, the smallest possible subproblem, according ot the divide and conquer analysis,
 --> when we say we are proving using structural induction
Following description below defines different kinds of inductions we have learned so far:

Structural Induction:
Structural induction is used to prove properties about recursively defined data structures, such as lists, trees, or algebraic data types.
It involves proving that a property holds for a base case (e.g., an empty list) and then showing that if the property holds for smaller or simpler elements,
 --> it also holds for compound or larger elements built from those smaller ones.
In Scheme, structural induction is commonly used to prove properties about lists or recursively defined functions.

Weak Induction:
Weak induction, also known as mathematical induction, is a technique used to prove statements about integers or other ordered sets.
It involves proving a base case (e.g., the statement holds for the smallest integer) and then showing that if the statement holds for a particular integer, it also holds for the next integer.
In Scheme, weak induction is often used to prove properties about numeric functions or algorithms.
Strong Induction:
Strong induction is similar to weak induction, but instead of assuming the statement holds for just one previous case, it assumes the statement holds for all smaller cases up to a certain point.
It involves proving a base case (e.g., the statement holds for the smallest integer) and then showing that if the statement holds for all integers up to a particular point,
 --> it also holds for the next integer.
In Scheme, strong induction is less common but can still be used for proving more complex properties about integers or other ordered sets.

The absolute worst thing is filthy code, it's importnat to learn to write clean code. 75% of every software budget involves dealing with filthy code, code that's clever and lacks documentation, trying to track down security flaws in code that was very badly written.

It is recommended to work before you pursue graduate school, so that having a job while funding education for grad school, or do both simultaneously, as that was the original plan

Expected time analysis --> very importnat in the industry

|#





