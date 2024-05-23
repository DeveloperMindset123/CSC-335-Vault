; Homework 2
; Spring 2024



;; 1. Show how (logical) OR can be implemented using COND
;; 2. Can you implement logical NOT using COND?

;; 3. Exercises 1.7 and 1.8 in Abelson and Sussman

;; 4. Draw the environment diagram for (f add1 3) given

;;     (define (f x y)
;;       (+ (x y) (x (x y))))
;;
;;     (define (add1 x)
;;       (+ x 1))


;; 5. The primitive function list takes a finite number of numbers (say x1, x2, ..., xk) and returns the list
;;    (x1 x2 ... xk) of these numbers in the same order.  Thus if x1 = 10, x2 = 3, x3 = 6, and x4 = 20, the call
;;           (list x1 x2 x3 x4)
;;    returns
;;           (10 3 6 20)

;;    In addition, the primitive function cons takes two arguments -- a number new, and a list lst, and
;;    returns the list formed by inserting new at the front of lst.  Thus (cons 10 (list 3 6 20)) returns
;;    (10 3 6 20).  


;;    Write a function sortFive to input 5 distinct integers and return the list of the input values, sorted
;;    from smallest to largest.  Thus (sortFive 10 5 20 0 6) returns (0 5 6 10 20).

;;    You may NOT use the primitives min or max, or any kind of recursion or iteration, either in sortFive itself
;;    or in any helper functions you define.  

;;    You MUST use cond, let, cons and list.

;; (  HINT: you DO want to define helper functions.)