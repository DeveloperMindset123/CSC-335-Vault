#!/usr/bin/racket
#lang racket

;The following was the question from the quiz
;;This is a closed book, closed notes, no devices quiz.

;;Problem 1 (2 points)
;;;;Speciy, design, develop/prove and code a purely functional PROPERLY recursive R5RS scheme prodcedure named larget-from-2

;;With argument n, where n is a non-negative integer, which returns the largest 2-digit number m which can be formed from the digits of n.
;;This (largest-from-2 314) = 43 and (largest-from-2 9) = 99 and (largest-from-2 5555) = 55
;;Your program must work in a single pass over the input n.

;Note the following in regards to divide and conquer strategy in funcitonal programming:
;;;This paradigm, divide-and-conquer breaks a problem into subproblems that are similar to the original problem.
;;;Recursively solves the subproblems and finally combines the solution to the subproblems to solve the original problem.

;Note the constraint, the function must make a single pass over the program

;;The most basic problem would be to have two integers
;; As the divide and conquer strategy suggests, we need to break donw this question to the smallest solvable problem.
;; The smallest possible subproblem this can be broken down into is where only two integers have been provided
;; Within this small subproblem, we can also implement the solution to handle the base case where duplicate values appear

;;implement the smallest helper function, note that due to constraints pertaining to Scheme, we can only iterate through 

(define (two-biggest-of-2 a)
  ;conditional alongside predicate to check the smallest number, retrieve and compute the rightmost and leftmost number
  (cond ((or (< (modulo a 10)   ;check the rightmost value of the two digit
                (quotient a 10)) (= (modulo a 10) (quotient a 10))) ;in the case that the values are equal to one another
         (+ (* (quotient a 10) 10) (modulo a 10)))  ;--> logic is incorrect, the onditinal statement is checking (a % 10 < a // 10) --> mathematically, therefore, the largest number in the value returned should be placed at the leftmost position
        
         ;check the leftmost value of the two digit --> essentially checking if ((modulo a 10) < (quotient a 10))
             ;add the second statement --> if so, arithemtically calculate the modulo, multipy by 10 to shift the digit one position to the left and then add that to the quotent a 10
        
        ((= 0 (quotient a 10)) (+ (* a 10) a)) ; this is in the case that a consist of only a single string of digit, in which return a duplicate value

        ;third conditional assumes that (> (modulo a 10) (quotient a 10)) --> matheamtically, (a % 10 > a // 10), which would mean the 2nd digit is larger than the first, in which case, it should be placed at the leftmost (aka 0th index)
        (else (+ (* (modulo a 10) 10) (quotient a 10)))))

;test out the function to check if it works as intended --> I will run 2 tests here to verify the result
(display "Output result for two-biggest-of-2 helper Test 1:")
(newline)
(two-biggest-of-2 28)  ;;should return 82

(display "Output result for two-biggest-of-2 helper Test 2:")
(newline)
(two-biggest-of-2 82) ; should also return 82

(display "Output result for two-biggest-of-2 helper Test 3, checking if it recognizes duplicate values or not:")
(newline)
(two-biggest-of-2 5) ;should output 55
(two-biggest-of-2 55) ;should also output 55

;theoretically, this should handle all the edge cases

; Now that we have defined the smallest value, we will just have to build our way up from there, that will be our divide and conquer strategy
(define (two-biggest-of-n a largest-so-far)  ;;initial value for largest-so-far will have to be zero
  ; we will have to keep track of the largest-so-far as we iterate through the string of digits that is of n length within single pass to find the largest value
  ;due to the constraint of single pass, we will need to implement a method such that we avoid the multiple scans and simultaneously reduce the value each each time

  ;set the largest-so-far value, initialized
  ;(+ (largest-so-far) (two-biggest-of-2 a))

  ;define the termination condition as checking when we have reached the end of n
  ;(cond ((> (quotient a 10) 0)  ;think of this as the looping condition  --> replaced termination case (original: (> (quotient a 10) 0), updated: (if (= n 0)

  ;define the termination case/base case for the function -->note that we will have to utilize nested conditionals for this
  (if (= 0 a) largest-so-far

;else, do the following, check if the largest-so-far happens to be smaller than the largest-so-far value currently
      ;--> note that due to the logic constraint of two-biggest-of-2 a, we will need to only pass in two values, otherwise it will return the error "application : not a procedire; expected a procedure that can be applied to arguments
      (if (> (two-biggest-of-2 a) largest-so-far) (+ (* largest-so-far 0) (two-biggest-of-2 (+ (* (modulo a 10) 10) (modulo (quotient a 10) 10))))  ;ensure that modulo function is correctly called

          ;else, recurisvely reduce the function since we updated the new max
          (two-biggestof-n (quotient a 10) largest-so-far))))

;make test call to check if the function works as intended
(newline) (display "Output result for two-biggest-of-n using helper two-biggest-of-2 Test 1, using only 3 numbers:") (newline)
(two-biggest-of-n 289 0)  ;;should output 98

(newline) (display "Output result for two-biggest-of-n Test 2, checking if it recognzes duplicate values or not") (newline) (two-biggest-of-n 444444 0) ;should output 44
(display "My logic is lacking, I didn't take into consideration that the largest two values may not be placed next to one another") (newline)
(newline) (display "Output result for two-biggest-of-n Test 2, checking if it recognizes values ordered in the other way around") (newline) (two-biggest-of-n 945 0) ;should output 95