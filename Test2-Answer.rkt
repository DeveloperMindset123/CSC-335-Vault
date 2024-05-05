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
(define (largest-from-2 a largest-so-far)  ;;initial value for largest-so-far will have to be zero
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
          (largest-from-2 (quotient a 10) largest-so-far))))

;make test call to check if the function works as intended
(newline) (display "Output result for two-biggest-of-n using helper two-biggest-of-2 Test 1, using only 3 numbers:") (newline)
(largest-from-2 289 0)  ;;should output 98

(newline) (display "Output result for two-biggest-of-n Test 2, checking if it recognzes duplicate values or not") (newline) (largest-from-2 444444 0) ;should output 44
(display "My logic is lacking, I didn't take into consideration that the largest two values may not be placed next to one another") (newline)
(newline) (display "Output result for two-biggest-of-n Test 2, checking if it recognizes values ordered in the other way around") (newline) (largest-from-2 945 0) ;should output 95


;another potential method is to use one function to keep track of one maximum and another to keep track of the other maximum, once the iteration finishes, compare the two using the first helper function defined named two-biggest-of-2


; The code below, makes modification to the previous solution to take into consideration the issue in regards to the logical flaw to my algorithm, this is the solution generated based on ChatGPT
; The following list of modifications were made:
;;;;1. Base case adjustment: Recognize the end of recursion not just by checking if `a` is 0 but by considering if `a` has no more digits left to process.
;;;;2. Accumulators: Use accumulators to keep track of the two largest digits found so far. Since we cannot use direct assignments, we integrate these accumulators into the function parameters.
;;;;3. Adjusting for single digits and duplicate value string of digits: Ensure that the number n is a single digits are all duplicates, the function returns nn is len(n)=1 or n[0]...[k] === n[0] (meaning all the digits are the same), return nn

;The following is the solution according to Troeger

;solution developed by divide and conquer
;;here's the idea: assuming n has at least 3 digits, construct the 2 largest digit numbers from (modulo n 10) and the --> retrieve the rightmost value by using modulo n 10
;; digits of the number returned by the call on (quotient n 10) --> retrieve everything but the rightmost value using quotient n 10 --> since there's only 2 digits, it will return the leftmost value in this case

;;p and q are digits
;;p is not 0, the question stated we are dealing with nonnegative integers
(define (make-num p q)
  (+ (* 10 p) q))   ;;this simply forms an integer pq, note the ordering in which the parameter is provided is relevant to the integer formed, meaning if we pass is (make-num q p) --> it will return qp
  ;the two-biggest-of-2 function previously defined can be further broken down it seems

(define (largest-2-digit-number-from-2-digits p q)
  ;use cond to check and determine the largest 2 values, pretty straightforward logic here
  (cond ((>= p q) (make-num p q))  ;in the case that p >= q, form a number pq, this will handle the case of duplicate values as well
                  ;else --> in the case that p < q, form a number qp
                  (else (make-num q p))))  ;--> don't make the mistake of wrapping paramters around paranthases


;now, we will utilize the largest-2-digit-number-from-2-digits to develop the function largest-2-digit-number-from-3-digits n
;;assuming n>=0 and n is an integer
(define (largest-2-digit-number-from-3-digits p q r)  ;it seems this question had to define a 3rd helper function to take into consideration how it owuld go about determining a number that contains 3 digits, which will be recursively called upon to determine largest-2 from n digits
  (let ((m (min p q r))) ;--> we are using let binding to initialize a variable named m, and we are using the primitive min to determine the smallest value amongst the 3 digits provided, note that let bindings can only be used within the scope of it's parent function, since it serves as a local variable
  ; define the conditinal statements to go with it
  (cond ((= m r) ;in the case that the smallest value happens to be r, return p and q using the largest-2-digit-number-from-2 helper, by passing in p and r as it's parameter values
         (largest-2-digit-number-from-2-digits p q))  ;;we don't have to worry about the underlying logic here as we have already defined it within the previous variable

        ;define the 2nd conditional statement
        ((= m q) ;in the case that the smallest value happens to be q, form the largest using the helper passing in p and r as it's parameters
         (largest-2-digit-number-from-2-digits p r))  ;;we don't ahve to worry about the underlying logic here as we have already defined it within the previous variable

        ;define the third conditional statement, in the case that the smallest value happens to be p
        ((= m p)
         (largest-2-digit-number-from-2-digits q r))))) ;we have handled all the cases of determining the largest 3 digits utilizing the min primitive


;lastly, we can utilie the main fucntion where n digits is provided (remember scheme refers to n as a string of digits)
(define (largest-2-digit-number-from-n-digits n)
  ;write the edge case to handle where n is a single digit number, in whcih case we form and return a number with it's duplicate
  (cond ((< n 10) (make-num n n))
         ((< n 100) (largest-2-digit-number-from-2-digits (quotient n 10) (modulo n 10))) ; this is handling the case where n consists of 2 digits, I did not think of this method earlier
         (else (let ((m (largest-2-digit-number-from-n-digits (quotient n 10)))) ; Here, m represents the reduced form of n, with the rightmost digit removed
            (let ((p (quotient m 10))  ;set p to consist of all the leftmost values except the rightmost value (we are reducing m by 1 digit (in this case rightmost digit) again)
                  (q (modulo m 10)) ;retrieve the rightmost digit from m, meaning we are extracting the 1 digit that was reduced by p
                  (r (modulo n 10))) ;in regards to r, we are retrieving the rightmost value of the original n, this will handle the case such that the n digit that was reduced doesn't lose it's rightmost value, by doing so \n
                  ;we will pass in the three newly created local variables within the helper function largest-2-digit-number-from-3-digits to determine the largest 2 possible digits that can be formed from the rightmost 3 values

                  ;recursively call on the helper function
                  (largest-2-digit-number-from-3-digits p q r))))))  ;;this function will handle determining the largest possible 2 digit value we can form (however, do consider how the process in regards to forming 
                  
                     
;now, call on the functions to see if it works as intended
(newline) (display "Output to the updated function call largest-2-digit-number-from-n-digits 2869") (newline)
(largest-2-digit-number-from-n-digits 2859) ;should output 98

(newline) (display "Output to the updated function call largest-2-digit-number-from-n-digits 1029372") (newline)
(largest-2-digit-number-from-n-digits 1029372) ;should be 97


;Addtional logic explaining how each of the function works as intended, according to chatgpt:

; 'make-num' --> This function takes two digits, 'p' and 'q', and combines them into two digit numbers with 'p' as the tens place and 'q' as the ones place.
;            --> It simply multiplies 'p' by 10 and adds 'q' to it. The order of the parameters matters because ('make-num p q') does not neccessarily equal ('make-num q p'), unless p = q


; largest-2-digit-number-from-2-digits --> This function determines the larger of the two digits, 'p' and 'q', and returns them as a two-digit number, ensuring the larger digit in the tens palce.
; --> It uses a conditional ('cond') to check which of 'p' or 'q' is larger (or if they are equal, in which case it doesn't matter which one is placed first) and calls 'make-num' accordingly.

; largest-2-digit-number-from-3-digits --> This function extends the logic to three digits, 'p', 'q', and 'r'. It uses 'min' function to find the smallest of the
; --> three digits and then determines the largest two-digits and then determines the largest two-digit number that can be made from the remaining two larger digits by calling 'largest-2-digit-number-from-2-digits'.
; --> This approach effectively discards the smallest of the three

; largest-2-digit-number-from-n-digits --> This is the main function that works with any nonnegative integer 'n'. It handles 3 cases:
; --> if 'n' is a single digit number, it duplicates the digit to form a two digit number with both places occupied by the same digit.
; --> if 'n' is a two-digit number, it simply uses 'largest-2-digit-number-from-2-digits' to potentially rearrange the digits (if needed) to ensure the larger digit is in the tens place.
; --> for numbers with more than two digits, it recurisvely reduces 'n' by stripping off the rightmost digit and calling itself with this reduced number unti it fits one of the simpler cases above.
; --> it then uses 'largest-2-digit-number-from-3-digits' to incorporate the rightmost digit that was stripped off in the last recurisve call, ensuring that, across the recursive calls, it eventually considers all digits of 'n' to find the two largest ones and form the largest possible two digit number.

;The following three view will help clear out the confusion in regards to determining the tree recursion based structure

;largest-2-digit-number-from-n-digits(2859)
;|
;|--- largest-2-digit-number-from-n-digits(285) [Remove rightmost digit]
;     |
;     |--- largest-2-digit-number-from-n-digits(28) [Base case: Two digits, compute directly]
;     |    |
;     |    |--- returns 82 (from largest-2-digit-number-from-2-digits(2, 8))
;     |
;     |--- Now, m = 82, p = 8 (quotient of 82), q = 2 (modulo of 82), r = 9 (original n's rightmost digit)
;          |
;          |--- largest-2-digit-number-from-3-digits(8, 2, 9)
;               |
;               |--- returns 98 (largest of 8, 2, and 9)


;That was the end of the recursive implementaiton, below is the iterative based implementation:
; Solution using an accumulator

;; n > 0 is an integer
(define (ilargest-2-digit-number-from-digits-of-n n)

  ;;rsf is the largest 2 digit number formed from the alterady processed digits --> important to note as rsf will be one of the parameter values within the helper function
  ;;N = [nyp][ap]

  ;;;; extended bracket noatation, which allows (for example) 123000 to be written as [123][000], similarly 123004 can be written as [123][004]
  ; --> the idea here is to break down the numbers such that half of it is set ot the not yet processed while the other half has already been processed

  (define (iter nyp rsf)
    (newline)(display rsf)(newline)  ;we will output the largest 2 digit number that has been found from the already processed array
    (cond((zero? nyp) rsf) ;output rsf once nyp equals zero, this will be our termination case, note that functions defined using let scope cannot be accessed outside of the let scope
         (else (let ((l-rsf (quotient rsf 10))  ;take the left value of the largest 2 digit (this should theoretically be the largest value in the entire string of digits that is passed as n)
                     (r-rsf (modulo rsf 10))) ;take the right value of the largest 2 digits (this should theoretically be the second largest value in the entire string of digits that is passed as n)

                 ;next, we will recursively call on the helper function (recall that tail recursion is a form of iterative approach as well)
                 ;note that for the iter function, since it is being called upon recurisvely, we are reducing nyp by 1 and taking the rightmost digit that was removed and passing it as a parameter value to largest-2-digits-number-from-3-digits
                 (iter (quotient nyp 10) (largest-2-digit-number-from-3-digits (modulo nyp 10) l-rsf r-rsf)))) ;--> do not make the mistake of wrapping parameters around with paranthases, we will utilize the helper function we defined during the recursion to check if the rightmost digit of the not yet processed string of digit forms a new largest 2 value compared to the existing value

         ))

  ;exiting the function body for (define (iter nyp rsf)) --> essentially, we will need to implement the loop as a form of recursion since we are not allowed to use loops, but do note that all forms of loops can be implemented in the form of recursion, but for the sake of recursion, no direct recursion should be implemented for iterative solution
  ;conditional statement for the original function (ilargest-2-digit-number-from-digits-of-n n)

  (cond ((< n 10) (make-num n n)) ;the base case in this case remains the same as before, if n is a value less than 10, return it's duplicate value
        ((< n 100) (largest-2-digit-number-from-2-digits (quotient n 10) (modulo n 10)))  ;similarly, if n happens to be a 2 digit number, call on the helper function largest-2-digit-number-from-2-digits
        ;otherwise, call on the iterative function defined above, passing in the neccessary parameter values as need
        ;here, we are reducing n (which is also nyp in this case, at the initial condition, nyp=n, ap=0 and at the end of termination, ap=n while nyp=0 (the values gets reversed)
        ;in the call to the iter function, since in the recursion, we are already reducing the value by 1, we are instead recuding the value by 2 positions and passing them into the helper largest-2-digit-from-2-digits in order to form the pair of the current largest value that can be found wtihin ap
        (else (iter (quotient n 100) (largest-2-digit-number-from-2-digits (modulo (quotient n 10) 10) (modulo n 10))))))



;;make the function call (note that rsf stands for result so far)
(newline) (display "Iterative function call to test the value 2869") (newline)
(ilargest-2-digit-number-from-digits-of-n 2869) ;;output should be 98

;The following represents a treelike structure of the iterative call that is taking place
;ilargest-2-digit-number-from-digits-of-n 2869
;│
;└─► iter 28 69
 ;  │
  ; └─► iter 2 69
   ;   │
    ;  └─► iter 0 69 (End of Recursion, RSF = 69)