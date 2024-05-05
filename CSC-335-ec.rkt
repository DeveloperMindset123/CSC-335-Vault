#lang racket

; CSC 335 Midterm Extra Credit
; 1. [Definition of LD] Consider the use of non-negative integers to represent lists in the class LD, where LD consists of nonempty lists whose members are either digits or themselves members
; of LD. One idea might be to use 1 to stand for the left paren (, and 2 to stand for the right paren ). We would want 1 and 2 to be used as digits as well as end markers (but never both at the same time).

; Here are some examples
; 112 represents (1)
; 1112 represents (1 1)
; 1122 represents (1 2) -- Note that our setup would have 12 representing () if we allowed LD to contain the empty list, but given the constraint, empty lists aren't allowed in this case
; and (8 (1 2 (3))(5 6 1)) --> here, we are choosing some values of 1 and 2 remain as it is and other values of 1 and 2's to be converted to ( and ) parenthases
; would be represented as 181121322156122

; 2. [Definition of LD number] An integer arising in this way will be called LD-number. Thus 112, 1112 and 1122 are LD-numbers, but 12 is not and neither is 143. (The reason for this is because
; --> 12 represents the empty set (), which is not allowed and and 143 can only be represented as ( 43, meaning we have an opening parenthases without any closing parenthases/

; 3. [Problem Statement] Observe that multiple elements of LD can genereate a given LD number. Given this, it makes sense to ask: how many different elements of LD are represented by a given LD-number?
; 4. [Further Instructions] Team mates do not need to be from the same section. Each team should submit just one paper, displaying the name and email address for each team member. Each document should
; --> present a full development leading to working (and tested) code.

; Name: Ayan Das
; Email: adas006@citymail.cuny.edu
; Date: 04/12/2024
; PM Section
; EMPLID: 24073854

; Purpose: Calculates the number of digits in a given number 'n'. If 'n' is less than 10, it only has one digit. Otherwise, it recursively divides 'n' by 10 and strips off it's digits, counting them until only one digit remains.
#|
Proof Of Correctness: (Using Strong Induction)
- Induction Hyphothesis: Let's use strong induction to prove that for all non-negative integers, 'n', the function, (length-recursive n) correctly computes the number of digits in n.
- Base Case: In range of where n is between 0 and 9, n only has one digit, guranteed. The function starts by checking this as the base/termination case where it check if n < 10. If this condition is true, return 1. This holds true because any single-digit number has exactly one digit.
- Inductive Step: Assume for the purpose of induction that (length-recursive k) correctly computes the number of digits for all integers k such that 0 <= k <= n. We need to prove that '(length-recursive n) correctly computes the number of digits of n.

There are two cases to consider:
1. When n < 10 : the function returns 1, which is correct since n is a single digit non-negative integer.
2. When n >= 10: The function returns (+ 1 (length-recursive (quotient n 10))). By the induction hyphothesis, (length-recursive (quotient n 10)) correctly computes the number of digits in n / 10, which is n without it's rightmost/last digit. Since dividing the integer n by 10 effectively removes the last digit (and thus reduces) the integer, the total number of digits in n is 1 (the last digit) plus the number of digits in 'n / 10'. Hence, (length-recursive n) returns the number of correct number of digits
|#
(define (length-recursive n)
  (cond ((< n 10) 1) 
        (else (+ 1 (length-recursive (/ n 10))))))
(display "Length of 29231 (should be 5): ")(display (length-recursive 29231))(newline)
(display "Length of 5 (should be 1): ")(display (length-recursive 5))(newline)


#|
- Proof/Design Idea: The function 'length-iter' computes the numebr of digits in non-negative integer 'n' using an iterative appraoch with tail recursions. The function uses two arguments, 'n' which is the number to determine the length of, and count, which tracks the count of digits encountered so far.

- Loop invariant: At the beggining of each iteration, 'count' holds the number of digits in the original number divided by 10^(current count value). in simpler terms, the invariant is that 'count' is equal to the number of digits processed so far in 'n'.

- Pre-Condition: n must be a non-negative integer and count must be initialized to 0 when the function is first called to correctly count the number of digits in 'n'.
- Post-Condition: When the funciton completes, 'count' must equal the total number of digits in the original 'n'.
- Weak-Enough Test: The invariant is weak to be true at the start of the loop. When the function is initially called the 'count' set to 0, the invariant holds because 0 digits have been processed, which corresponds to the state of 'n'.
- Strong-Enough Test: At the end of the iteration (when 'n' becomes 0), 'count' holds the total number of digits that 'n' had initially, which is the goal of the function. Therefore, the invariant is storng enough to prove the post-condition.
- Preservabillity Test: Within the loop (the recurisve call), the function decreases 'n' by dividing it by 10 and incrases count by 1. This update preserves invariant: as n is reduced by one digit, count is incremented by 1, reflecting the AP digits. Hence the invariant is preserved across each iteration of the loop.
|#
(define (length-iter n count)
  (cond ((= n 0)count)
        (else (length-iter (quotient n 10) (+ count 1)))))

; Purpose: replaces the digit at a specified 'place' in a number 'n' with a new 'val'. This function manipulates the number by dissecting it into parts and reconstructing it with new value in place.

#|
Induction Proof:
- Induction Hyphothesis: We assume that the function 'replaceValueGivenIndex' correctly replaces the digit at the specified 'place' in any number n with the digit 'val'.
- Base Case: For 'n' having digits only at the unit place (i.e. when place=1) --> If n is a single digit number, the function should effectively replace that digit with 'val'. Testing with any single digit number 'n' and place = 1, should yield a number where 'n' is repalced by 'val'.
- Inductive Step : Assume the function works for replacing a digit at position 'place=10^k'. We need to show it works for place = 10^(k+1). Using the function with the next higher place should remove digits less significant than 10^(k+1) and add add val * 10^(k+1) plus all digits less significant than 10^(k+1) from the original number 'n'. By induction hyphothesis, this means it correctly replaces the intended digit.
- Guessing Invariant: At each step, the parts of the number 'n' outside the target 'place' remains unchanged. The digit at 'place' should be replaced by 'val' after the function execution.
- Pre-Condition: n is a non-negative integer, place is a power of 10 (meaning it's remainder is 0), val is a single digit between 0-9 (since we cannot have negative integer values and the goal of the function is to change only one position's value at a time).
- Post-Condition: The digit at the position 'place' in 'n' has been replaced by 'val', all the other digits of n remain what they originally were.
- Weak-Enough Test: The invariant must hold true before the function starts; initially, 'n' is unchanged and meets this criterion as no digits has been altered yet.
- Strong-Enough Test: At the completion of the function, the digit at 'place' is replaced by the value specified at 'val' and values at all the other position remains unchanged, meaning the post-condition holds true and so does the guessing invariant.
- Preservabillity Test: Each step in the function keeps parts of 'n' outside the target 'place' unchanged while updating the target position, thus preserving the invariant.
|#
; Iterative Implementation
(define (replaceValueGivenIndex n place val)
     (+(+(*(quotient n (* 10 place))(* place 10)) (* val place)) (modulo n place)))
(display "Replace 2nd place of 2013 with 1 (should be 2003): ")(display (replaceValueGivenIndex 2013 1 2))(newline)
(display "Replace 2nd place of 456789 with 0 (should be 406789): ")(display (replaceValueGivenIndex 456789 2 0))(newline)


#|
Design Idea/Proof Concept:
- Recursive Design: The function uses recursion to traverse through the digits of n from least to most significant until it finds the correct position to replace the digit.
- Base Case Effectiveness: When the recursion reaches the digit to replace (base case), it correctly reconstructs the number with the new digit.
- Recursive Step Correctness: Each recursive call handles one digit of n, either passing it through unchanged if it's not at the correct place or replacing it when the correct place is reached. The function reassembles the number correctly on the way back up the recursive calls.
|#

;Recursive Implementation
(define (replaceValueGivenIndex-Recursive n place val)
  (if (= place 1)
      (+ (* val place) (* 10 (quotient n 10)))  ; Replace the digit and reconstruct the part above it
      (+ (* (modulo n 10) place)                ; Keep the current least significant digit
         (* 10 (replaceValueGivenIndex-Recursive (quotient n 10) (quotient place 10) val)))))  ; Recurse and shift left

(display "Replace 2nd place of 2013 with 1 (should be 2003), Recursive Implementation: ")(display (replaceValueGivenIndex-Recursive 2013 1 2))(newline)


;purpose: extracts the first (left-most) digit at number n. it does so by dividing 'n' by 10 raised to the power of one less than the length of 'n', effectively removing all but the first/leftmost digit.

#|
Proof By Induction:
- base case: For any n with a single digit (i.e. 0 <= n < 10), 'leftmostDigit', directly returns 'n', which is the leftmost and the only digit.
- Inductive Step: leftmostDigit works correctly for any number with k digits. For a number with k+1 digits, the function strips away the last digit and applies itself to the result. By the induction hypothesis, it correctly identifies the leftmost digit of the new number, which remains the leftmost digit of the original number.

Guessing Invariant: At each step, n is reduced to its first k-1 digits.
Precondition: n must be a non-negative integer.
Postcondition: Returns the leftmost digit of n.
Weak-Enough Test: True at the beginning, as n has not been modified.
Strong-Enough Test: True at the end; when reduced to one digit, the function returns that digit.
Preservability Test: True, as each step correctly reduces n without losing the leftmost digit.
|#
; iterative implementation
(define (leftmostDigit n)
  (quotient n (expt 10 (- (length-recursive n) 1))))
(display "Leftmost digit of 2013 (should be 2): ")(display (leftmostDigit 2013))(newline)
(display "Leftmost digit of 98765 (should be 9): ")(display (leftmostDigit 98765))(newline)

#|
Design Idea:
leftmostDigit: Strips away digits until only the leftmost is left.
|#
; recurisve implementation
(define (leftmostDigit-recursive n)
  (if (< n 10)
      n
      (leftmostDigit-recursive (quotient n 10))))
(display "Leftmost digit of 2013 (should be 2), recursive approach: ")(display (leftmostDigit-recursive 2013))(newline)
(display "Leftmost digit of 98765 (should be 9), recursive approach: ")(display (leftmostDigit-recursive 98765))(newline)

; purpose: retrieves the rightmost digit from the string of digits, very simple logic
(define (rightmostDigit n)
  (modulo n 10))
(display "Rightmost digit of 2013 (should be 3): ")(display (rightmostDigit 2013))(newline)
(display "Rightmost digit of 98765 (should be 5): ")(display (rightmostDigit 98765))(newline)

#|
- Guessing Invariant (GI): At each invocation of 'valMatches-num1-num2', 'n', 'num1' and 'num2' are unchanged and correctly represents the initial input values. The functions 'leftmostDigit' and 'rightmostDigit' provides consistent and correct outputs based on 'n'.
- Weak-Enough Test: The invariant is valid at the start because it fundamentally asserts the correct function of leftmostDigit and rightmostDigit, which are assumed correct.
- Strong-Enough Test: The invariant provides enough information to conclude the post-condition because the outputs of leftmostDigit and rightmostDigit determine the return value of the main function.
- Preservability Test: Each call to valMatches-num1-num2? does not alter any data or state, preserving the invariant.
- pre-condition: n is a non-negative integer and num1 and num2 are single digit integers between 0-9.
- post-condition: The function returns '#t' if the leftmost and rightmost digits of 'n' match 'num1' and 'num2' respectively.

Induction Proof:
- Base Case: For a number n with a single digit (where n < 10), 'leftmostDigit' and 'rightmostDigit' should return n. If num1 = num2 = n (this is only true when length of n is 1), then the function returns #t, this is the smallest possible subproblem.
- Inductive Step: Assume 'valMatches-num1-num2?' works correctly for any n with k digits. For n with k+1 digits, the correctness of 'leftmostDigit' and 'rightmostDigit' ensures that 'valMatches-num1-num2?' also works, since this function depends on the two helper function.
|#

; purpose: checks if the first and last digits of n are 'num1' and 'num2', respectively. Returns '#t' (true), if so, otherwise '#f' (false) 
(define (valMatches-num1-num2? n num1 num2)
  (cond ((and(=(leftmostDigit n)num1)(=(rightmostDigit n)num2))#t)
        (else #f)))
(display "Does 2013 start with 2 and end with 3? (should be #t): ")(display (valMatches-num1-num2? 2013 2 3))(newline)
(display "Does 4567 start with 4 and end with 8? (should be #f): ")(display (valMatches-num1-num2? 4567 4 8))(newline)


#|
Design Idea:
The function is-12Within-N? is designed to recursively check whether the digit sequence '12' appears anywhere within a number num. The function works by repeatedly stripping off the least significant digit of num and checking if the two least significant digits form the sequence '12'.|#
;purpose: searches for a pattern '12' within num, indicating an empty list which is not allowed. return '#t' if found, and otherwise '#f'
(define (is-12Within-N? num)
     (cond ((= num 0) #f)
           ((and (=(quotient (modulo num 100)10)1) (=(modulo num 10)2)) #t)
           (else (is-12Within-N? (quotient num 10)))))
(display "Is 12 within 46386127979? (should be #t), recursive approach: ")(display (is-12Within-N? 46386127979))(newline)

;iterative form of is-12Within-N
#|
- Guessing Invariant (GI): At each iteration, no occurence of '12' has been found in the previously checked least significant digits of 'num'.
- pre-condition: num is a non-negative integer
- post-condition: returns #t if '12' is found within the digits of 'num', otherwise, return #f.
- Weak-Enough Test: The invariant is valid at the start since no digits have been checked and no incorrect '12' sequence has been found.
- Strong-Enough Test: When the function finds '12' or reduces num to less than two digits, it can correctly return the result based on the findings, satisfying the post-condition.
- Preservability Test: Each iteration correctly updates num and checks the current two digits without losing information about previous checks. The invariant is preserved because if '12' hasn't been found in prior digits, the process of checking the next two digits will not incorrectly alter past results.

Induction Proof:
- Base case: if 'nun' is less than two digits (i.e. 'num' < 10), the function correctly returns '#f' as it is not possible to form '12' with fewer than two digits, this is the smallest possible subproblem we can break down this function to.
- Inductive Step: Suppose the invariant holds that no '12' has been found in all checks up to num. For each iteration --> If modulo num 100 equals 12, then '12' has been found, and returning #t is correct.Otherwiset, the function strips the least significant digit (by performing quotient num 10) and repeats the check. By the induction hypothesis, this approach will correctly handle the detection of '12'.
|#
(define (is-12Within-N?-iterative num)
  (let iter ((num num))
    (cond ((< num 10) #f) ; If num has less than 2 digits, it can't contain 12.
          ((= (modulo num 100) 12) #t) ; Directly check if the last two digits are 12.
          (else (iter (quotient num 10))))))
(display "Is 12 within 987654 (should be #f), iterative appraoch: ")(display (is-12Within-N?-iterative 987654))(newline)

;Series of Test Cases
(display "Testing with number containing 12: ")(display (is-12Within-N?-iterative 12345))(newline) ; Expected: #t
(display "Testing with number not containing 12: ")(display (is-12Within-N?-iterative 34567))(newline) ; Expected: #f
(display "Testing with number ending in 12: ")(display (is-12Within-N?-iterative 4512))(newline) ; Expected: #t
(display "Testing with only 12: ")(display (is-12Within-N?-iterative 12))(newline) ; Expected: #t
(display "Testing with single digit: ")(display (is-12Within-N?-iterative 2))(newline) ; Expected: #f
(display "Testing with leading 12: ")(display (is-12Within-N?-iterative 12987))(newline) ; Expected: #t
(display "Testing with 12 in the middle: ")(display (is-12Within-N?-iterative 4512876))(newline) ; Expected: #t


; purpose: checks if the parenthases in 'num' are balanced. it uses'count' to track balance, incrementing for '1' (open parenthases) and decrementing for '2' (close parenthases). Returns '#t' if balanced, '#f' otherwise
#|
Design Idea for the Recurisve Implementation:
--> It recurisvely checks the balance of digits in num by processing each digit from the least significant to the most significant value.
--> With each recurive call that is being made, it recurisvely processes each digits, adjusting the count value based on whether it's an 1 or 2. If count were to ever become negative, the recursion terminates immediately and returns #f, due to the fact that there's an imbalance (i.e. closing paranthases appearing prior to an opening paranthases when count is already 0).
--> It terminates when all digits have been processes, return #t if count is zero (balanced) or #f if it's not otherwise for all other scenarios.
|#
; Tested and worked
(define (isbalanced? num count)
    (cond ((= num 0) (if ( = count 0) #t #f))
          ((< count 0) #f)
          ((and (=(quotient (modulo num 100)10)1) (=(modulo num 10)2))(isbalanced? (quotient num 100)  count))
          ((=(modulo num 10)1) (isbalanced? (quotient num 10) ( - count 1)))
          ((=(modulo num 10)2) (isbalanced? (quotient num 10) ( + count 1)))
          (else (isbalanced? (quotient num 10) count))))
(display "Is 1212 balanced? (should be #t): ")(display (isbalanced? 1212 0))(newline)
(display "Is 111222 balanced? (should be #t): ")(display (isbalanced? 111222 0))(newline)


#|
Induction Proof:
- Base Case: When num is 0, the function checks if count is 0 as well, which means all occurences of 1 (treated as the opening parenthases) have been balanced by 2 (which is treated as the closing parenthases), hence the sequence is balanced and the function returns #t.
- Inductive Step: Assume that isBalanced-iterative correctly checks for the balance up to the first k digits of num. (where k <= n) Therefore, for the k+1 iteration --> if at least two significant digits from the sequence '12', the function skips these two digits and continues (since that is an empty list, therefore it increments and then decrements, balancing each other out), preserving count (as the sequence '12' considered neutral and therefore does not have any impact in balance.
 --> If the least significant digit (leftmost digit) is 1, increment count by 1, reflecting an open parenthases that requires a corresponding closing parenthases.
 --> If the least significant digit is 2, the function decrements count by 1, reflecting a closing parenthases for the previous open parenthases.
 --> The function iterates by reducing 'num' (removing the checked least signficant digit) and adjusting count accordingly, balancing it out, with the goal of count being 0 and anything otherwise will result in #f.
- Guessing Invariant: The invariant throughout the function's execution is that 'count' directly represents the net 'unbalanced' count between 1's and 2's, wtih 1 incrementing the count and 2 decrementing it.
- Precondition: num must be a non-negative intger
- Postcondition: Returns #t if the sequence represented by num is balanced and # f otherwise.
- Weak-Enough Test: The function starts with count = 0, representing a neutral or balanced state initially. This condition is weak enough to be true before the function starts iterating.
- Strong-Enough Test: The function returns #t only if count equals zero after all digits have been processed, ensuring all increments (1's) have been matched with decrements (2's). This is strong enough to ensure that the sequence is indeed balanced when the function claims it is.
- Preservability Test: At each step of iteration, the function correctly modifies count based on the current digit of num. The invariant that count tracks the balance is maintained regardless of which branch of the conditional is executed.
|#
;iterative implementation: --> tested and worked
(define (isBalanced-iterative? num)
  (let iter ((num num) (count 0))
    (cond ((= num 0) (= count 0))
          ((< count 0) #f)
          ((and (=(quotient (modulo num 100) 10) 1) (=(modulo num 10) 2)) (iter (quotient num 100) count))
          ((=(modulo num 10) 1) (iter (quotient num 10) (- count 1)))
          ((=(modulo num 10) 2) (iter (quotient num 10) (+ count 1)))
          (else (iter (quotient num 10) count)))))
(display "Is 1212 balanced? (should be #t), iterative approach: ")(display (isBalanced-iterative? 1212))(newline)
;(display "Is 111222 balanced? (should be #t), iterative appraoch: ")(display (isbalanced-iterative? 111222))(newline)



#|
Induction Proof:
- Base Case: Assume 'n' has a minimal valid format, such as '132', it is unbalanced because it starts because it starts and ends correctly and does not contain the sequence '12'.
- Inductive Step: For any number 'n', assume the following:
---> isBalanced? is used to check balance.
---> is-12Within-N? correctly identifies the presnece of sequence "12". The start and end digits can be checked directly using this helper function.

- Pre-condition: 'n' must be a non-negative integer, ideally represented in a way that the digits '1' and '2' can denote specific structural properties (like parenthases).
- Post-condition: Returns #t if n is a valid LD number (where isBalanced holds true and is-12Within-N returns false, since we cannot have 12), and false otherwise
- Guessing Invariant (GI) : The sequence of checks (isbalanced?, not containing '12', starting with 1, and ending with 2) does not alter n and consistently applies helper predicates to determine if n is a valid LD number.
- Weak-Enough: The initial conditions assume no prior knowledge about the number except its form. This state must be enough to allow for all checks.
- Strong-Enough: After applying all checks, if the function deduces #t, it must mean all criteria are conclusively met.
- Preservabillity: Each step (or check) in the process must preserve the correctness of the previous steps results. This ensures that no intermediate step invalidates the result of another.

|#
;purpose: determines if n is a valid LD number. It checks if 'n' is balanced (properly nested and matching paranthases), does not contain a specific pattern and starts and ends with '1' and '2' (representing opening and closing paranthases)
(define (is-n-valid-num? n)
    (cond ((and (isbalanced? n 0) (not (is-12Within-N? n))) #t)
          (else #f)))
(display "Is 181121322156122 a valid LD-number? (should be #f): ")(display (is-n-valid-num? 181121322156122))(newline)
(display "Is 112 a valid LD-number? (should be #t): ")(display (is-n-valid-num? 112))(newline)


;purpose: this function seems intended to remove the first and last digits of 'num'
#|
Induction proof:
- Base Case: If num is a two-digit number, removeFirstAndLast results in 0 which is the correct expectation for the minimal input form.
- Inductive Step: Assume that removeFirstAndLast correctly removes the first and last digits from a number with k digits --> For num with k+1 digits, the function calculates the total number of digits (length-recursive num), removes the highest and lowest order digits, and correctly reduces the number to its middle part.

- Precondition: num must be greater than or equal to three digits for meaningful removal.
- postcondtion: the returned result is 'num' with it's first and last digits removed.
- weak-enough: before execution, num is simply a number with no alterations, thus meeting the initial condition of being a multi-digit integer that is greater than 3 in length
- strong-enough: if the function outputs a number, it must be a 'num' minus it's first and last digits, ensuring the function's reliabillity at the end execution of the program.
- preservabillity: the steps to remove digits do not alter the internal structure or value of the remaining digits.
- Guessing Invariant: Throughout the function execution, 'num' is reduced in a controlled manner that preserves the integrity of the remaining digits (except the first and last). The structure of num maintained as required by the operation.
|#
; tested and worked
(define (removeFirstAndLast num)
  (quotient (modulo num (expt 10 (-(length-recursive num )1))) 10))
(display "Remove first and last from 1029172 (should be 2917): ")(display (removeFirstAndLast 1029172))(newline)
(display "Remove first and last from 1234 (should be 23): ")(display (removeFirstAndLast 1234))(newline)


; purpose: attempts to count all valid LD interpretations of num by recursively exploring and manipulating it, has a wrapper all-LD
(define (combined-LD num)
  (define (combined-LD-wrapper num ld count)
    (cond ((= num 0) (if (is-n-valid-num? ld)1 0))
        ((or (=(modulo num 10)1) (= (modulo num 10)2))
         (+ (combined-LD-wrapper ( quotient num 10) (replaceValueGivenIndex ld count 3) (* count 10))
            (combined-LD-wrapper ( quotient num 10) ld (* count 10))))
        (else (combined-LD-wrapper ( quotient num 10) ld (* count 10)))))
  (combined-LD-wrapper (removeFirstAndLast num)(removeFirstAndLast num) 1))
(display "How many LD representations for 181121322156122? (expected output may vary): ")(display (combined-LD 181121322156122))(newline)

#|
(all-LD num) Design Idea:
The primary function is designed to recursively generate all possible combinations of numbers termed as LD numbers. It evaluates these combinations to determine if they are valid LD numbers. Validity is assessed based on specific criteria implemented in the `isvalid` function. 

Induction Proof:
Inductive Step:
- Base Case: The recursion terminates when a parameter, possibly representing the number of potential combinations (NYP), reaches zero. At this point, all LD combinations have been processed. The `isvalid` function then checks each LD number: if valid, it returns `1`; if invalid, it returns `0`.

- Inductive Hypothesis: We assume that the function `all-ld` effectively works for all given inputs of `N`.
Inductive Step: (Broken down into two parts)
During each recursive call:
- If the last digit of the number is `1` or `2`, the function proceeds to evaluate the number produced by the previous recursive call (`k`th call). It then decides whether to replace the LD element with an arbitrary number or to retain it as an LD element. It calculates the sum of these two possibilities to progress the computation.
- If the number does not end in `1` or `2`, the function retains the number from the previous (`k`th) call without alteration.

This approach ensures that every recursive depth checks and branches based on the conditions of the LD number ending, effectively exploring all potential LD number configurations.

Note: Due to the implementation being recursive, we do not need a guessing invariant.
|#

; something important to note, understanding the difference between let and let* in scheme (and letrec) --> while let makes it's bindings available only in the bodys, the let* makes it's bindings available to any later bindings available to any later binding expressions


; As a helper for the project, we will need to first define a function named addFirstAndLast
(define (addFirstAndLast num)
; assuming length-recursive calculates the number of digits in num
  (let* ((len (length-recursive num))
        (prefix (* 1 (expt 10 (+ len 1))))
      (withNum (* num 10))
      (result (+ prefix withNum 2)))
  result))

; example usage
; (display (addFirstAndLast 1234))

;(cond ((= num 0) (if (is-n-valid-num? ld) (list (addfirstandlast ld) )(list ))))
(define (all-LD num)
  (define (all-ld num ld count)
    (cond ((= num 0) (if (is-n-valid-num? ld)
                         (list (addFirstAndLast ld))
                         (list)))
          ((or (=(modulo num 10) 1) (= (modulo num 10) 2))
           (append (all-ld (quotient num 10) (replaceValueGivenIndex ld count 3) (* count 10))
                   (all-ld (quotient num 10) ld (* count 10))))
          (else (all-ld (quotient num 10) ld (* count 10)))))
  (all-ld (removeFirstAndLast num) (removeFirstAndLast num) 1))

(all-LD 1029172)

