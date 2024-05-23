

;; Quiz 1
;; CSc 335 Fall 2023
;; Section R



;; Name and Last 4: ________________________________________________________________________________________________________________


;; This is a closed book, closed notes, no devices quiz.  Even the sight of an open phone or other device will result in your failing the quiz.



;; One can reverse the digits of a nonnegative integer n to obtain another nonnegative integer:  for example, the
;; number formed by reversing the digits of 1234 is 4321.

;; Let us say that a number is a palindrome if it equals the number formed by reversing its digits.  
;; Thus 1, 11, 121 are all palindromes, while neither of 12 nor 123 is a palindrome. Note that numbers with leading
;; 0s are not recognized as numbers by Scheme - you do not need to consider inputs with leading 0s.

;; Design and code an R5RS function palindrome? to input a number n of at most 4 digits which outputs #t if n is a palindrome
;; and #f otherwise.  Thus (palindrome? 121) = #t, (palindrome? 12) = #f.  Your design should include helper functions. 

;; You may not use of recursion or iteration in any of these functions. 

;; Use only functions -- no lists, no strings, no vectors ... 

;; Give careful specifications for each function you define.  

