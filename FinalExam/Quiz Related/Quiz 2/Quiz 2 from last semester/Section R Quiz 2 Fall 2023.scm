



;; Quiz 2
;; CSc 335 Fall 2023
;; Section R
;;


;; Name and Last 4: __________________________________________________________________________________


;; This is a closed book, closed notes, no devices quiz.  Even the sight of an 
;; open phone or other device will result in your failing the quiz.

;; Design, prove and code an iterative function partition to input

;;                     a nonnegative integer n which contains no 0s

;;                     a digit d

;; and which returns the number
;;                                 [greater-equal][ds][less-than]
;; where
;;                     greater-than is a number whose digits are all those digits of n which are > d,
;;                     in the same order and with the same multiplicity as they occur in n
;;
;;                     ds is the number whose digits are all d, with the same number of digits as
;;                     there are occurrences of d in n
;;
;;                     less-than is a number whose digits are those digits of n which are < d,
;;                     in the same order and with the same multiplicity as they occur in n
;;
;; and for 0-free nonnegative integers g, ds and l, [g][ds][l] is the number formed by writing these one after another.
;;

;;
;; For example, (partition 9281384556 5) returns 9886552134, ie, [9886][55][2134].
;;

;; Use only functions and numbers -- no lists, no strings, no vectors ...

;; Be sure to provide specifications for your functions. 