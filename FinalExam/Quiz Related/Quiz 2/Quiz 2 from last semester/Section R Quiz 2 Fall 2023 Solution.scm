



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
;;                                 [greater-than][ds][less-than]
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; solution sketch

;; GI : ap is partitioned as 

;;          ap = [greater-than][ds][less-than]

;; conjoined with

;;       n = [nyp][ap]


;; propose to solve the problem in a single scan from right to left, grabbing the next digit, adjoining it to the left of the appropriate
;; segment, and repeating until nyp = 0.  Clearly, adjoining to the left will preserve order and multiplicities.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; I will not maintain the variable ap, so my GI must be interpreted carefully:  the segments greater-than, ds
;; and less-than will be maintained, and these will comprise the partition of ap.  This should be stated directly
;; in the GI, so let me revise as follows:

;; GI is 

;;        n = [nyp][ap]

;; conjoined with

;;        partition (ap, d) = [greater-than][ds][less-than]

;; where partition is regarded here as the (math) function corresponding to the specification.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Surely it appears that we will need a function to form the number [d][p] from a digit d and a nonnegative
;; integer p



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; but first:

;; floating point is tricky!  Try this log-based function on input 1000:

;;;(define (num-digits n)
;;;  (cond ((zero? n) 1)
;;;        (else (+ 1 (inexact->exact (floor (log n 10)))))))

;; After noting that the value returned is 3, let me ask how confident you are in

;;;(define (num-digits n)
;;;  (cond ((zero? n) 1)
;;;        (else (+ 1 (inexact->exact (ceiling (log n 10)))))))

;; which gives the correct result for 1000?  Will it always give the correct result?  Do you agree that a closer examination
;; (say, into the derivation of the formula) is required?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; n is a non-negative integer
(define (num-digits n)
  (cond ((< n 10) 1)
        (else (+ 1 (num-digits (/ n 10))))))


;; p and q are non-negative integers
;; returns [p][q]
(define (make-number-2 p q)
  (+ (* p (expt 10 (num-digits q))) q))


;; p is a non-negative integer
;; q and r are positive integers
;; returns [p][[q][r]], that is, the number formed by writing p followed by q followed by r.

(define (make-number-3 p q r)
  (let ((qr (make-number-2 q r)))
    (make-number-2 p qr)))
         
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; wait: what if the input does not have digits in each category?

;;; could there be a flaw in the specification?!  It seems we need to add something like

;;;; ... where any of [greater-than], [ds], [less-than] is omitted if there are no digits satisfying the stated condition

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; I will add to the GI that each of the variables greater-than, ds, less-than is equal to -1 iff no digits in the corresponding
;; category have been encountered.  So then

;           x = [greater-than][ds][less-than]

;; means

;          x = [greater-than][ds] if less-than = -1
;          x = [greater-than][less-than] if ds = -1
;          x = [ds][less-than] if greater-than = -1

;; and otherwise

;          x = [greater-than][ds][less-than]

;; as defined above

;; here is one (perhaps clumsy) way of facilitating this 

(define (flagged-make-number-2 p q)
  (cond ((= p -1) q)
        ((= q -1) p)
        (else (make-number-2 p q))))

(define (flagged-make-number-3 p q r)
  (cond ((= p -1) (flagged-make-number-2 q r))
        ((= q -1) (flagged-make-number-2 p r))
        ((= r -1) (flagged-make-number-2 p q))
        (else (make-number-3 p q r))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; so now we can give some code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (partition n d)
  (partition-iter n d -1 -1 -1))

(define (partition-iter nyp d greater-than ds less-than)
  (cond ((zero? nyp) (flagged-make-number-3 greater-than ds less-than))
        (else
           (let ((r-nyp (modulo nyp 10))
                 (l-nyp (quotient nyp 10)))
             (cond ((< r-nyp d) (partition-iter l-nyp d greater-than ds (flagged-make-number-2 r-nyp less-than)))
                   ((= r-nyp d) (partition-iter l-nyp d greater-than (flagged-make-number-2 d ds) less-than))
                   ((> r-nyp d) (partition-iter l-nyp d (flagged-make-number-2 r-nyp greater-than) ds less-than)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; the termination argument is simple: #digits(nyp) is decremented with each call

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; questions for students: (1) is this program (a program is henceforth defined to be spec + proof + code) correct?
;; (2) can you see a nice way to modify the code so that the amount of time spent counting digits is (greatly) reduced? How would doing
;; so affect the GI? (3) can you think of a nicer way of handling the possibility that for given inputs n and d, one or more
;; of the greater-than, ds, less-than segments might be empty?  (4) is the restriction on zeros (that is, none of the digits
;; of n are 0) necessary? (5) what about looking into the computation of the number of digits using logs?  Does ceiling always work?  
