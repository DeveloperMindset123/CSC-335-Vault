;; Quiz 2
;; CSc 335 Fall 2023
;; Section M
;;


;; Name and Last 4: __________________________________________________________________________________


;; This is a closed book, closed notes, no devices quiz.  Even the sight of an 
;; open phone or other device will result in your failing the quiz.

;; Design, prove and code an iterative function merge to input two sorted nonnegative integers m and n, with output the sorted integer
;; resulting from merging m and n.  The definition of 'merge' is just as it was in  your algorithms class; you may decide
;; whether 'sorted' means digits in non-decreasing or non-increasing order. Your procedure must preserve multiplicities.
;; You may assume that neither m nor n contain 0s. 

;; Example: for nondecreasing order, (merge 11234 11233345) = 1111223333445

;; Use only functions and numbers -- no lists, no strings, no vectors ...

;; Be sure to provide specifications for your functions.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;; solution sketch

;; GI :  m = [m-nyp][m-ap] and m, m-nyp and m-ap are sorted
;;       n = [n-nyp][n-ap] and n, n-nyp and n-ap are sorted
;;       rsf is the sorted merge of m-ap and n-ap

;;       &&

;;       any digit in m-nyp is <= any digit in rsf

;;       &&

;;       any digit in n-nyp is <= any digit in rsf


;;; After some thought, it seems that it is necessary to specify that the sorted merge of a number n
;;; with 0 is n, and that the sorted merge of 0 with n is also n.  Thus the sorted merge of m-ap and
;;; n-ap is m-ap if (for example) it turns out that progress is not made on n for the first few steps.

;; Also: let me define [0][n] = n


;(define (merge m n)
;  (merge-iter ~~~~~~~~~~~~~~~~~~~~))
;
;(define (merge-iter m-nyp m-ap n-nyp n-ap rsf)
;  (cond ((zero? m-nyp) "put n-nyp in front of rsf")
;        ((zero? n-nyp) "put m-nyp in front of rsf")
;        ((let ((rightmost-m-nyp (modulo m-nyp 10))
;               (rightmost-n-nyp (modulo n-nyp 10))
;               (cond ((>= rightmost-m-nyp rightmost-n-nyp)
;                      (merge-iter (quotient m-nyp 10)
;                                  "put rightmost-m-nyp in front of m-ap"
;                                  n-nyp
;                                  n-ap
;                                  "put rightmost-m-nyp in front of rsf")
;                     (else
;                      (merge-iter ...
;                       "put rightmost-n-nyp in front of rsf"))

;; the termination argument is clear: the number of unprocessed digits remaining is reduced by 1 with every iteration

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Surely it appears that we will need a function to form the number [d][p] from a digit d and a nonnegative
;; integer p

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; As we have seen, this requires a function to count the digits in a nonnegative integer n.  Should we use the by now
;; familiar log formula? 

;; floating point is tricky!  Try this log-based function on input 1000:

;;;(define (num-digits n)
;;;  (cond ((zero? n) 1)
;;;        (else (+ 1 (inexact->exact (floor (log n 10)))))))

;; After noting that the value returned is 3, which of course is incorrect,  let me ask how confident you are in

;;;(define (num-digits n)
;;;  (cond ((zero? n) 1)
;;;        (else (+ 1 (inexact->exact (ceiling (log n 10)))))))

;; which gives the correct result for 1000?  Will it always give the correct result?  Do you agree that a closer examination
;; (say, into the derivation of the formula) is required before we can deploy the log-based function for this purpose?

;; Instead, we go with

;; n is a non-negative integer
(define (num-digits n)
  (cond ((< n 10) 1)
        (else (+ 1 (num-digits (/ n 10))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; and now back to the problem of making a number from two inputs

;; p and q are non-negative integers
;; returns [p][q]
(define (make-number p q)
  (+ (* p (expt 10 (num-digits q))) q))


;; another helper, just for readability

;; n is a non-negative integer
;; returns rightmost digit of n
(define (rd n)
  (modulo n 10))


;; m, m-nyp, m-ap, n, n-nyp, n-ap and rsf as above.  GI as above.  Spec as above.

(define (merge m n)
  (if (>= (rd m)(rd n))
      (merge-iter (quotient m 10) (rd m) n 0 (rd m))
      (merge-iter m 0 (quotient n 10) (rd n) (rd n))))
     

(define (merge-iter m-nyp m-ap n-nyp n-ap rsf)
  (cond ((zero? m-nyp) (make-number n-nyp rsf))
        ((zero? n-nyp) (make-number m-nyp rsf))
        
        (else (let ((rm-nyp (modulo m-nyp 10))
                    (rn-nyp (modulo n-nyp 10)))
                
                (cond ((>= rm-nyp rn-nyp)
                       (merge-iter (quotient m-nyp 10) (make-number rm-nyp m-ap)
                                   n-nyp n-ap
                                   (make-number rm-nyp rsf)))
                                 
                      (else
                       (merge-iter m-nyp m-ap
                                   (quotient n-nyp 10)(make-number rn-nyp n-ap)
                                   (make-number rn-nyp rsf))))))))
           
  

;; questions for students: (1) are the ap variables actually needed?  How  would you adjust the code and invariant if these were made 'virtual'?
;; (2) can you see any advantage at all in first developing a version in which these variables are used? (3) is this program (henceforth
;; a program is defined to be spec + proof + code) correct?  (4) can you see a nice way to modify the code so that the amount of time
;; spent counting digits is (greatly) reduced? How would doing so affect the GI?  (5) what about looking into the computation of
;; the number of digits using logs?  Does ceiling always work?  



