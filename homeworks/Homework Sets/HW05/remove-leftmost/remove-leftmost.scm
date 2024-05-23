
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem    Specify, design, develop, code and prove correct an R5RS Scheme program remove-digit-left which
;;            inputs an integer n and a digit d and which returns the number obtained from n by removing the
;;            leftmost occurrence of d.  Your program is not to use lists or strings. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; again -- have you solved / tried to solve this yourself?  Don't read my solution until you have!  Start by figuring out how
;; you will deploy a divide and conquer strategy.

;; how will you spot any mistakes I might have made unless you solve the problem yourself?


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Specification


;; notation: for an integer n >= 0, number->string(n) is the string of digits of n, occurring in the same order as they
;; do in n.  Thus number->string(234) = "234"

;; notation: if s is a string of digits, let string->number(s) be the scheme number corresponding to s.  Thus
;; leading 0s are suppressed: string->number("00234") = 234 = string->number("234").  This function differs
;; in definition from the same-named [the word is 'eponymous'] scheme  primitive, as it is most convenient
;; here to specify that string->number("") = 0.  



;; pre: n >= 0 is an integer

;; post: if number->string (n) is the concatenation (*) of strings p, "d", and q, where d does not occur in p, then
;;       (remove-digit-left n d) = string->number(p*q).  In particular, if n = d, then 0 is returned.  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; a recursive solution

;; the divide and conquer strategy I used is apparent in this code -- as in so many of these examples,
;; the induction is on the number of digits in the input n


(define (remove-digit-left n d)
  (cond ((= (modulo n 10) d) (if (occurs? (quotient n 10) d)
                                 (+ (* 10 (remove-digit-left (quotient n 10) d)) d)
                                 (quotient n 10)))
        (else (+ (modulo n 10)
                 (* 10 (remove-digit-left (quotient n 10) d))))))

;; n and d as above
;; checks whether the digit d occurs in n
(define (occurs? n d)
  (cond  ((< n 10) (= n d))
         ((= (modulo n 10) d) #t)
         (else (occurs? (quotient n 10) d))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; an iterative solution

;; I will process n from right to left; the GI is given just below.

;; again, it seems simplest to describe what is going on by using the functions
;; number->string and string-> number introduced above.  In addition, let's have
;; the string function strip-leftmost-d, which inputs a string s of digits and returns the
;; string obtained from s by deleting its leftmost d. 



(define (rld n d)
  (cond ((= n d) 0)
        ((< n 10) n)
        (else (rld-iter n 0 1 d))))

;; GI: nyp and ap >= 0 are integers

;;     &&

;;     pt is 10^(k - 1) where k is the number of digits in ap

;;     if d occurs in nyp, then
;;                strip-leftmost-d (number->string (n)) = strip-leftmost-d (number->string (nyp))
;;                                                        *
;;                                                        number->string (ap)

;;     &&

;;     if d does not occur in nyp, then
;;               strip-leftmost-d (number->string (n)) = number->string (nyp) * number->string (ap)


(define (rld-iter nyp ap pt d)
  (cond ((zero? nyp) ap)
        ((= d (modulo nyp 10)) (if (occurs? (quotient nyp 10) d)
                                   (rld-iter (quotient nyp 10)
                                         (+ (* (modulo nyp 10) pt) ap)
                                         (* 10 pt)
                                         d)
                                   (+ (* (quotient nyp 10) pt) ap)))
        (else (if (occurs? (quotient nyp 10) d)
                  (rld-iter (quotient nyp 10)
                        (+ (* (modulo nyp 10) pt) ap)
                        (* 10 pt)
                        d)
                  (+ (*  nyp pt ) ap))
              
              )))
                   

        


;; another approach would be to start by reversing the digits of the number. 



         
  