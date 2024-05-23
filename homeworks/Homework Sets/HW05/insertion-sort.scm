
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sorting the digits of a nonnegative integer 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; First guess-solution: iterative insertion sort

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Before reading ahead -- 

;; How did you do?  Compare your solution to mine!  Don't have a solution of your own?  Then please
;; make one BEFORE reading ahead.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; While reading ahead --

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; How did I do? Are there any mistakes here?  Can you use the course technology to find them?

;; (To put this another way: can you use the course technology to show that this solution is correct?)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Does the insert function meet the requirements imposed by sort?  Exactly what are these requirements?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; What changes are needed if we want 'sorted' to mean 'non-increasing' rather than 'non-decreasing'?

;; Can you write a single sort function which allows the user to choose between these?  (First hint: add a parameter
;; and use a wrapper function; next: try for a solution which does not use a wrapper.  And
;; of course, a question: are 0s still a problem for the non-increasing sort?)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Building on the development sketch worked out in class last Thursday, I start with a possible
;; scheme implementation of iterative insertion sort


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; pwr is 10^(num-digits back), so that (make-number front pwr back) returns the number formed as [front][back]
(define (make-number front pwr back)
  (+ (* front pwr) back))

;; Specification for sort:
;;; n is a non-negative integer
;;; (i-sort n) returns the number formed by the digits of n by placing them in non-decreasing order, with all
;;; 0s removed UNLESS n is 0, in which case 0 is returned

(define (i-sort n)

  ;; GI: (sorted n) = (make-number (sorted ns) 10^(num-digits as) as)
  ;;        where (sorted n) is the number formed from the digits of n in non-decreasing order
  ;;        and (num-digits m) is the number of digits in m

  ;; note that this GI has the form [Total Work] = [Work Remaining] COMBINED WITH [Work Already Done]
  ;; note that both sorted and num-digits are math functions, not actually implemented in this solution
  ;; note the use of the scheme function make-number to simplify the presentation

  (define (iter ns as)
    (cond ((zero? ns) as)
          (else (iter (quotient ns 10) (i-insert (modulo ns 10) as)))))

  (iter n 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; what is the termination argument?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; Specification for the iterative helper function i-insert
;;; d is a digit and s is a (scheme) number with digits in non-decreasing order
;;; (i-insert d s) returns the sorted number formed from d and s by inserting d into its proper place in s
;;;              if s = 0, d is returned (as 0d is not a scheme number)

(define (i-insert d s)

  (define (insert-d-between-nyp-and-ap nyp d tp ap)
    (make-number (make-number nyp 10 d) tp ap))

  (define (shift-rightmost-digit-nyp-to-ap nyp tp ap)
           (make-number (modulo nyp 10) tp ap))

  ;; GI: the conjunction of the variables' design roles, given as:
  ;;; s = (nyp * tp) + ap
  ;;; tp is 10^(num digits ap)
  ;;; d < any digit of ap
  ;;; nyp is sorted
  ;;; any digit of nyp is <= any digit of ap
  ;;; ap is sorted
  
  (define (iter nyp ap tp)
    (cond ((zero? nyp) (make-number d tp ap))
          ((>= d (modulo nyp 10)) (insert-d-between-nyp-and-ap nyp d tp ap))
          (else (iter (quotient nyp 10) (shift-rightmost-digit-nyp-to-ap nyp tp ap) (* 10 tp)))))

  (iter s 0 1)
  )

;; What is the termination argument?


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Guess-solution: recursive insertion sort  - the divide and conquer approach

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; for r-sort

         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
         ;    assume already sorted                        ; l ;    need to insert l into the already sorted segment
         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; for r-insert

         ; assume s is sorted, and that d can be correctly inserted in (quotient s 10) -- this is the IH

         ; show how to obtain the desired result from s once d has been inserted in (quotient s 10)
        


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; specification as for i-sort

(define (r-sort n)
  (cond ((< n 10) n)
        (else (r-insert (modulo n 10) (r-sort (quotient n 10))))))


;; d is a digit, s is a sorted integer
(define (r-insert d s)
  (cond ((>= d (modulo s 10)) (+ (* 10 s) d))
        (else (+ (* (r-insert d (quotient s 10)) 10)
                 (modulo s 10)))))



;; work out the induction!

;; what is the termination argument?


         
        
