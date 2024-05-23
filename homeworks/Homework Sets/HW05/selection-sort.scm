
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; recursive solution

;; you should work out the divide and conquer analyses for each of these functions, and then, for practice, give
;; the formal inductions as well.  Remember: do NOT just accept that the code is correct!!!


;; n >= 0 is an integer -- for simplicity, let us say that n has no occurrences of 0
;; (sel-sort n) returns (sorted n), that is, the number formed from the digits of n arranged in non-decreasing order

(define (sel-sort-r n)
  (cond ((< n 10) n)
        (else (let ((d (max-digit n)))
                (+ (* (sel-sort-r (remove-digit d n)) 10) d)))))

;; returns number formed from n by removing the rightmost occurrence of d, if d occurs in n; otherwise returns n
;; returns 0 if d = n
(define (remove-digit d n)
  (let ((rightmost (modulo n 10)))
    (cond ((= rightmost d) (quotient n 10))
          (else (+ (* (remove-digit d (quotient n 10)) 10) rightmost)))))


;; returns the largest digit which occurs in n
(define (max-digit-r n)
  (cond ((< n 10) n)
        (else (max (max-digit-r (quotient n 10)) (modulo n 10)))))


;; what are the termination arguments?


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; iterative solution

;; you should work out the development steps

;; you should work out the specifications for each function

;; can you improve these?  Do you see a different functional decomposition?


(define (max-digit-i n)

  ;; GI: n is the number [nyp][ap] formed from nyp and ap, and max-so-far is a largest digit in ap.
  ;; note that ap is a virtual variable

  (define (iter nyp max-so-far)
    (cond ((zero? nyp) max-so-far)
          ((> (modulo nyp 10) max-so-far) (iter (quotient nyp 10) (modulo nyp 10)))
          (else (iter (quotient nyp 10) max-so-far))))
  (iter n 0))


(define (remove-digit-i d n)
  ;; design roles (GI is the logical and of these)
  ;; n = [nyp][ap]
  ;; tp = 10^(num-digits ap)
  (define (iter nyp ap tp)
    (cond ((= (modulo nyp 10) d) (+ (* (quotient nyp 10) tp) ap))
          (else (iter (quotient nyp 10) (+ (* (modulo nyp 10) tp) ap) (* 10 tp)))))

  (iter n 0 1))


(define (sel-sort-i n)
  ;; design roles (GI is the logical and of these)
  ;;  (sorted n) = [(sorted nyp)][ap]
  ;;  tp = 10^(num-digits ap)
  ;;  ap is sorted
  ;;  any digit in nyp is <= any digit in ap
  
  (define (iter nyp ap tp)
    (cond ((zero? nyp) ap)
          (else (let ((d (max-digit-i nyp)))
                  (iter (remove-digit-i d nyp) (+ (* d tp) ap) (* 10 tp))))))

  (iter n 0 1))

;; check that the helper and calling functions all nest properly! 

;; what are the termination arguments?


  