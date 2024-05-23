
;; solution developed by divide and conquer

;; here's the idea: assuming n has at least 3 digits, construct the largest 2 digit number from (modulo n 10) and
;; the digits of the number returned by the call on (quotient n 10)

;; p and q are digits
;; p is not 0
(define (make-num p q)
  (+ (* 10 p) q))

(define (largest-2-digit-number-from-2-digits p q)
  (cond ((>= p q) (make-num p q))
        (else (make-num q p))))

(define (largest-2-digit-number-from-3-digits p q r)
  (let ((m (min p q r)))
    (cond ((= m r) (largest-2-digit-number-from-2-digits p q))
          ((= m q) (largest-2-digit-number-from-2-digits p r))
          (else (largest-2-digit-number-from-2-digits q r)))))

;; n > 0 is an integer
(define (largest-2-digit-number-from-digits-of-n n)
  (cond  ((< n 10) (make-num n n))
         ((< n 100) (largest-2-digit-number-from-2-digits (quotient n 10) (modulo n 10)))
         (else (let ((m (largest-2-digit-number-from-digits-of-n (quotient n 10))))
                 (let ((p (quotient m 10))
                       (q (modulo m 10))
                       (r (modulo n 10)))
                   (largest-2-digit-number-from-3-digits p q r))))))







;; solution using an accumulator

;; n > 0 is an integer

(define (ilargest-2-digit-number-from-digits-of-n n)

;; rsf is the largest 2 digit number formed from the already processed digits

;; N = [nyp][ap]

;;;; extended bracket notation, which allows (for example) 123000 to be written as [123][000], and
;;;; 123004 to be written as [123][004]

  
  (define (iter nyp rsf)

    (newline)(display rsf)(newline)
    
    (cond ((zero? nyp) rsf)                           
          (else (let ((l-rsf (quotient rsf 10))
                      (r-rsf (modulo rsf 10)))
                  (iter (quotient nyp 10)
                        (largest-2-digit-number-from-3-digits (modulo nyp 10) l-rsf r-rsf))))

          ))
    
  (cond  ((< n 10) (make-num n n))
         ((< n 100) (largest-2-digit-number-from-2-digits (quotient n 10) (modulo n 10)))
         (else (iter (quotient n 100) (largest-2-digit-number-from-2-digits (modulo (quotient n 10) 10) (modulo n 10))))))

