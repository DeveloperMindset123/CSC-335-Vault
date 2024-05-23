
;; helper functions

(define (count-digits n)
  (cond ((zero? n) 1)
        (else (iter n 0))))

(define (iter remaining count)
  (cond ((zero? remaining) count)
        (else (iter (quotient remaining 10) (+ count 1)))))

(define (cube x)
  (* x x x))


;; recursive solution: sketch

;; divide and conquer using (quotient n 10)

(define (mapnum n)
  (cond ((< n 10) (cube n))
        (else (let ((newdigits (cube (modulo n 10))))
                (if (zero? newdigits)
                    (* (mapnum (quotient n 10)) 10)
                    (+ (* (mapnum (quotient n 10))(expt 10 (count-digits newdigits)))
                       newdigits))))))



;; iterative solution : sketch

(define (imapnum n)

  ;; pt = (expt 10 (count-digits result-so-far))

  ;; (mapnum N) = [(mapnum remaining)] [result-so-far]

  ;;;; extended bracket notation, which allows (for example) 123000 to be written as [123][000], and
  ;;;; 123004 to be written as [123][004]

  (define (iter remaining result-so-far pt)
    (cond ((zero? remaining) result-so-far)
          ((zero? (modulo remaining 10)) (iter (quotient remaining 10) result-so-far (* pt 10)))
          (else
           (let ((next-digits (cube (modulo remaining 10))))
             (iter (quotient remaining 10) (+ (* next-digits pt) result-so-far)
                   (* pt (expt 10 (count-digits next-digits))))))))
                      

  (let ((first (cube (modulo n 10))))
    (iter (quotient n 10) first (expt 10 (count-digits first))))

  )

 