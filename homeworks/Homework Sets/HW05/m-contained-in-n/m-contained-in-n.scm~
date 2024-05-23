
;; solution disregarding multiplicities

(define (contained? m n)
  (let ((m0 (modulo m 10))
        (n0 (modulo n 10)))
    (cond ((= m0 n0)(if (< m 10)
                        #t
                        (contained? (quotient m 10) n)))
          (else (if (< n 10)
                    #f
                    (contained? m (quotient n 10)))))))

;; solution taking multiplicities into account


