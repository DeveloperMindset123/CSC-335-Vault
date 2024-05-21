;; CSc 335
;; Spring 2024
;; Project

;; Released April 16 2024
;; Due by 23:59:59 May 18 2024, via email


;; I ask for complete developments and proved, working code for three problems:

;; 1.  Write and prove correct a program to input an LD number (as previously defined) which returns a list of all lists of
;; 1s and 2s which map to that number.  Use this program to check your solution to the takehome part of the midterm.
(define (add-end lists number)
(append lists (list number))
)

(define (numberlist number)
(cond ((= number 0)(list ))
(else (append (numberlist (quotient number 10)) (list (modulo number 10))))))

(define (addlist init temp)
(define (addlist init templist anslist)
    (cond ((null? init) anslist)
    (else (addlist (cdr init)
    templist (cons (append (car init) templist) anslist)))))
(addlist init temp (list )))


(append (list 1 2 3 ) (list 2 3 4) (list ) )
;(define (main lists temp largelist)
;(cond ((null? lists) largelist)
;((= (car lists ) 1) (main lists temp
;(append (addlist (permutations (lists)) temp)
;(addlist (permutations (temp)) lists)  largelist))
;(else main (cdr lists) (cons (car lists) temp) largelist))))
;)
(define (get-end lists )
   (cond ((=(length lists) 1) ( car lists))
    (else (get-end (cdr lists)))))

(get-end (list 1 2 3 4 5 6 ))


(define (isld? lists)
(if (and (= (car lists) 1) (= (get-end lists) 2))
    #t
    #f))
(define (perm lists)
(define (permute numbers res)
(cond
    ((null? numbers) (cons res (list )))
    ((and (not (= (length res) 0)) (= (car numbers) 2))
    (append (permute (cdr numbers) (cons res (list ))) (permute (cdr numbers) (add-end res 2))))
    (else (permute (cdr numbers) (add-end res (car numbers) ) ))))
    (permute lists (list )))


(define (linears lst)
(define (linear number ans)
(cond ((null? number) ans)
        ((= (car number) 1) (linear (cdr number) (append (perm (cdr number)) ans)))
        (else (linear (cdr number) ans))))
(linear lst '())
)


; Example usage:
(list 1 2 3 1 4 2 5 2)
(perm '( 1 2 2 2))
(display "\n")
;(display (linears '(1 3 1 4 2 5 2)))

;; 2.  Write and prove correct a syntax checker for TLS-scheme, as specified in HW 11.
(define (simple-check expr)
  (cond
    ((number? expr) #t)
    ((boolean? expr) #t)
    ((symbol? 


;; 3.  Write and prove correct an interpreter for TLS extended by let*.
(define *let
  (lambda (e table)
    (let*
        ((bindings (cadr e))
         (vars (map car bindings))
         (vals (map cadr bindings))
         (body (caddr e))
         (lambda-expr (cons (list (quote lambda) vars body)
                            vals)))
      (meaning lambda-expr table))))

;; Function definition for *let*
(define *let*
  (lambda (e table)
    (let*-eval (cadr e) table (caddr e))))

; define the helper function.
(define let*-eval
  (lambda (bindings table body)
    (if (null? bindings)
        (meaning body table)
        (let* ((bindings (car bindings))
               (var (car bindings))
               (val (meaning (cadr binding) table)))
          (let*-eval (cdr bindings) (extend-table (new-entry (list var) (list val)) table) body)))))

