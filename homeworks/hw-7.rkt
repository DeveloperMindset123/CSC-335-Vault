
; Seventh Homework Set
; CSc 335


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Homework7.scm

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Here are some homework problems to get you started with lists

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Note that I have sometimes deliberately offered incomplete specifications - if you find this
; to be the case, you will need to complete the specification as you deem best.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Give both recursive and iterative procedures (along with a development/proof) for each.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; 1.  Write your own version of length using the list functions we have discussed.  You can find
; length documented at http://www.schemers.org/Documents/Standards/R5RS/
; One potential method of caluclating the length of a list is to have a method where we reduce the list recurisvely using cdr and add 1 each time, once the list is empty, output 0 and we can check if the list is empty or not using the null? operator

(define (my-length n)
; define function body for my-length
; first amendment of scheme states that 
(cond ((null? n) 0)
    (else 
    (+ 1 (my-length (cdr n)))
    )
)

; The idea is suggested by (my-length '(a b c d)) = 4.  


; 2.  Write your own version of list-ref using the list functions we have discussed.  You can find
; list-ref documented at http://www.schemers.org/Documents/Standards/R5RS/

; Briefly, the idea is indicated by this example:  (my-list-ref '(a b c d) 2) = c.  Note the 0-based
; indexing.  What happens if the input index exceeds the size of the input list?

; potential algorithm idea --> loop through the list and decrement the counter each passing

; 3. Write a function start that takes two arguments, lst and num, and which returns the
; first num elements of lst.

; 4.  Write a function but-last that takes two arguments, lst and num, and which returns the
; list of all but the last num elements of lst.

; 5.  Write a function end that takes two arguments, lst and num, and returns the last num
; elements of lst.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; suggested reading:

;;    http://en.wikipedia.org/wiki/Scheme_(programming_language)

; Something important to note --> let* permits bindings to refer to variables defined earlier in the same construct.

; We can define a global scheme varibale in the following manner:
(define var "goose")
; Any reference to var here will be bound to "goose"
;we can define local variable in the following manner
(let ((var 10))
    ; statements goes here. Any reference to var here will be bound to 10.
    (+ 5 var)
)
; Any reference to var here will be bound to "goose".

; a varinant of let will be let* --> let* permits bindings to refer variables defined earlier in the same construct, and example of that would be the following:
(let* ((var1 10)
    (var2 (+ var1 12)))
    ; Although we can refer to var1 on the line where var2 is defined, we cannot reference var2 on the line where var1 has been defined, since var2 has yet to be initialized
)

; The other variant, letrec, is designed to enable mutually recursive procedures to be bound to one another
; Take a look at the following example to better understand the implementation: --> randomly land between 0 or 1 depending on the value n that has been provided
(define (hosfstadter-male-female n)
    (letrec ((female (lambda (n)
    ; if n=0, output 1
        (if (= n 0) 1)
        ; otherwise, do the following, notice that we are referencing the male value which hasn't been defined yet in the code, letrec allows us to reference variables prior to their initializations --> in simple terms, set the value of female after decrementing n by 1, set that to the value of female, then set the value of the male to the female and then perofrm the operation n - male
        (- n (male (female (- n 1)))))))

        ; now we are defining the male value, this is similar to binary classificaiton, with female being set to 1 and male being set to 0
        (male (lambda (n) 
        ; in the instance that n is 
            (if (= n 0) 0)
            ;else, do the following
            (- n (female (male (- n 1))))))
        
        )
)
