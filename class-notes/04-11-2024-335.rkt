#|
- can elem (i.e. member?) be implemented using accumulate? Start by figuring out what it needs to be
(lambda (x y) ... )
|#

; consider the code for accumulate-left

(define (accumulate-left op init seq)
  (define (iter acc rest)
    (if (null? rest)
        acc
        (iter (op acc (car rest))
              (cdr rest)))) 
  (iter init seq))

(define (testElemInList e I)
  (accumulate-left
   (lambda (acc first) (or acc (eq? first e)))
   #f
   I))

; Test elem
(testElemInList 3 '(1 2 3 4 5)) 

; AVA : fold-right (accumulate-right) (fold-right) --> given the below function, consider why is this said to be 'right' fold? --> note that professor defined this as accumulate, not accumulate-right
(define (accumulate-right op init seq)
  (if (null? seq) 
      init
      (op (car seq)
          (accumulate-right op init (cdr seq)))))

;we are passing in empty list as a parameter --> applying car on an empty list gives the empty list, way of calculating the leftmost, assuming the list is not empty
(define (leftmost seq)
  (accumulate (lambda (x y) x) '() seq))

; Look at the makeup class notes, he posted one of the solution to HW 7 --> can be found in classes 20 and 21
; See if you can write reverse using accumulate-left and accumulate-right
; Refer to the notes in regards to what to solve as part of homework questions

; You should also aim to solve this on your own (refer to his notes from the makeup notes office hour)
((repeated square 2) 5) ;output: 625
; I am falling behind, need to catch up with the homework
; Funcitonal programming is predominantly about recognizing/identifying patterns

;another function that needs to be memorized is the filter function
(define (filter pred seq)
  (cond ((null? seq) seq)
        ((pred (car seq)) (cons (car seq) (filter pred (cdr seq))))
        (else (filter pred (cdr seq)))))

; Consider how we can use accumulate right on the filter function defined above
; Reworking filter to use accumulate-right (accumulate is the Abelson and Sussman for this function)
; we can redefine as the following --> you would never use list as a parameter name, something to note, use lst instead as a suitable parameter name
(define (filter pred lst)
  (accumulate
   (lambda (x y)
     (if (pred x) (cons x y)

         y ; otherwise, toss and return y
         ))
   '()  ;if nobody satisfies the predicate, have empty list
   lst))

; Link to understand cons: https://stackoverflow.com/questions/5741111/help-explaining-how-cons-in-scheme-work

; Suggested for memorization --> accumulate, filter 
; cddr --> takes the cdr of the cdr, skipping the two pairs in a list and returning the rest of the list
; cadr --> takes the car of the cdr, which gives you the second item in the list --> this trick can be used to retrieve an individual element within a list

; enumeration definition --> base case, f
(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

; below function is from notes: Class 20, April 9, 2024
(define (enumerate-tree tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

; Let's look at 2 versions of postcondition for enumerate-tree. The first being
; 1. Post 1: returns a list of the leaves of a tree (no mention of the order of these)
; 2. Post 2: returns the list of the leaves of a tree, in the same order as they occur in a tree.
; Look at Abelson and Sussman, there's a book called Scheme in 48 hours, which has R5RS



; Abelson and Sussman have a signal processing approach
(define tree-2 (list 1 (list (list 2 3) (list 4 5) (list (list 6))))
  (define (sum-odd-squre tree)
    (accumulate + 0
                (map square
                     (filter odd?
                             (enumerate-tree tree))))))

(define (fib n)
  (define (aux curr prev count)
    (if (= count 0)
        prev
        (aux (+ curr prev) curr (- count 1))))
  (aux 1 0 n))

; refer to his notes from Class 20 April 9 2024.pdf to understand the coding implementation. If your pressed for time, jump and look at homework 10, do a couple problems from each homework

                






