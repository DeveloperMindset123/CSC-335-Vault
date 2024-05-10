; still scheme, think of the extra credit question over the week
; The problem could not possibly be approached by throwing down code
; We were expected to try and understand what he means by LD numbers
; Some more on cons-cell structures, let's start with an example
; If you read the problem through and spend some time thinking about it, a lot of the thinking to solve the EC problem will be directly relevant to the mideterm
; --> for the PM section at least
; The problem requires a mix of iteration and recursion
; The divide and conquer that works is an iterative divide and conquer, add to an accumulator with recursive calls
; Take the LD number, break it down, and make recursive calls and add it to the accumulator
; One potential method is to recursively/iteratively define what the single digit is
; The base case has been specified.

; cons: the cons function is used to create a new pair. It takes two arguments, usually an element and another pair (or '()' representing an empty list)
; --> and returns a new pair where the first argument is the 'car' (head) of the pair and the second argument is the cdr (tail) (pronounced coulder)
; cons is essentially prepending an atom (element) onto a list
(cons 1 2)  ; the output: (1 . 2)
(cons 3 4) ; output: (3 . 4)

; we are expected to have the recursive and iterative solution for the EC due next week
; we don't return list for the miderm/EC
; LD stands for List of Digits

; consider the box and pointer diagram (for (lunch))?  --> notice that this is a list of length 2
; --> so you might think, the length of the list is the number of cons cells in the box and pointer representaiton
; refer to notes on notebook to see the box and pointer diagram
; Refer to homework 7 to get additional practice on lists
(define my-list '((1 2) (3 4)))
(car my-list)  ;return (1 2) --> aka the first atom, head atom value (even tho in this case it's a
; lists and dotted pairs are the same.
; some lists are primitive pairs
; list and list? are primitive
list?

; In the homework, we are asked to define our own lenght function, in Racket, disallow usse of primtivies (note that length itself is a primitive)
; Another primitive is "append"

(define l (append '((a b)) '(a b)))  ;crushes the list structure, returns

; a set is a multiset where the multiplicity is all 1. multisets and sets in general aren't ordered
; reverse is a primitive (reverses the element of a list without crushing the internal structure of the list)
; another is liftref, lists are regarded as zero index

; consider the following list
(define l7 (append '(a b) '(c d)))

;( a b c d)
(list-ref l7 0)  ;output: a
(list-ref l7 1) ; output: b

;list primitives to remmeber: pair?, list and list?, length, append, reverse, list-ref

; 2 things to consider for defintion: soundness and completeness. (conisder doing the quesition within the induction directory on teams)
; A list is soemthing that has a box and pointer diagram, whcih has a cons cell backbone ending in an empty list
; Consider a porgram which is to input a list, and output #t if the input is a list of atoms, and #f otherwise

; Here, we will assume that:
; list :: () | (cons atom list) | (cons list list)
; and
; lat ;;= () | (cons atom lat)
; atom := a | b
; Note that lists are s-expressions but not all s-expressions are lists (s-expressions may also contain atoms)
; see if you can define a function that will calcuate and check if the length of a list is less than the integer n (n can be 20 or 120)
; Soundness:show that a list based program only builds structures
; Completeness:

; Take a look at the following program: input is a list
(define (lat? l) ;returns true if l is a lat and false otherwise (refer to the definition of lat on the notes)
  (cond ((null? l)  #t) ;termination case
        ((atom? (car l)) ;atom? is something we will have to define ourselves, if the current value is an atom, return the recursive call
         (lat? (cdr l)))  ;recurisvelycall on the coulder of the list l
        (else #f)))


;The design is ferquently described as "cdring down" design. You can think of the above program as a recursion based call on an weak program.
; lat? is a list of atoms
; This program can also be described as based on weak induction (on the length of it's input)
; and also as based on structural induction (on the structure of list)

;let's write another program where input is a lat
(define (memeber? a lat)
  (cond ((null? lat) #f)
        ((eq? a (car lat)) #t)
        (else (member? a (cdr lat)))))  ;syntactically iterative, there's no giard on the iteartion appraoch, and becasue it's iterative, it has an invariant.


;The invariant of the abov eiteative procedure would be
        

; list of homework questions, balance parenthases, box and pointer diagram
; a is a member of LAT iff 
