; Priotorize the homework problems in the following order --> hw 8, hw 10, h2 9 and then hw 11, additionally, look into the correct implementation of cons, car, cdr 



; Eighth Homework Set
; CSc 335

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Homework Problems

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




; 1.  Prove the correctness of the value function presented in the class notes.  Your argument should be an
; induction on (the complexity of) the argument.  State clearly what assumptions you make.


; 2.  Make all changes needed to allow the a-iexp calculator to work with numbers given in base 1.
; Explain carefully how you will represent such numbers; design plus, times, and exponent procedures
; for them.  Prove correctness of your functions.

#|
Some things to note in regards to list prior to it's implementaiton:
- In Scheme, the 'append' primitive is used to concactenate two or more lists into a single list. It is particularly useful in situations where you need to combine seperate lists of data into a unified sequence.

Purpsoe of append:
- concacternation : The primary function of 'append' is to concactenate lists. it takes any number of lists as arguments and combines them into one continous list.
- preservation of order : append preserves the order of elements as they appear in the input lists. The elements of the first list appear first, followed by the elements of the second list, and so on.
- Non-Destructive : append does not modify the original lists, it retuns a new list. This characteristic is important for functional programming styles to avoid side effects.

The following is the implementaiton of append: --> if we were to implement this primtive from scratch
(define (my-append a b)
; if a is null, return b since concancternating an empty list is the ame as returning the other list
(if (null? a) b)
; otherwise, add the first element from list a and reduce list a by one, and while doing so, also pass in b, we will output b and the recursive implementaiton will end once list a is empty, then we just return b, and this concacterning a newly created list consisting of list a and b
(cons (car a) (my-append (cdr a) b))))

Important list primitives that is predefined to remember moving forward:
- car : Returns the first element of a list
- cdr : returns all elements in a list except the first
- cons : constructs a new list from a given element and a list by prepending the element to the list
- list : constructs a list from it's arguments
- length : returns the length of a list
- list? : checks if a given object is a lit or not
- null? : checks if a given list is empty
- reverse : Returns a new list with the elements of the original list in the reverse order
- member : Checks if an element is in a list
- map : applies a function to each element of a list and returns a list of results
- filter (not built-in in some dialects) : Filters elements of a list based on a predicate function. --> consider conditional statements 

Now moving forward in regards to the representation of numbers in base 1:
- To implement the arithemtic calculator in Scheme R5RS for numbers represented in base 1, we need to first establish how numbers will be represented in this unusual base and then define arithmetic operations for them. Base 1, often referred to as unary notaiton, represents numbers using a series of marks or tokens, typically `1`'s. Each `1` represents a single unit.

Representaitons of numbers in base 1:
- In Scheme, we can represent numbers in base 1 using lists, where each 1 is represented as a dot (.) in the list, for example:
- `1` in decimal would be `(.)` in base 1.
-  2 in decimal would be `(.) (.) in base 2
- 3 in decimal would be (.) (.) (.)
- n in decimal would be represented using n number of dots, notice that this is the pattern that's involved.

Arithmetic Operations:
We'll need to define three procedures :
1. Plus(base1-plus) : Concactenates two base 1 numbers.
2. Times(base1-times) : Repeats a base 1 number a certain number of times based on another base 1 number.
3. Exponent(base1-exponent) : Raises a base 1 number to the power of another base 1 number by repeatedly applying base 1 time.
|#

; implementation of the base1-plus:
(define (base1-plus a b)
  ; refer to the definition of append within the comments to undertand how it's implemented. --> this function appends two base 1 lists, effectively adding/concactenating them.
  (append a b))

(define (base1-times a b)
  (if (null? b) (quote ())
      ; notice the difference in this case, we are appending the list a to list b and reducing list b each time --> in other words, the recurisve function multiplies two base 1 numbers by appending the list 'a' to itself as many times as there are elements in 'b' --> while also reducing b and the termination case is once list b is empty
      (append a (base1-times a (cdr b)))))

; implementation of base1-exponent --> this function raises a to the power of b by recursively multiplying a by itself b times using base1-times.
(define (base1-exponent a b)
  (if (null? b)
      ; base 1 representation of 1 (at 0th place)
      (quote ("dot"))
      ; rather than append, it calls on base1 times as many times as the length of b (meaning each time base1-times gets called, we are appending a that many times to b, this is similar to a dual for loop)
      (base1-times a (base1-exponent a (cdr b)))))

; Proof of correctness via induction:
#|
Plus(base1-plus):

Base Case : adding 0 (empty list) to a number when b is null.
inductive step : Assume a list of size 'n' works correctly. For n+1 (i.e. (append (.) a) and b), the function produces '(append (append '(.) a) b)', which by definition of list append and by induction hypthothesis is correct.

base1-times:
Base case : when b is null, return an empty list --> meaning we have added a the number of times there are b

|#

; consider the following function to create a power set
(define (subsets s)
  ; return an empty list if s is null
  (if (null? s) (quote ())
      ; otherwise, utilize the let binding to define a function
      ; rest variable contians the recursively called subsets (with list s being reduced once)
      (let ((rest (subsets (cdr s))))
        ; use append to concactenate rest, and map to apply the logic for each of the individual element within the list
        (append rest (map (lambda (subset) (cons (car s) subset)) rest)))))

; we can test out this function
(subsets (quote (1 2 3)))

; Note that the following is the implementation of the dot product in scheme --> using the accumualte function
(define (dot-product v w)
  ; the operation being done is addition, the intitial value being used is 0 and the sequence being implemented is using the map function for the list v and w (where v and w is being multipleied and the resulting products are being added --> this is the definitionof map)
 (accumulate + 0 (map * v w)))

; the following is the implemetaiton of the amtrix vector implementation in scheme
(define (matrix-* vector m v)
  ; the difference between matrix vector and dot product is that --> call on the annoynomous function that the map is applying ot each of the row, which in this case is the dot product being applied to each of the row values and the list v, where the dot product is being applied to each of the row values within the matrix 
  (map (lambda (row) (dot-product row v)) m))

; functional implementation of the transpose function in scheme --> accepts one paraemter, that being the matrix --> transpose does nothing but swap the row and the column values
(define (transpose mat)
  ; here, the accumulate is passing in an annonoynomous operation, and the annoynomous operation being eprformd 
  (accumulate (lambda (x y) (map cons x y))













; 3.  Exercises 2.2, 2.3 and 2.4 in Abelson and Sussman.  Proofs are not necessary.



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

