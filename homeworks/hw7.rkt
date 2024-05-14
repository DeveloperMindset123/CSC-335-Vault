
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

; Some important tips to note --> use cons to build lists, use cdr to reduce lists and car to extract values from a list (null?, eq?, zero?, lat? are all helpful list predicates as well)

; 1.  Write your own version of length using the list functions we have discussed.  You can find
; length documented at http://www.schemers.org/Documents/Standards/R5RS/
; One potential method of caluclating the length of a list is to have a method where we reduce the list recurisvely using cdr and add 1 each time, once the list is empty, output 0 and we can check if the list is empty or not using the null? operator

(define (my-length n)
; define function body for my-length
; first amendment of scheme states that 
(cond ((null? n) 0)
    (else (+ 1 (my-length (cdr n))))))

; TODO: Implement the iterattive version --> provide the guessing invariant, strong, weak and good enough test, the pre and the post condition, induction proof.
; TODO : Provide the appropriate developmental proof, in this case, since it's a recursion, provide the design idea

; HINT: The idea is suggested by (my-length '(a b c d)) = 4.

; run some test cases to see if my-length works as intended --> all test cases passed --> recursive implementation successfull
(display "Testing my-length function")(newline)
(display "Test case 1: ")(my-length (quote (a b c d)))(newline)
(display "Test Case 2: ")(my-length (quote (1 2 3 4 5 78)))(newline) 
(display "Test case 3: ")(my-length (quote ("A" "AB" "AC")))(newline)
 


; 2.  Write your own version of list-ref using the list functions we have discussed.  You can find
; list-ref documented at http://www.schemers.org/Documents/Standards/R5RS/

; Briefly, the idea is indicated by this example:  (my-list-ref '(a b c d) 2) = c.  Note the 0-based
; indexing.  What happens if the input index exceeds the size of the input list? --> think of try and catch block, out of indexing range, will need to indicate an error message to user

; NOTE : Start of my own implemenetation below

; Note that since Scheme doesn't come with it's own predefined loops, we will need to define the helper function for a while loop, since while-loop can help easily solve this problem
(define (while-loop counter limit curr-list)
  ; define the termination case for the loop, the procedures will continue while this contiion holds true
  (if (< counter limit)
      (begin
        ; display the value of the current counter --> note that this may even not be neccessary
        (display counter)
        ; in the case that the display syntax isn't neccessary, the newline syntax may not also be unneccesary
        (newline)
        ; recurisvely increment the counter, this is the underlying structure of a for loop, you should understand this pattern that is being seen
        (while-loop (+ counter 1) limit))))

; Function definition to display error message in case something goes wrong --> the helper function error takes in one parameter, the string message to be displayed
(define (error message)
  ; We need to be able to distinguish between regular messages and error messages in Scheme
  (display "Error!: ")
  (display message)
  (newline))

; end of while loop helper function
; IN order to handle input entered exceeding the length of the list, we can use a conditional to check if the length of the list is less than the input index entered, if so, the index will be out of range (however, note that since the length starts at 1 and the indexing is always 1 less than the length of the list, we will need to adjust the length accordingly --> note that we can do a direct comparison or save the result to a local variable and perform the comparison as needed, it would be easier to just do direct comparison rather than worry about the appropriate range of the let binding that is being used.

(define (my-list-ref list-param num-param)

  ; use a conditional to check if the parameter values being passed in are list and num appropriately, enforcing type checking
  (if (and (list? list-param) (number? num-param))

      ; understanding the purpose of the begin primtiive in Scheme --> In Scheme, the begin primitive is used to sequence multiple expressions, ensuring that they are evaludated in a specific order, and to return the value of the last expression evaluated. This si particularly useful in several contexts where multiple expressions needs to be evaluated for their side effects, but where only the result of the last expresison is needed as the return value. IN simple terms, begin is commonly used in the bodies of procedures or functions where multiple statements need to be executed one after another. --> otherwise, we will need to continue iterating through the list using cdr and reducing the input value by 1 each time
      
      (begin
        ; Perform operations here if the parameter types are correct --> use the display message to indicate success
        (display "Parameters passed in are correct types, processing...")(newline)
        ; conditional to handle out of range index --> note that error isn't a built in primitive, therefore, we will first need to define it as a helper function prior to using it, otherwise, it will result in the following error message in the terminal --> "error:undefined; cannot reference an identifier before its definition".
        
        (cond ((> num-param (- (my-length list-param) 1)) (newline) (error "Attempted to access out of range index"))
              ; However, in the instance that the index isn't out of range, we will need a method to decrement the index value each time we iterate through the list and reduce the index, we can utilize a while loop focused helper function which will continue to run while the num-param isn't 0

              ; termination case, when num-param is 0, return the car value
              ((zero? num-param) (car list-param))
              ; otherwise, recurisve implementation --> reduce the list, and the index value
              (else (my-list-ref (cdr list-param) (- num-param 1)))))
      
      ; Otherwise, in the case that the parameter pass failed
      (begin
        (error "Invalid arguments types : expected (list, number)."))))

; Test out the function to check if it's working as intended --> test case 1 and 2 passed, 3 failed
(display "Test Case 1, accessing atom at index 2 of the provided list: (Expected : 3), Actual: ")(my-list-ref (quote (1 2 3 4)) 2)(newline) 
(display "Test Case 2, accessing atom at index 4 of the provided list: (Expected :  ), Actual: ")(my-list-ref (quote ("a" "b" "c" "d" "e" "f")) 5)(newline)
(display "Test case 3, attempting to access an out of bound atom in the provided list: ")(my-list-ref (quote (10 20 30)) 8)(newline)
(display "test case 4, passing in mismatched data types (expect an error message to be displayed) ")(my-list-ref 2 (quote (1 2 3 4)))(newline)

        
        
; Obervation : From the look of it, we can see that the function my-list-ref accepts 2 arguments, the first being the list and the second being the 

; potential algorithm idea --> loop through the list and decrement the counter each passing

; 3. Write a function start that takes two arguments, lst and num, and which returns the
; first num elements of lst.

; One hint we are given is that the first parameter has to be of type list and the second parameter has to be of type num, and we need to return all the elements that comes before the num index, one method of doing this to check and verify if num is a positive number or not, have a counter that will continue to iterate until we reach that specified num value.

#| --> original code implementation, resulting in an error, and wasn't easy to read, this function can be broken down into two smaller functions (meaning we can define and utilize a helper function instead)
(define (elements-before-num lst num)
  ; check and verify if the parameters being passed in are strictly correct using predicate checker
  (if (and (list? lst)(number? num))
      ; use begin to indicate that the sequence has begun
      (begin
        (display "Parameters passed the type check, processing...")(newline)
        ; define logic for how to show the elements that comes before the num (recall that we can use cons to build list and cdr to reduce the existing list)
        ; First, do a simple check (and this will also be our termination case), which will check if we have reached the end of the index values
        (if (or (zero? num) (null? lst)) (quote ()))
         ;((eq? num 0) (quote ()) --> this is what I originally had, but this didn't handle edge cases for an empty list
              ; else --> reduce and build the list while reducing the num simultaneously
            (cons (car lst)) (elements-before-num (cdr lst) (- num 1)))
      (begin
        (error "invalid arguments types : expected (list, number)."))))

(display "Test case 1, passing in correct dataypes and checking if all elemeents upto the index specified is printed out and built as a new list or not --> ")(elements-before-num (quote (1 2 3 4)) 2)(newline)

|#

; Alterantive (working) approch to the above question
(define (elements-before-num lst num)
  (if (and (list? lst) (number? num))
      (begin
        (display "Parameters passed the type check, processing...")(newline)
        ; build-list has been defined below --> helpers help abstract away a lot of the conceptual understanding involved --> end the begin block and define the code for what will happen if the type checking fails
        (build-list lst num))
      (begin
        (error "Invalid argument types : expected (list, number)."))))

; Define helper function build-list
(define (build-list lst num)
  ; check if num value is 0 or lst is empty, and additionally, this also serves as the termination case as we will need to be returning an empty list in the case that there are no more elements to process, it's important that we return an empty list, because otherwise, there would be no list to cons into, and cons requires a list to exist to build a proper list, even if that list is empty
  (if (or (zero? num) (null? lst)) (quote ())
      ; else, do the following
      ; recursively build the list
      (cons (car lst) (build-list (cdr lst) (- num 1)))))

(display "Test case 1, passing in correct dataypes and checking if all elements upto the index specified is printed out and built as a new list or not --> ")(elements-before-num (quote (1 2 3 4)) 2)(newline)
(display "Test case 2, passing in the correct datatypes and checking if all the elements upto the index specified is printed out and built as a new list or not, only difference is that I am attempting to access an out of bound index --> ")(elements-before-num (quote (1 2 3 4)) 7)(newline)
(display "Test case 3, passing in the incorrect datatype and checking if the correct error message gets printed out in the terminal or not, mismatched position of arguments results in --> ")(elements-before-num 2 (quote (1 2 3 4)))(newline)

; 4.  Write a function but-last that takes two arguments, lst and num, and which returns the
; list of all but the last num elements of lst. --> does this mean it skips the num element and returns every other element in the list?

; The question is somewhat vague and thus makes it difficult to understand what it's asking for explicitly, however, based on going on the assumption that we are to develop an algorithm that returns all the elements left of num and all the elements right of num index, but exclude num index we can implement the following intutition:

; 1. Develop a helper function to develop the left side of the list
; 2. Develop a helper function to develop the right side of the list
; 3. Call them both within a single function


; let's first attempt to implement the recursive implementation of this algorithm --> we will simply need to cdr at the point where the nums appears (twice), rather than once and otherwise, the rest of the implementation can remain more or less the same
(define (return-all-except-index-num lst num)
  ; perform type checking to make sure that we are passing in a list and number accordingly
  (if (and (list? lst) (number? num))
  (begin
    (display "Parameters passed type checks, processing...")(newline)
    ; Implement logic for building list
    (cond 
      ; Applying the first amedment, we will always need to ensure that list passes the null? check
      ((null? lst) (quote ())
      ; In the case that we have reached the num that the index value specified, we simply cdr 
      ((zero? num) (cdr (cdr lst)))
      ; otherwise, recurisvely reduce and build the list
      (else (cons (car lst) (return-all-except-index-num (cdr lst) (- num 1)))))))))


; 5.  Write a function end that takes two arguments, lst and num, and returns the last num
; elements of lst.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; suggested reading:

;;    http://en.wikipedia.org/wiki/Scheme_(programming_language)


; Beyond represents my own notes in regards to scheme let binding differences (let, let* and letrec)
; Something important to note --> let* permits bindings to refer to variables defined earlier in the same construct.

; We can define a global scheme varibale in the following manner:
(define var "goose")
; Any reference to var here will be bound to "goose"
;we can define local variable in the following manner
;(let ((var 10))
    ; statements goes here. Any reference to var here will be bound to 10.
 ;   (+ 5 var)
;)
; Any reference to var here will be bound to "goose".

; a varinant of let will be let* --> let* permits bindings to refer variables defined earlier in the same construct, and example of that would be the following:
;(let* ((var1 10)
 ;   (var2 (+ var1 12)))
    ; Although we can refer to var1 on the line where var2 is defined, we cannot reference var2 on the line where var1 has been defined, since var2 has yet to be initialized
;)

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
            (- n (female (male (- n 1))))))))

(display var)
