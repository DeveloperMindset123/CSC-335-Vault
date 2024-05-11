#lang racket
#!/usr/bin/racket

; A list is a collection of s-expressions enclosed by parenthases
; below shows an example of an empty list, a special s-expression known as null (or empty) list, however note that an empty list is not an atom
; definition of an atom is the following --> atom is a string of characters (consisting of alphanumerical values)
;()

; A list consisiting of several empty lists is a list itself --> because () lists are considered s-expressions and a list is defined as a collection of s-expressions
;(() () () ()) --> this line was causing error during compile time

; THe car operator returns the head element/s-expression within a list
; example --> the car of the list (a b c) would be a
; example 2 --> the car of the list ((a b c) x y z) --> would be (a b c), since (a b c) is treated as a single x-expression
; example 3 --> asking something like what is the car of l where l is an atom --> this will return an error because primitives like car and cdr are reserved for lists only
; example 4 --> one exception of car is that we cannot ask the car element of an empty list --> example include --> what is the car of () --> this will return an error message as well

; This leads us to the following conclusion --> the law of car --> the primitive car is defined for non-empty lists

; similarly, something like cdr (coulder) returns everything BUT the head element/s-expression within a list
; example 1 --> what is the cdr of l where l is (a b c) --> (b c) --> because (b c) is the list l without (car l)
; example 2 --> what is the cdr of l where l is ((a b c) x y z) --> (x y z), because the first s-expression in this case is (a b c), s-expressions can be lists wihtin lists as well, s-expression represent both lists and atoms

; example 3 --> an interesting exception to consider --> what is the cdr of (cdr l) where l is (hamburger) --> this is an non-empty list, contians a single s-expression, therefore, since cdr excludes the first s-expression, the output will be ()

; just as how we cannot ask the car of an atom, we cannot ask the cdr of an atom, therefore, asking something like what is the cdr of l where l is hotdogs will return an error message.
; additional thing to note --> similar to car, we cannot take the cdr of an empty list either --> therefore, asking something like what is the (cdr l) where l is () will also result in an error

; The formal definition of cdr is the following --> the primitive cdr is defined only for non-empty lists. the cdr of any non-empty lists is always is always another list.

; We can use things like car and cdr nested as well, for example, take a look at the followng snippet of questions:
#|
- Example 1 --> uses car and cdr together --> What is (car (cdr l)) where l is ((b) (x y) ((c))) --> Let's try and understand what's going on here:
    - First, it's asking for the cdr, meaning (b) will be excluded the resulting output will be ((x y) ((c)))
    - Next, taking the car of this mutated list will return (x y), since (x y) is the new "first" s-expression

- Example 2 --> using cdr nested --> what is (cdr (cdr l)) where l is ((b) (x y) ((c))) --> again, we can break it down based on the behavior of the first and second cdr primitives:
    0 First, the first cdr will return ((x y) ((c))), then the next cdr will return (((c)))

- As a reminder, both car and cdr takes in non-empty lists as arguments
|#

; unlike car and cdr, which removes a certain s-expression, when it comes to prepending (adding to the beggining of an existing list), the primitive operator that is used is cons
; Thus, if we were to be asked the following question --> what is the cons of an atom a and the existing list l where a = peanut and l = (butter and jelly)? --> written as (cons a l) --> the resultig output is (peanut butter and jelly) --> here, the atom peanut has been added to the beggining of the list 

; understanding cons --> what does cons takes in as arguments --> cons takes two arguments --> the first is one is any s-expressions and the second one is any list
; a trickier example using cons would be the following --> what is cons s l where s is (a b (c)) and l is ()? --> let's break it down, it meets the requirement, the first argument being passed in is an s-expression, which in this case is a list and the second argument being passed in is a list --> in this case it's an empty list --> and therefore the resulting output will be --> ((a b (c)))
; if the second argument when using cons is an atom or both argument is an atom, cons will throw an error message instead
; The law of cons states the following --> The primitive cons takes in two arguments --> The second argument of cons must be a list and it returns a mutated list as an output

; we can define list using the following scheme syntax --> what is (null? (quote ())) --> This is a null predicate checking whether the list being passed in is empty or not, if so return true, otherwise return false --> null outputs a boolean value --> (quote ()) is another method of initializing list in scheme
; Another example of using the null? predicate --> what is the output of (null? l) || (null? (quote (a b c))) where l = (a b c) --> the output will be false since null expects the list it is expecting to be empty, in this case, list l is non empty
; Last case of using null? --> this will throw an error --> what is the output of (null? l) where l is sphaghetti --> No answer because we cannot ask the null? of an atom.
; In practice, null? is false for all instance except an empty list, so only use this to check and verify if null? is empty or not
; The law of null? --> the primitive null? is defined only for lists.

; just as there exists null? predicate, there also exists the atom? predicate --> example of using the atom? predicate is the following --> is (atom? s) true or false --> where s=Harry --> the output will be true --> and atom? is a predicate that either returns true or false, just as null? does.
; Example where atom? would return false would be the following --> is (atom? l) true or false? where l = (harry had a heap of apples) --> notice that although l is an list consiting of atom (lat), this doesn't mean it's an atom --> therefore, atom? will return false since l is a list, not an atom
; atom? takes in one argument --> and that argument can be any specific s-expression


; next predicate to cover is eq? --> predicate used to check if two s-expressions being passed in are the same non-numerical atom --> example (true) (eq? a1 a2) where a1=Harry, a2=Harry results in true --> example 2 (false) (eq? a1 a2), where a1=harry, a2=butter, results in false because they are two distinct atoms
; eq? takes in two arguments. Both of them must be non-numeric atoms
; in the case of eq, if we were to pass in 2 lists, or one atom and one list --> we wouldn't get any answer, nor if we pass in numerical values despite being atoms, since the restriction is that it must be non-numerical atom values
; The law of eq? states the following --> The primitive eq? takes two arguments --> each must be a non-numeric atom

; The next predicate is lat? (predicate used to check whether or not the s-expression being passed in is a list of atom or not) --> (lat? l) where l is (Jack Sprat could eat no chicken fat) --> this output is true since l is a list of atoms.
; Instance where lat? will return false is if the list contians s-expresions that are list (nested list) or empty lists
; Following shows the function definition for lat? --> recurisve implementation
(define lat?
    (lambda (l)
        (cond
        ; conditionl statement to check if the annoymous function's argument, an s-expresion, is an empty list or not --> this is the termination case
            ((null? l) #t)

            ; Otherwise, check if the first element is an atom or not 
            ((atom? (car l))
            
            ; Then, recursively continue to check if the list's remaining element --> which we can reduce using cdr is an atom or not
            ((lat? (cdr l)))
            (else #f)
            )  
        )
    )
)
; we can run a test case to see what the resulting output will look like
(lat? (quote (bacon egg and cheese)))

; the member? predicate function is used to check is a certain s-expression is contained within an existing list of atoms
; The first commandment --> always ask null? as the first question in expressing any funnctions
; Note : the member? predicate is used to check if an atom is present within a list of atoms