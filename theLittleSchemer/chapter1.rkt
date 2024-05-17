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

; what is rembar --> scheme syntax pseudocode --> (rember a lat) --> (lamb chops and jelly) --> "Rember" stands for removing a member (treat member as an element within a list, a list of atoms to be specific) --> example uses : (rember a lat) where a = mint and lat = (lamb chops and mint flavored mint jelly) --> since there's two mint present, the first time mint appears will be removed from the list in this case and the resulting mutated list will be  --> (lamb chops and flavored mint jelly)

; function definition for rember (removing a member from a list): --> note that this predicate doesn't return a boolean value, rather it returns a mutated list (with the specified value being removed from the list)
(define rember
    (lambda (a lat)
        (cond 
        ; if the list of atoms passes the null operator check (meaning the list is empty as that is the only case that the null operator will return true) --> return an empty list in that case --> note that this also serves as the termination case to the recurison logic as well.
            ((null? lat) (quote()))
            (else (cond
            ; otherwise, compare and check if the value a (the s-expression [atom] to be removed from the list) is equal to the first value in the list of atoms or not, if so, return the cdr (coulder), which will return everything but the first element, thus effectively removing the value, otherwise, recurisvely reduce the list until we arrive at the member
                ((eq? (car lat) a) (cdr lat))
                (else (rember a (cdr lat))))))))


; the second commandment states --> use cons to build lists

; The following shows a modified version of the rember algorithm  --> utilizing cons --> the syntax may not be entirely correct however
(define rember
    (lambda (a lat)
        (cond 
            ((null? lat) (quote ()))
            (else (cond 
            ; same logic as before, compare adn check if the current head element of the list matches the element we are attempting to remove, if so, return the current list's cdr, effectively removing the value we needed to remove (however, note that rember only removes the value we specify and keeps the rest of the lists intact, adn therefore, we will need to use the cons primitive to rebuild the list in the case that we need to remove an element in the middle or at the end of the list, we will need to remove and add back the elemenet that isn't supposed to be removed when recurively iterating through our list)
                ((eq? (car lat) a) (cdr lat))
                ; otherwise, take the first elemnet and prepend it to the beggining of the list
                ; if the values don't match, extract the first element/atom, prepend it to the list, and simultaneously reduce the list, allowing for is to iterate through the list without unneccessary losing elements
                (else (cons (car lat)
                    (rember a (cdr lat))
                ))
            ))
        )
    )
)
; A simple explanation of the implementation of the rember algorithm --> The function rember checked each atom of the lat, one at a time, to see if it was the same as the atom and. If the car was not the same as the atom, we saved it to be consed to the final value later (think of the fibbonacci sequence implementation where the recurisve function continues expanding until it is reduced down to the base case, in this case, this recurisve function will continue expanding until either of the two conditionals specified turns out to be true), when rember found the atom, it dropped it, and consed the previous atoms back onto the rest of the lat.


; we can further simplify the implemetation of the rember algorithm (as shown below):
(define rember 
    (lambda (a lat)
        (cond 
            ((null? lat) (quote ()))
            ((eq? (car lat) a) (cdr lat))
            ; we are using the cons and the car primitives to hold the elements that has been reduced but aren't part of the element to be removed as we will be adding that back once an empty list gets returned or we found the element we are looking for (in which the case the cdr list will be returned and the elements extracted using car will be added back using cons to that mutated list)
            (else (cons (car lat)
            (rember a (cdr lat))
            ))
        )
    )
)

; a more advanced form of the car primitive, is known as the first primitive, whcih is applied generally within nested lists --> it returns the first element within each of the nested lists
; For example, suppose we were asked the following question : What is (first l) where l is ((apple peach pumpkin) (plum pear cherry) (grape raisin pea) (bean carrot eggplant)) --> the resulting out will be (apple plum grape bean) --> it reduces dimensionality by 1 and returns the first element within each of the corresponding nested lists.

; In simple terms, the first primitive does the following  --> The function first takes one argument, a list, whcih is either a null list or contains only non-empty lists. It builds another list composed of first s-expressions of each internal lists.

; The thid commandment states the following --> When building a list, describe the first typical element, and then cons it onto the natural recurison.

; the fourth commandment states the following --> always change at least one argument while recurring. It must be changed to be closer to termination. The chaning argument must be tested in the termination condition: when using cdr, test termination using null?/
; when it comes to atoms, we do not consider negative numbers. --> nor do we consider floating point values as numbers --> in regards to the atom? primitive, we only consider nonnegative integers

; suppose there's a primitive named add1 --> as the name suggest, it adds 1 to the integer value provided as it's parameter --> an example would be suppose we were to write --> (add1 67) --> 68 --> same holds true for sub1

; cons builds lists and add1 builds numbers
; note the primtive tup? which handles whether a list is a tuple or not --> example code would be  --> Is (2 11 3 79 47 6) considered a tuple? --> yes, as it's a list of numbers --> a list such as (1 2 8 apple 4 3) would however not be considered a tuple, tuples require all it's elements to be numbers, strings and alphabetical values aren't allowed --> that list however would be conisdered a list of atoms --> nested numbered list wouldn't be considered a tuple either --> an example being (3 (7 4) 13 9) --> () would be considered a tuple

; knowing the tup? primitive, which acts as a boolean predicate, takes us to the primtiive addtup, which deals with adding up all the tuple values into a single number.

; the most natural way to build numbers is to use + instead of cons (cons is used to build lists)
; natural recursion of a list is (cdr lat), similarly, natural recursion of a tuple would be (cdr tup)
; natural recursion of a number is (sub1 n) where n represents a nonnegative integer

; The first commandment of number is the following --> When recurring on a list of atoms, lat, ask two questions about it : (null? lat) and else
; When recurring on a number, n, ask two questions about it : (zero? n) and else

; revised version of the 4th commandment stated earlier --> Always change at least one argument while recurring. It must be changed to be closer to termination. The chaning argument must be tested in the termination condition. The chaning argument must be tested using the following two clauses --> for lists, when using cdr, test termination using null? and when using sub1, test termination using zero?

; The 5th commandment states the following (rules for termination) --> When a building a value using +, always use 0 for the value of the termination line, for adding 9 does not change the value of an addition.
; when building a value with x, always use 1 for the value of the terminating line, for multiplying by 1 does not change the value of a multiplication
; when building a value with cons, always consider () for the value of the terminating line.

; a revised version of the very first amendment --> When recurring on a list of atoms, lat, ask two questions about it : (null? lat) and else.
; when recurring on a number, n, ask questions about it : (zero? n) and else
; when recurring on a list of S-expressions, l, ask three questions about it : (null? l), (atom? (car l)), and else.


; The 4th commandment states the following:
; Always change at least one argument while recurring. When recurring on a list of atoms, lat, use (cdr lat).  When recurring on a number, n, use (sub1 n). When recurring on a list of s-expressions l, use (car l) and (cdr l) if neither (null? l) nor (atom? (car l)) are true.

; it must be changed to closer to termination. The chaning argument must be tested in the termination condition.
; when using cdr, test termination with null? and when using sub1, test termination using zero?
; Note the following in regards to the leftmost function --> The function leftmost finds the leftmost atoms in a non-empty array list of S-expressions that does not contain an empty list.
; eqlist? --> a predicate used to determine if two lists are equal or not
; The sixth commandment --> simplify only after the function is correct
; expression related to arithmetic expressions cannot contain paranthases
; note the primitive named numbered? --> another predicate value
; also note the primtive value

; The seventh commandment states the following --> Recur on the subparts that are the same nature:
; - on the sublist of a list and - on the subexpressions of an arithmetic expression.
; the 8th commandment --> use help functions to abstract from representations

#| for the implementation, refer to chapter 8 of tls |#
; note the newly introduced primitive named set? --> as the name suggests, it is used to check if a list is a set or not
; Describe in your own words how the primtiive makeset works --> The function makeset remembers to cons the first atom in the lat onto the result of the natural recursion, after removing all occurences of the first atom from the rest of the lat --> as the purpose of a set is to remove duplicate values, at least in the context of programming.
; as followup to set?, there's subset? primitive as well --> used to check if one set is a subset of another --> Then there's also eqset? --> as the name  suggests, it's used to check if two sets are equal to one another.

; The intersect function is used to retrieve the values that interset between two sets, recall the intersection venn diagram we learned in discrete math.
; the union function is used to retrieve all the values that are contained within two sets (combined together, with duplicates removed)
; the intersectall function takes in a nested list (with the inner list being list of atoms), and outputs a value that is shared amongst all of them.




