; nested mappings
; given a postive integer n, find all ordered pairs of distinct pstistive integres i and j where 1 <= i < j <= n

(define (ordered-pairs-of-distinct-integers n)
  (accumulate append
              '()
              (map (lambda (i) (map (lambda (j) (list i j))
                                    (enumerate-interval (+ i 1) n)))
                   (enumerate interval 1 (-n 1)))))

;reference to the above function : relatize that once again, there is no top-level recursion or looping. So our interest is directed at the data flow, as for the previous example.
; flatmap allows a simplficiation
(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

; ordered pairs of distinct integers using flatmap
(define (ordered-pairs-of-distinct-integers n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enumerate-interval (+ i 1) n)))
           (enumerate-interval 1 (-n 1))))

; flatmap turns out to be quite useful - next we use ti compute all permutaition of a set S (code provided below) --> all permutations of a Set S
; for example, the permutations of {1,2,3} are given --
; first, list all permutations iwht 1 in the first position
; next, list all permutations with 2 in the first position
; finally, list all permutations with 3 in the first psoition

; context behind the map funciton: map is a built in Scheme function that takes a function and a list as an argument and returns the list that results by applying that function to every element of the list (similar to how javascript map function works)
; flatmap states that instead of applying it to seek, apply it to whatever is being mapped on seek

(define (permutations s)
  (if (null? s)
      (list '())
      (flatmap
       (lambda (x)
         (map (lambda (p) (cons x p))
              (permutations (remove x s))))
       s)))

; between this and the homework set, we need to have a good understanding of the idioms involved.
; when you use flatmap, it still returns a list, simply mapped lower (a level lower for flaptmap) --> think of this as flattening involved in machine learning


; Undertanding Idiom (general definition) --> referecnce: hw 8, 9 and 10 (which we will be going over during zoom hours rather than in class)
; Programming idioms or code idiom is a group if cide fragments sharing an equivalent semantic role, which recurs frequently across software projects often expressing a special featuer of a recurring construct in one or more programming languages or libraries.

; List of idioms relevant to funcitonal programming:
#|
- map, reduce, filter, and flatten --> these commands replaces the for and if commnads
- monads : these are containers with rules that regulate how they work and interact with other monads
- Algebraic Data Types: These are meaningful, composite types
|#

#|
Consider the following question from the homework:
(Die to friendman adnd Felleisen, the authors of TLS)
- An L-expression is an S-expressiion which is either:
(AND l1 l2), or
(OR l1 l2), or
(NOT l1), or
an arbitary symbol, which we call a variable

- Here l1 and l2 are arbitary L-expressions, so this is an inductive definition once we add 'and nothing else is an L-expression'
(a) Write and certify a function lexp? Which checks whether an S-expression is an L-expression
(b) Write and ceritfy a function covered? of an L-expression lexp and a list of symbols los that rests whether all the variables in lexp are in los.
(c) For the evaliation of l-expressoo we need asosociation lists, or alists. A alist for L-expressions is a list of (variable, value) pairs. The variable component is always a symbol, and the value component is either the number 0 (for false) or 1 (for true). Write and certify a function
            user layer
           ----------------------------------------------------------------------------------------------
            ^ - l1 and l2 are representations of |  first-operand                   |   AND? OR? NOT?   |
            |   l-expressions (LEXPS for short)  |  second-operand                  |                   |
            | (AND l1 l2), or                    |  operator (will be the car)      |                   |
            | (OR l1 l2), or                     |                                  |                   |
            | (NOT l1), or                       |----------------------------------|-------------------|
            |                                    |  selector                        |   classifiers     |
data type   |                                    |                                  |                   |
            |                                    |                                  |                   |
           ----------------------------------------------------------------------------------------------
       <bottomarrow>
- the user layer above represents a concrete represenation of LEXPs prefix lots
- Simetimes R(l) is used to indicate a particular represenatin of f l.
; atoms are neither an empty list nor a pair
|#

;before we check if it's an atom or not,w e need to make sure to check if it's null or not
; last conditional is checking if length is 
(define (lexp? l)
  (cond ((null? l) #f)
        ((atom? l) (symbol? l))
        ((> (length l) 3) #f)
        ((= (length l) 3)
         (or (and (eq? (car l) AND)
                  (lexp? (fistoperand l))
                  (lexp? (secondoperand l)))
             (and (eq? (car l) 'OR))))))


; the syntax checker on the project is based on Scheme, rather than the l-expression
; Suppose you have some arbitary l-expression, code provided below

; Here, you would need to know the truth values of the variables --> to evaliate something like this
(AND (OR X Y) (NOT Z))

; to do this, we feed the interpreer an argument in addition to the lexp itself --> namely, an a-list specifying the variable's values, and it may look like the following below
((X #T) (Y #F) (Z #T))

; part b (from the above question), walks through the l-expressions and determines the list of sumbols (a list of all the symbols that occur here)

; stack data structure is the fundamental of how variables are defined in scheme (providing automatic locality)
(define (lookup var alist)
  (cond ((eq? var (caar alist)) (cadar a list))
        (else (lookup var (cdr alist)))))

; this returns the value form the first (leftmost) binding of var.
; note that (AND X Y) means anything alist is ((x 0) (y 1)), and means something if alist is
; note: and is not actually a function in R5RS even though it is a primitive
; [ it is instead a macro ]
; You might want to look at using dotted argument lists as a way fo writting your own functions whihc (like y) are happy to take an arbitary number of arguments.

; you might ask how to write a really nice way to write all possible associations, you would need to define logical and and OR in terms of mathemtics (suppose 0 is true, 1 is false, then AND will need to be multiplciation where the only time it's true is 1 * 1)

; There is no such thing as a base 1 number system (although according to google, it is known as unary number system)
; we will continue looking at Scheme in Scheme, we cannot have scheme programs assuming all variables globally, tommorow night offic ehours as well










