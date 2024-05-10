#lang racket
; A list is a collection of s-expressions enclosed by parenthases
; below shows an example of an empty list, a special s-expression known as null (or empty) list, however note that an empty list is not an atom
; definition of an atom is the following --> atom is a string of characters (consisting of alphanumerical values)
()

; A list consisiting of several empty lists is a list itself --> because () lists are considered s-expressions and a list is defined as a collection of s-expressions
(() () () ())

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