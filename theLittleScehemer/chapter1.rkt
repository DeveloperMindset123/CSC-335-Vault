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