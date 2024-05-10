; Lecture notes for 3/28/2024:
; Change post: strip out every occurence of a, but without other changes
; Naming convetion, following practice for rational/regular language -- * is used to signify iteration. So TLS calls this strip all a's function remeber *

;consider the following scheme code
(define (remember a lat)
  (cond ((null? lat) '())  ;using ' to intiialize an empty list, look into the notes to better understand what lat means
        ((eq? a (car lat)) (cdr lat)
                           (else (cons (car lat)
                                       (remember a (cdr lat)))))))


;take as a pre:  a is an atom and lat is a lat
; post: return the input lat unchanged except for the removal of the leftmost occurence of a
; except for that one a, the returned lat has the same multiplicies and order as the input lat
; Take a look at another scheme code
(define (rember* a lat)
  (cond ((null? lat) lat) ;if lat is null, return lat
        ((eq? a (car lat)) (rember* a (cdr lat)))
        (else (cons (car lat)
                    (rember* a (cdr lat))))))

;next thing is to go beyond list of atoms, start working on HW 7
; Practice using box and pointer diagrams, because they will show up on the exam.
; Consider what trees do in regards to recursion
; refer to notes on notebook corresponding to today's lecture to see how the tree view is drawn
