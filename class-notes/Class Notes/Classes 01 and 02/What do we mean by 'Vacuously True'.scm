
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; On the terms 'vacuous' and 'vacuously true'

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; can you explain why (+) has the value 0?

;;; well, there are no arguments, and the sum of zero numbers is 0

;; similarly, can you explain why (and) has the value #t?

;;; well -- given (and arg1 arg2 ... argk), and returns true if every one of
;;; arg1, arg2, ... , argk is true.  For (and) -- ie, and applied to no arguments -- the set {arg1, arg2, ..., argk}
;;; is empty -- it follows then that every one of these arguments is true.

;;; one says then that (and) is vacuously true

;;; we will encounter this over and over again in the coming weeks -- you want
;;; to review universal quantifiers:  "for every arg in the empty set {}, (f arg) is
;;; true whenever (f arg) computes a Boolean value".  Intuitively,
;;; ask yourself "how could it be false?"  Well, there would
;;; need to be an argument in the empty set for which the value(f arg) is false.  But of course
;;; there are no arguments in the empty set.

;;; I like to refer to this as the 'green elephant argument'.  The corresponding
;;; claim is this: "every green elephant in my office just now is wearing purple
;;; boots."  This is a true statement, for the simple fact that there are no
;;; green elephants in my office at this time -- so -- vacuously -- every one of them is
;;; wearing purple boots!

;;; Another use of the phrase "vacuously true" arises when talking about
;;; propositions -- recall the definition of P ==> Q

;;;           P           Q             P ==> Q
;;;          ---         ---           ---------
;;;           T           T               T
;;;           T           F               F
;;;           F           T               T
;;;           F           F               T

;;;  The last two lines are described by saying that when the antecedent P
;;;  is false, then the implication P ==> Q is vacuously true.

;;; What about (or)?  (or arg1 arg2 ... argk) is #t exactly when at least
;;; one of arg1, arg2, ..., argk is true.  So -- if none are true, then
;;; the or evaluates to false.

;;; So the relevant question is: how many args in {} are true?
;;; Clearly, the answer is 0.  So (or) _must_ evaluate to false.

