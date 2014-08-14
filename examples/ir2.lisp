;;; --- Mode:Lisp; Syntax:Common-Lisp; Package:FUG5 ---
;;; ------------------------------------------------------------
;;; File        : IR2.L
;;; Description : Simple inputs to test GR2.
;;; Author      : Michael Elhadad
;;; Created     : 20-Jun-88
;;; Modified    : 18 May 90
;;; Language    : Common Lisp
;;; Package     : FUG5
;;; ------------------------------------------------------------

(in-package "FUG5")



(defun isetup2 ()
  (format t "The examples in IR0 and IR1 won't work with GR2~%")
  (clear-tests)
  (def-test ir21
    "Mice are eaten by thieves."
  ((cat s)
   (focus {goal})
   (prot ((number plural) (nnp ((n === thief)))))
   (goal ((number plural) (nnp ((n === mouse)))))
   (verb ((v === eat)))))
  
  (values))



