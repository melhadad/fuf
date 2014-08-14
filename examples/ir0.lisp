;;; --- Mode:Lisp; Syntax:Common-Lisp; Package:FUG5 ---
;;; ------------------------------------------------------------
;;; File        : IR0.L
;;; Description : Simple inputs to test GR0.
;;; Author      : Michael Elhadad
;;; Created     : 20-Jun-88
;;; Modified    : 18 May 90
;;; Language    : Common Lisp
;;; Package     : FUG5
;;; ------------------------------------------------------------

(in-package "FUG5")

(defun isetup0 ()
  (clear-tests)

  ;; Notation === stands for ((n ((lex "john"))))
  (def-test ir01
      "John likes Mary."
    ((cat s) 
     (prot ((n === "John"))) 
     (verb ((v === like))) 
     (goal ((n === "Mary")))))

  ;; Agreement error
  (def-test ir02 
      "<fail>"
    ;; when used with GR1 does not fail - a passive voice is forced to accomodate for agreement
    ((cat s)
     (prot ((n === john) (number sing)))
     (verb ((v === like) (number plural)))
     (goal ((n === mary)))))

  (def-test ir03 
      "John sells cars."
    ((cat s)
     (prot ((n === "John")))
     (verb ((v === sell)))
     (goal ((n === car) (number plural)))))

  (def-test ir04 
      "The man eats the meal."
    ((cat s)
     (prot ((n === man) (proper no)))
     (verb ((v === eat)))
     (goal ((n === meal) (proper no)))))

  (format t "~%Setup tests for gr0 - 4 tests~%")
  (values)
  )



