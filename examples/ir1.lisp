;;; -*- Mode:Lisp; Syntax:Common-Lisp; Package:FUG5 -*-
;;; ------------------------------------------------------------
;;; File        : IR1.L
;;; Description : Simple inputs to test GR1.
;;; Author      : Michael Elhadad
;;; Created     : 20-Jun-88
;;; Modified    : 18 May 90
;;; Language    : Common Lisp
;;; Package     : FUG5
;;; ------------------------------------------------------------

(in-package "FUG5")

(defun isetup1 ()
  (clear-tests)
  ;; Add an irregular verb to lexicon
  (store-verbs '( ("sell" "sells" "sold" "selling" "sold")) )

  (def-test ir11
    "Mary is liked by john."
    ((cat s) 
     (voice passive)
     (prot ((n === john))) 
     (verb ((v === like))) 
     (goal ((n === mary)))))

  (def-test ir12
    "<fail>"
    ((cat s)
     (prot ((n === john) (number sing)))
     (verb ((v === like) (number plural)))
     (goal ((n === mary) (number sing)))))

  (def-test ir13
    "Cars are sold by John."
    ((cat s)
     (voice passive)
     (prot ((n === "John")))
     (verb ((v === sell)))
     (goal ((n === car) (number plural)))))

  (def-test ir14
      "The meal is eaten by the man."
    ((cat s)
     (voice passive)
     (prot ((n === man) (proper no)))
     (verb ((v === eat)))
     (goal ((n === meal) (proper no)))))

  (format t "~%Setup tests for gr1 - 4 tests~%")
  (values)
)





