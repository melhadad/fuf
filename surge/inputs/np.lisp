;;; -*- Mode:Lisp; Syntax:Common-Lisp; Package: FUG5 -*-
;;; -----------------------------------------------------------------------
;;; File:         np.lisp
;;; Description:  Examples of nps
;;; Author:       Michael Elhadad
;;; Created:      28 Jul 1997
;;; Modified:
;;; Package:      FUG5
;;; -----------------------------------------------------------------------

(in-package "FUG5")

(def-test np1
  "A Montogovian view of NP meaning."
  ((cat np)
   (head ((lex "view")))
   (definite no)
   (describer ((lex "Montogovian")))
   (qualifier ((cat pp)
	       (np ((lex "meaning")
		    (countable no)
		    (classifier ((lex "NP")))))))))

(def-test np2
  ;; Such: not covered in syntax
  ;; ones: faked as a common noun instead of pronoun
  "[Such] examples just like the unary ones."
  ((cat np)
   (head ((lex example)))
   (number plural)
   (definite no)
   ;; (pre-det ((lex "such")))
   (qualifier ((cat pp)
	       (prep ((lex "like") (adverb ((lex "just")))))
	       (np ((definite yes)
		    (head ((lex one)))
		    (number plural)
		    (describer ((lex "unary")))))))))
