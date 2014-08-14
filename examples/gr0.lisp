;;; --- Mode:Lisp; Syntax:Common-Lisp; ---
;;; ------------------------------------------------------------
;;; File        : GR0.L
;;; Description : Simplest grammar
;;; Author      : Michael Elhadad
;;; Created     : 20-Jun-88
;;; Modified    : 18 May 90
;;; Language    : Common Lisp
;;; Package     : FUG5
;;; ------------------------------------------------------------

(in-package "FUG5")

(defun gsetup0 ()
  (clear-bk-class)
  (reset-typed-features)
  (setq *u-grammar*
	'((alt top ( 
		    ;; a grammar always has the same form: an alternative
		    ;; with one branch for each constituent category.

		    ;; First branch of the alternative 
		    ;; Describe the category S.
		    ((cat s)
		     (prot ((cat np)))
		     (goal ((cat np)))
		     (verb ((cat vp)
			    (number {prot number})))
		     (pattern (prot verb goal)))

		    ;; Second branch: NP
		    ((cat np)
		     (n ((cat noun) (number {^ ^ number})))
		     (alt (
			   ;; Proper names don't need an article
			   ((proper yes)
			    (pattern (n)))
			   ;; Common names do
			   ((proper no)
			    (pattern (det n))
			    (det ((cat article)
				  (lex "the")))))))

		    ;; Third branch: VP
		    ((cat vp)
		     (pattern (v dots))
		     (v ((cat verb))))

		    ;; Fourth branch: ARTICLE
		    ;; doesn't do anything
		    ((cat article))
		    ((cat noun))
		    ((cat verb))))))
  (format t "~%GR0 installed.~%")
  (values))





