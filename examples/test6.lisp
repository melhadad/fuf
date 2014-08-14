;;; -*- Mode:Lisp; Syntax:Common-Lisp; Package: FUG5 -*-
;;; -----------------------------------------------------------------------
;;; File:         test6.l
;;; Description:  Test relative paths tricks
;;; Author:       Michael Elhadad
;;; Created:       2 Jul 1990
;;; Modified:     
;;; Package:      FUG5
;;; -----------------------------------------------------------------------

(in-package "FUG5")

;; Relative paths are ambiguous in general: this is an example of why: 

(defvar fd1 '((c ((m 1)))
	      (d ((car 1)
		  (cdr ((car 2)
			(cdr none)))))))
(defvar fd2 '((c1 {d car})
	      (c2 {d cdr car})
	      (c1 ((m {^ ^ c m})))))

;; What path is  -----^ pointing to?
;; It could be either {c m} if you read the textual place where it occurs
;; in the grammar.
;; It could also be {d c m} if you follow the path {c1}={d car}, therefore,
;; {c1 m} = {d1 car m}.
;; To resolve this ambiguity, you want the unifier to always follow the
;; textual position of a relative path in the grammar.
;; (So you want this path to point to {c m} NOT {d c m}.)
