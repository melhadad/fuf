;;; -*- Mode:Lisp; Syntax:Common-Lisp; Package:  -*-
;;; -----------------------------------------------------------------------
;;; File:         nba.lisp
;;; Description:  Part of SURGE proper to the NBA domain
;;; Author:       Jacques Robin
;;; Created:      18 Jan 1993
;;; Modified:      5 Jul 1995 SURGE 2.2 VERSION
;;; -----------------------------------------------------------------------

(in-package "FUG5")

(def-conj score
  (cat #(under score))
  (win ((cat cardinal) (digit yes)))
  (lose ((cat cardinal) (digit yes)))
  (to ((cat phrase) (lex "-")))
  (pattern (win to lose)))

(def-conj team-name
  (cat #(under team-name))
  (pattern (home franchise))
  (alt (((synt-funct #(under head)))
	((synt-funct none))))
  (alt (((franchise given)
	 (franchise ((cat noun)
		     (number {^2 number})
		     (feature {^2 feature})))
	 ({^ determiner} any)
	 (alt (((home given) 
		(home ((cat noun))))
	       ((home none)))))
	((home given)
	 (home ((cat noun)
		    (feature {^2 feature}))))))
  )

;; ============================================================
(provide "nba")
;; ============================================================


