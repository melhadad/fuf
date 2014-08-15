;;; -*- Mode:Lisp; Syntax:Common-Lisp; Package:  -*-
;;; -----------------------------------------------------------------------
;;; File:         nba.lisp
;;; Description:  Part of SURGE proper to the NBA domain
;;; Author:       Jacques Robin
;;; Created:      18 Jan 1993
;;; Modified:      5 Jul 1995 SURGE 2.2 VERSION
;;; -----------------------------------------------------------------------
;;; FUF - a functional unification-based text generation system. (Ver. 5.4)
;;;
;;; Copyright (c) 1987-2014 by Michael Elhadad. all rights reserved.
;;;
;;; Permission to use, copy, and/or distribute for any purpose and
;;; without fee is hereby granted, provided that both the above copyright
;;; notice and this permission notice appear in all copies and derived works.
;;; Fees for distribution or use of this software or derived works may only
;;; be charged with express written permission of the copyright holder.
;;; THIS SOFTWARE IS PROVIDED ``AS IS'' WITHOUT EXPRESS OR IMPLIED WARRANTY.
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
