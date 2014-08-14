;;; -*- Mode:Lisp; Syntax:Common-Lisp; Package: FUG5 -*-
;;; -----------------------------------------------------------------------
;;; File:         test8.l
;;; Description:  Test for freeze and ignore
;;; Author:       Michael Elhadad
;;; Created:      28 Jul 1991
;;; Modified:     
;;; Log file:     ChangeLog
;;; Package:      FUG5
;;; -----------------------------------------------------------------------

(in-package "FUG5")


(setq *agenda-policy* :force)

(defvar t10 '((a 0)))
(defvar t11 '((a 1)))

;; Nothing special
(defvar t12 '((alt (:wait a) (((a 0) (b 1))
			      ((a 1) (b 2))
			      ((a 2) (b 3))))))

;; undo a freeze
(defvar t13 '((b 2)
	      (alt C (:wait c) (((c 1)) ((c 2))))
	      (alt A (((a 2)) ((a 1))))))

;; force simple: with  t11
(defvar t14 '((alt 1 (:wait b) (((b 1)) ((b 2))))
	      (alt 2 (((a 0) (b 0))
		      ((a 1) (b 1))))))



;; Undo a thaw: replace frozen on agenda...
;; do t14 with t10


;; Thaw simple: with t11
(defvar t15 '((alt 1 (:wait b) (((b 1)) ((b 2))))
	      (alt 2 (((a 0) (b 0))
		      ((a 1) (b 1))))
	      (alt 3 (((a 0) (c 0))
		      ((a 1) (c 1))))))



