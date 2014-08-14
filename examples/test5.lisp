;;; -*- Mode:Lisp; Syntax:Common-Lisp; Package: FUG5 -*-
;;; -----------------------------------------------------------------------
;;; File:         test5.l
;;; Description:  Test paths as attributes (equations)
;;; Author:       Michael Elhadad
;;; Created:       4 Jun 1990
;;; Modified:      5 Jun 1990
;;; Package:      FUG5
;;; -----------------------------------------------------------------------

(in-package "FUG5")

(defvar a '(%TEST% (a ((b (({} ((x 1)))
			   ({^ z} {x})
			   ({^ c} ((y 2)))
			   (d 3)))))))
(defvar b0 '((a ((c 1)))))
(defvar b1 '((x 2)))
(defvar b2 '((a ((c ((z 3)))))))
(defvar b3 '((a ((b ((c 1)))))))


(defvar c '((alt TEST 
	     (((a 1)
	       ({x} 2))
	      ((a 2)
	       ({x} 1))))))
(defvar d0 '((x 1)))
