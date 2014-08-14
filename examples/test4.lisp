;;; -*- Mode:Lisp; Syntax:Common-Lisp; Package: FUG5 -*-
;;; -----------------------------------------------------------------------
;;; File:         test4.l
;;; Description:  Test new improved index
;;; Author:       Michael Elhadad
;;; Created:      23 May 1990
;;; Modified:     
;;; Log file:     ChangeLog
;;; Tag file:     TAGS
;;; Package:      FUG5
;;; -----------------------------------------------------------------------

(in-package "FUG5")

(define-feature-type x0 (x1 x2 x3))
(define-feature-type x1 (x11 x12 x13))
(define-feature-type x2 (x21 x22))
(define-feature-type x3 (x31 x32 x33))

(defvar a '((alt TEST (index on a) (((a x0) (b 1)) 
				    ((a x1) (b 2))
				    ((a x11) (b 3))
				    ((a x21) (b 4))
				    ((a x33) (b 5))))))
(defvar z '((ralt TEST (index on a) (((a x0) (b 1)) 
				     ((a x1) (b 2))
				     ((a x11) (b 3))
				     ((a x21) (b 4))
				     ((a x33) (b 5))))))
(defvar b0 '((a x0)))
(defvar b1 '((a x1)))
(defvar b2 '((a x11)))
(defvar b3 '((a x3)))


(defvar c '((alt TEST (index on a) (((b 1))
				  ((a none) (b 2))
				  ((a 1) (b 3))))))
(defvar d0 '((a 2)))
(defvar d1 '((a none)))
(defvar d2 '((a 1)))
(defvar d3 '((c 1)))




