;;; -*- Mode:Lisp; Syntax:Common-Lisp; Package: FUG5 -*-
;;; -----------------------------------------------------------------------
;;; File:         test-wait.l
;;; Description:  Test search strategy used for fuf
;;; Author:       Michael Elhadad
;;; Created:       5 Sep 1995
;;; Modified:     
;;; Package:      FUG5
;;; -----------------------------------------------------------------------

(in-package "FUG5")

(def-grammar gr ()
  (setq *wait-grammar* 
	'((alt root
	   (((cat root)
	     (cset ((== constituent-1)))
	     (constituent-1 ((cat c1)))
	     (alt c1 (:wait {^ constituent-1 constituent-12 some-slot})
		  (((cset ((+ constituent-2)))
		    (constituent-2 ((cat c2)))))))
	    ((cat c1)
	     (cset ((== constituent-13 constituent-11)))
	     (constituent-11 ((cat c11)))
	     ;; (constituent-13 ((cat c13)))
	     (alt c11 (:wait {^ constituent-11 some-slot})
		  (((cset ((+ constituent-12)))
		    (constituent-12 ((cat c12)))))))
	    ((cat c2)
	     (status done))
	    ((cat c11)
	     (some-slot done))
	    ((cat c12)
	     (some-slot done))
	    ((cat c13)
	     (status done)))))))


(setf tw1 '((cat root)))

