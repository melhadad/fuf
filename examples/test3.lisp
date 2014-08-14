;;; -*- Mode:Lisp; Syntax:Common-Lisp; Package: FUG5 -*-
;;; -----------------------------------------------------------------------
;;; File:         test3.l
;;; Description:  Test RALT construct
;;; Author:       Michael Elhadad
;;; Created:      21 May 1990
;;; Modified:     
;;; Log file:     ChangeLog
;;; Package:      FUG5
;;; -----------------------------------------------------------------------

(in-package "FUG5")


(setf a '((x ((a 1)))))
(setf b0 '((y ((ralt TEST (1 2 3))))))
(setf b1 '((x ((a ((ralt TEST (1 2 3 4))))))))
(setf b2 '((x ((a ((ralt TEST (2 3 4 5))))))))

(setf c '((x ((a 1))) (y 2)))
(setf d0 '((x ((ralt TEST (((a 1)) ((a 2)) ((a {y}))))))))
(setf d1 '((x ((ralt TEST (((a none)) ((a given)) ((a any))))))))
(setf d2 '((x ((ralt TEsT (index on a) (((a 1)) ((a 2)) ((a 3))))))))
