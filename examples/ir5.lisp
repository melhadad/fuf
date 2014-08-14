;;; -*- Mode:Lisp; Syntax:Common-Lisp; Package: FUG5 -*-
;;; -----------------------------------------------------------------------
;;; File:         ir5.l
;;; Description:  Example inputs for GR5
;;; Author:       Michael Elhadad
;;; Created:      10 Mar 1989
;;; Modified:     18 May 90
;;; Package:      FUG5
;;; -----------------------------------------------------------------------

(in-package "FUG5")

(defun isetup5 ()
  (clear-tests)
  (format t "~%GR5: list manipulation in FUF~%")
  (format t "(test-append (ir51))~%")
  (format t "(test-append (ir52))~%")
  (format t "The gensym symbols stand for Prolog variables.~%"))

(defun ir51 ()
  (format t "ir51 --> append([a, b], [c, e], Z)~%")
  '((cat append) 
    (x ~(a b))
    (y ~(c d))))

(defun ir52 ()
  (format t "ir52 --> append([a, X, c], [d, e], [a, b | Z])~%")
  '((cat append)
    (x ((car a)
	(cdr ( ;; no car given
	      (cdr ~(c))))))
    (y ~(d e))
    (z ((car a)
	(cdr ((car b)
	      ;; no cdr given
	      ))))))

(defun test-append (fd)
  (format t "Before: ~%")
  (format t "X --> ~s~%" (fd-to-prolog fd '{x}))
  (format t "Y --> ~s~%" (fd-to-prolog fd '{y}))
  (format t "Z --> ~s~%" (fd-to-prolog fd '{z}))
  (let ((ufd (uni-fd fd)))
    (format t "After:~%")
    (format t "X --> ~s~%" (fd-to-prolog ufd '{x}))
    (format t "Y --> ~s~%" (fd-to-prolog ufd '{y}))
    (format t "Z --> ~s~%" (fd-to-prolog ufd '{z}))))


