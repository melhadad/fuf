;;; -*- Mode:Lisp; Syntax:Common-Lisp; Package: FUG5 -*-
;;; -----------------------------------------------------------------------
;;; File:         external.l
;;; Description:  Support functions for external special value
;;; Author:       Michael Elhadad
;;; Created:      22 Jun 1990
;;; Modified:
;;; Package:      FUG5
;;; Macros:       externalp
;;; -----------------------------------------------------------------------
;;;
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

;; An external specification is either EXTERNAL or #(EXTERNAL fct).
;; In the short form, the fct used is *default-external-function*.
;; External works as follows for unification:
;; (u x external) stops the unification, call the external function
;; specified, and uses the value returned to continue as in (u x value).
;; External functions expect one argument: the path where the value they
;; return will be used.
;; They should return a valid grammar as value (can contain disjunctions).
;; They can return the value :fail if they want to make the unif. fail.
;; External allows the programmer to produce the constraints of a grammar
;; in a "lazy" way, specifying pieces of the grammar only when needed.

(defun external-get-function (x)
  (cond
   ((symbolp x) *default-external-function*)
   ((vectorp x)
    (cond ((equal (array-dimensions x) '(2))
	   (aref x 1))
	  (t (error "Invalid external specification: ~s" x))))
   (t (error "Invalid external specification: ~s" x))))


(defun call-external (x path frame)
  ;; Call the external function specified by the ext. x
  (declare (ignore frame))
  (let ((fct (external-get-function x)))
    (funcall fct path)))


(defun default-external-function (path)
  (format t "Enter a value for the path ~s: " path)
  (read))

(setf *default-external-function* 'default-external-function)


;; -----------------------------------------------------------------------
(provide "$fug5/external")
;; -----------------------------------------------------------------------
