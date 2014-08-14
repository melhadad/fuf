;;; -*- Mode:Lisp; Syntax:Common-Lisp; Package: FUG5 -*-
;;; -----------------------------------------------------------------------
;;; File:         control.l
;;; Description:  Unification of control and test
;;; Author:       Michael Elhadad
;;; Created:      22 May 1990
;;; Modified:     15 Jun 1990: added pair argument to unify.
;;;               02 Jul 1990: added path2 arg.
;;;               15 Aug 1991: added switch *ignore-control*
;;;               04 Sep 1991: changed macro char from @ to #@ (avoid
;;;               conflict with CLASSIC).
;;;               13 Nov 1991: changed call to *fail*
;;;               20 Oct 92: Added level to trace-format
;;;               27 Sep 93: Updated #@ to use normalize-path.
;;; Package:      FUG5
;;; -----------------------------------------------------------------------
;;;
;;; FUF - a functional unification-based text generation system. (Ver. 5.4)
;;;  
;;; Copyright (c) 1987-2011 by Michael Elhadad. all rights reserved.
;;;  
;;; Permission to use, copy, and/or distribute for any purpose and
;;; without fee is hereby granted, provided that both the above copyright
;;; notice and this permission notice appear in all copies and derived works.
;;; Fees for distribution or use of this software or derived works may only
;;; be charged with express written permission of the copyright holder.
;;; THIS SOFTWARE IS PROVIDED ``AS IS'' WITHOUT EXPRESS OR IMPLIED WARRANTY.
;;; -----------------------------------------------------------------------

(in-package "FUG5")
(format t "Control unifier...~%")

;;------------------------------------------------------------
;; CONTROL-UNIFY and TEST-UNIFY
;;------------------------------------------------------------
;; Handles the CONTROL and TEST special keywords.
;; These are not "pure" unification features: they are meant to
;; be used in ALT constructs.
;; syntax: (CONTROL <sexpr>)
;;         (TEST <sexpr>)
;; Example: (alt (((test (member #@{^ p u} '(a b c))) ...)))
;; In the <sexpr> the special char #@ can be used to access
;; values of the fd being unified.
;; If the evaluation of <sexpr> is non-nil, unification proceeds
;; otw it fails.
;; With CONTROL <sexpr> is evaluated at UNIFICATION-time
;; with TEST    <sexpr> is evaluated at DETERMINATION-time 
;; therefore, TEST provides still an "order independent" unification.
;; Example:
;; ((a v)) // ((alt (((CONTROL (eq #@{a} #@{b})) (c w))
;;                   ...))
;;             (b v))
;; With CONTROL this unification fails, with TEST it succeeds.


(set-dispatch-macro-character #\# #\@
  #'(lambda (stream char arg)
      (declare (ignore char arg))
      `(gdp *input* (absolute-path 
		     ',(read stream t nil t)
		     path))))


(defun filter-macro-char (l)
  "Put back \@ in CONTROL and TEST statements for pretty-printing"
  (cond ((null l) l)
	((leaf-p l) l)
	((path-p l) l)
	((and (consp l) (eql (car l) 'gdp) (eql (second l) '*input*)
	      (eql (car (third l)) 'absolute-path) 
	      (eql (third (third l)) 'path))
	 (cons '\@ (cdr (second (third l)))))
	(t (cons (filter-macro-char (car l))
		 (filter-macro-char (cdr l))))))

(defun control-unify (fd1 fd2 path1 path2 frame fail success 
			  &key (pair :unknown))
  (if (or *ignore-control*
	  (eval `(let ((path ',(path-extend path2 'control)) ;; PATH2!!!!
		       (%frame% ',frame))
		   ,(second (car fd2)))))
      (progn
       (trace-format (frame-trace-flags frame) frame 0
		     "CONTROL succeeds: ~s at level ~s" 
		     (filter-macro-char (car fd2)) path1)
       (unify fd1 (cdr fd2) path1 path2 frame fail success :pair pair))
      (*fail* fail frame path1 path2 pair "Fail in testing ~s at level ~s" 
	      (filter-macro-char (car fd2)) path2)))


(defun test-unify (fd1 fd2 path1 path2 frame fail success 
		       &key (pair :unknown))
  "Just push the test in a global var that will be tested by Determine"
  (push (make-test :test (second (car fd2)) 
		   :path (path-extend path2 'test))  ;; PATH2!!
	(frame-tests frame))
  (unify fd1 (cdr fd2) path1 path2 frame fail success :pair pair))


;; -----------------------------------------------------------------------
(provide "$fug5/control")
;; -----------------------------------------------------------------------


