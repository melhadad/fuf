;;; -*- Mode:Lisp; Syntax:Common-Lisp; Package: FUG5 -*-
;;; -----------------------------------------------------------------------
;;; File:         copypath.l
;;; Description:  Jacques's copying path instead of conflating
;;; Author:       Michael Elhadad
;;; Created:      23 Feb 1993
;;; Modified:     
;;; Package:      FUG5
;;; -----------------------------------------------------------------------

(in-package "FUG5")

;; Use as (att #{path})
;; Semantics: copy sub-fd pointed to by path at level att.
;; This is not a conflation - only what's currently at path
;; will get copied - and there won't be equality afterwards.

(set-dispatch-macro-character #\# #\{
  #'(lambda (stream char arg)
      (declare (ignore char arg))
      (vector 
	'external 
	`(lambda (path)
	   (declare (special *input*))
	   (copy-tree
	    (relocate *input* 
		      (absolute-path 
		       ,(make-path :l (normalize-path 
				       (read-delimited-list #\} stream t)))
		       path)))))))

