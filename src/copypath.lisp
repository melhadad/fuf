;;; -*- Mode:Lisp; Syntax:Common-Lisp; Package: FUG5 -*-
;;; -----------------------------------------------------------------------
;;; File:         copypath.l
;;; Description:  Jacques's copying path instead of conflating
;;; Author:       Michael Elhadad
;;; Created:      23 Feb 1993
;;; Modified:
;;; Package:      FUG5
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
