;;; -*- Mode:Lisp; Syntax:Common-Lisp; Package: FUG5 -*-
;;; -----------------------------------------------------------------------
;;; File:         fset.l
;;; Description:  Unification of FSETs
;;; Author:       Michael Elhadad
;;; Created:      22 May 1990
;;; Modified:     15 Jun 1990 Added pair argument.
;;;               02 Jul 1990 Added path2 arg.
;;;               13 Nov 1991 changed call to *fail* for bk-class
;;;               20 Oct 92: Added level to trace-format
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
(format t "Fset unifier...~%")

;; -----------------------------------------------------------------------
;; FSET-UNIFY
;; -----------------------------------------------------------------------
;; fset: fail if some invalid att is already here.
;; So, first unify all non-fset atts with none.
;; Then, unif fset1 fset2 is intersection of fset1 and fset2.
;; If result is nil (no constituent acceptable), the parent must
;; become NONE.
;; NOTE: pair1 can be nil.
;; pair is the pair containing fd1 if known.

(defun fset-unify (fd1 fd2 pair1 pair2 path1 path2 frame fail success
		       &key (pair :unknown))
  (let ((new-fset (if pair1
		      (intersection (second pair1) (second pair2))
		    (second pair2))))
    (cond
     ((and (null new-fset) (path-null path1)) ;; at top level of total fd.
      ;; ***** NOT CLEAR IF THIS IS A LEAF FAILURE
      (*fail* fail frame path1 path2 pair))
     (t
      (fset-check-existing-atts
       new-fset fd1 fd1 fd2 path1 path2 frame fail
       #'(lambda (fd fail frame)
	   (declare (ignore fd))
	   (cond
	    (new-fset
	     (trace-format (frame-trace-flags frame) frame 0
			   "Adding FSET : ~s at level ~s" new-fset path1)
	     (if (null pair1)
		 (enrich fd1 (list 'fset new-fset) frame)
	       (update-pair pair1 new-fset (path-extend path1 'fset) frame)))
	    (t  ;; no att is acceptable: none'ify parent.
	     (trace-format (frame-trace-flags frame) frame 0
			   "EMPTY FSET at level ~s" path1)
	     (setf fd1 'none)
	     (update-pair pair 'none path1 frame)))
	   (unify fd1 (cdr fd2) path1 path2 frame fail success
		  :pair pair))
       pair)))))


;; Unify each pair in fd1 that is not in fset with none.
(defun fset-check-existing-atts (fset orig-fd1 fd1 fd2 path1 path2
				      frame fail success pair)
  (cond ((null fd1) (funcall success orig-fd1 fail frame))
	;; on these pairs, don't do anything:
	((or (leaf-p (car fd1))
	     (eq (caar fd1) 'fset)
	     (member (caar fd1) fset))
	 (fset-check-existing-atts fset orig-fd1 (cdr fd1) fd2 path1 path2
				   frame fail success pair))
	;; (car fd1) is a pair not defined in fset: none'ify it.
	(t
	 (unify fd1 `((,(caar fd1) none))
		path1 path2 frame
		#+ignore(make-failure fail
		  (trace-format (frame-trace-flags frame) frame 5
				"Attribute outside of FSET: ~s level ~s - ~
                                   FSET is ~s"
				(car fd1) path1 fset)
		  (funcall fail nil))
		fail
		#'(lambda (fd fail frame)
		    (declare (ignore fd))
		    (fset-check-existing-atts fset orig-fd1 (cdr fd1) fd2
					      path1 path2 frame
					      fail success pair))
		:pair pair))))

;; -----------------------------------------------------------------------
(provide "$fug5/fset")
;; -----------------------------------------------------------------------
