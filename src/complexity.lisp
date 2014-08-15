;;; --- Mode:Lisp; Syntax:Common-Lisp; Package:FUG5 ---
;;; -----------------------------------------------------------------------
;;; File:         COMPLEXITY.L
;;; Description:  Computes complexity of FUgrammar
;;; Author:       Michael Elhadad
;;; Created:      11-Aug-88
;;; Modified:     30 Apr 90 - moved exports to fug5
;;;               20 Jun 90 - allowed for equations in grammar.
;;; Package:      FUG5
;;; Status:       Experimental
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
(format t "Complexity...~%")

(defun complexity (&optional (fd *u-grammar*) (with-index t))
  "Computes complexity of a grammar, that is, how many
       branches would be in the disjunctive normal form of fd.
       If with-index is T (default), the indexing declarations
       are taken into account, and an indexed alt is considered
       as a deterministic construct (assumes indexing always
       works). "
  (declare (special with-index))
  ;; top level alt is always indexed on cat, but this is not
  ;; declared in the grammar. So special handling here.
  (let ((branches (branches (car fd))))
    (if with-index
	(apply #'max (mapcar #'complexity1 branches))
	(apply #'+ (mapcar #'complexity1 branches)))))


(defun complexity1 (fd)
  (declare (special with-index))
  (cond ((leaf-p fd) 1)
	((path-p fd) 1)
	((eq (car fd) 'alt)
	 (multiple-value-bind (branches traced indexed) (branches fd)
	   (declare (ignore traced))
	   (if (and indexed with-index)
	       (apply #'max (mapcar #'complexity1 branches))
               (apply #'+ (mapcar #'complexity1 branches)))))
	((eq (car fd) 'opt)
	 (1+ (complexity1 (branches fd))))
	((leaf-p (car fd)) 1)
	((path-p (car fd)) 1)
	(t (apply #'* (mapcar #'complexity1 fd)))))


(defun avg-complexity (&optional (fd *u-grammar*) (with-index t) rough-avg)
  "Computes avg. complexity of a grammar, that is, how many
       branches on the average would be tried if the input were
       empty.
       If with-index is T (default), the indexing declarations
       are taken into account, and an indexed alt is considered
       as a deterministic construct (assumes indexing always
       works).
       If rough-avg is nil (default), the avg. for an alt.
       construct is the sum of the complexity of the first half
       branches (when not indexed). Otw, the avg. is half the sum
       of the complexity of all branches (less accurate)."
  (declare (special with-index rough-avg))
  ;; top level alt is always indexed on cat, but this is not
  ;; declared in the grammar. So special handling here.
  (let ((branches (branches (car fd))))
    (values
      (round
	(if with-index
	    (apply #'avg (mapcar #'avg-complexity1 branches))
	  (if rough-avg
	      (/ (apply #'+ (mapcar #'avg-complexity1 branches)) 2)
	    (apply #'+
		   (mapcar #'avg-complexity1
			   (butlast branches
				    (round (/ (length branches) 2)))))))))))


(defun avg (&rest numbers)
  (if (null numbers)
      0
      (/ (apply #'+ numbers) (length numbers))))


(defun avg-complexity1 (fd)
  (declare (special with-index rough-avg))
  (cond ((leaf-p fd) 1)
	((path-p fd) 1)
	((eq (car fd) 'alt)
	 (multiple-value-bind (branches traced indexed) (branches fd)
	   (declare (ignore traced))
	   (if (and indexed with-index)
	       (apply #'avg (mapcar #'avg-complexity1 branches))
	     (if rough-avg
		 (/ (apply #'+ (mapcar #'avg-complexity1 branches)) 2)
               (apply #'+
		      (mapcar #'avg-complexity1
			      (butlast branches
				       (round (/ (length branches) 2)))))))))
	((eq (car fd) 'opt)
	 (1+ (complexity1 (branches fd))))
	((leaf-p (car fd)) 1)
	((path-p (car fd)) 1)
	(t (apply #'* (mapcar #'complexity1 fd)))))


;; -----------------------------------------------------------------------
(provide "$fug5/complexity")
;; -----------------------------------------------------------------------
