;;; -*- Mode:Lisp; Syntax:Common-Lisp; Package: FUG5 -*-
;;; -----------------------------------------------------------------------
;;; File:         findcset.l
;;; Description:  New version of find-cset that fixes bugs
;;; Author:       Michael Elhadad
;;; Created:      31 May 1993
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


(defun find-fset (fd)
  (when (consp fd)
    (let ((pair (safe-assoc 'fset fd)))
      (if pair (second pair)))))


;;; Computation of cset with the following definition:
;;; 1/ If cset is not present, the IMPLICIT cset at level path is computed.
;;;    The implicit cset at level path is the set of paths that:
;;;    a/ either appear in the pattern
;;;    b/ or are of the form {path + att} and contain the feature (cat xxx)
;;; 2/ If cset is explicitly present, it can have 2 forms:
;;; (cset (a b c...)) or (cset ((== a1 b1...) (+ a2 b2 ...) (- a3 b3 ...)))
;;; The first form is also equivalent to (cset ((= a b c))).
;;; Each a b c can be either a single symbol or else a path (absolute or rel).
;;; The interpretation of the form (cset ((= l1) (== l) (+ l2) (- l3))) is as
;;; follows:
;;; a/ If (= l1) is present, the cset is exactly l1 (regardless of the rest).
;;; b/ If (= l1) is not present, the cset is computed as follows:
;;;    b1/ The implicit cset is first computed.
;;;        If (== l) is present, l1 is taken instead of the implicit cset.
;;;        This forms the basis cset.
;;;    b2/ The basis cset is incrementally adjusted according to + and -:
;;;        The total cset is: (set-difference (union implicit-cset l2) l3)
;;;
;;; In all cases, the resulting cset is "cleaned" according to the
;;; following rules:
;;; 1/ All "duplicates" in the sense of gdp are removed ({p} and {q} are
;;;    duplicate in the sense of gdp in (eq (gdp {p}) (gdp {q})).
;;; 2/ All "gdp-duplicates" of l3 are removed from cset.
;;; 3/ All leaf-fd constituents are removed from cset (that is all paths
;;;    such that (leaf-p (gdp {p})) are removed).

(defun find-cset (fd path &optional (cat-attribute *cat-attribute*)
		     (cset-attribute *cset-attribute*))
  "Find the set of constituents (cset) of fd occuring at level path."
  ;; Now handles csets of the form: (cset thelist impllist addlist sublist)
  ;; addlist and sublist are also applied to the implicit cset if list is
  ;; not specified.
  ;; a constituent is something :
  ;; which appears in a cset
  ;; Or if no cset is present,
  ;;    which appears in a pattern
  ;;    or whose value contains a (cat-attribute xxx)
  (when (or (leaf-p fd) (some #'atom fd))
    (return-from find-cset nil))
  (let* ((cset (new-cset-syntax (second (safe-assoc cset-attribute fd))))
	 (add-cset (cdr (safe-assoc '+ cset)))
	 (sub-cset (cdr (safe-assoc '- cset)))
	 (the-cset (cdr (safe-assoc '= cset)))
	 (impl-cset (cdr (safe-assoc '== cset)))
	 (cpath (path-extend path 'cset)))
    ;; Do not do any sort of checking whether + and - are compatible with =
    ;; at this time.
    (if the-cset
      (clean-cset the-cset nil fd path cpath)
      ;; Compute first set before cleaning: so use append instead of union
      ;; And delay the make-absolute until the end.
      (let ((basis-cset
	     (if impl-cset
	       (make-absolute impl-cset cpath)
	       (let ((from-pattern
		      (clean-pattern (safe-second (safe-assoc 'pattern fd))))
		     (from-cat (mapcan
				#'(lambda (pair)
				    (when (pair-has-cat pair cat-attribute)
				      (list (car pair))))
				fd)))
		 (append from-pattern from-cat)))))
	(clean-cset (append basis-cset add-cset) sub-cset fd path cpath)))))


;; Receive an element from a cset list and return a pair path value - where
;; value is the gdp of path efficiently computed (with assoc in fd when
;; possible) and otherwise with full gdp.
;; path is the path to fd in which cset occurs (without cset).
;; cpath is the path with cset added.
(defun pair-path-value (x fd path cpath)
  (declare (special *input*))
  (cond
   ((attr-p x)
    (let ((p (path-extend path x))
	  (val (safe-assoc x fd)))
      (cond ((null val) (list p val))
	    ((leaf-p val) (list p val))
	    ((path-p (second val)) (list p (gdp *input* (second val))))
	    (t (list p (second val))))))
   ((path-p x)
    (let ((p (absolute-path x cpath)))
      (list p (gdp *input* p))))
   (t (error "Invalid value in cset: ~s in ~s at level ~s"
	     x (safe-assoc 'cset fd) path))))



;; (Clean-cset lconst1 lsub ...): remove gdp-duplicates from lconst1 and
;; all gdp-members of lsub from lconst1.  Return a list of absolute paths.
;; Lconst1 and lsub can be lists as in patterns and cset, that is, either
;; paths or symbols.
;; Additional arguments: fd: fd in which cset appears.
;; path: level of fd.
;; cpath: (path-extend path 'cset)
;; NOTE: relies on fact that gdp returns eq values for conflated paths.
(defun clean-cset (lc1 lsub fd path cpath)
  ;; First compute an alist for each constituent, of the form
  ;; ((absolute-path gdp)...)
  (let ((alc1 (mapcar #'(lambda (c) (pair-path-value c fd path cpath)) lc1))
	(alsub (mapcar #'(lambda (c) (pair-path-value c fd path cpath)) lsub)))
    (setf alc1 (set-difference alc1 alsub :key #'second))
    (setf alc1 (remove-duplicates alc1 :key #'second))
    (mapcar #'car (remove-if #'(lambda (pair) (attr-p (second pair))) alc1))))



(defun pair-has-cat (pair cat-attribute)
  (and (not (path-p (second pair)))
       (not (member (car pair) *special-attributes*))
       (consp (second pair))
       (safe-assoc cat-attribute (second pair))))


;; ------------------------------------------------------------
(provide "$fug5/findcset")
;; ------------------------------------------------------------
