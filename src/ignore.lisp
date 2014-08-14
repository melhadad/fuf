;;; -*- Mode:Lisp; Syntax:Common-Lisp; Package: FUG5 -*-
;;; -----------------------------------------------------------------------
;;; File:         ignore.l
;;; Description:  Deal with the ignore annotations in ALT constructs
;;; Author:       Michael Elhadad
;;; Created:      28 Jul 1991
;;; Modified:     
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

;; Ignore annotations are of 2 sorts: ignore-unless and ignore-when.
;; Syntax is: (alt (:ignore-when <fd>) <branches>)
;; The meaning for :ignore-when is:
;; If (u *input* <fd>) is not :fail, the whole alt is ignored (it is as if
;; the grammar did not contain it).
;; If (u *input* <fd>) is :fail, the alt is processed normally. 
;; Meaning is reversed for :ignore-unless.
;; NOTES: 
;; 1. the call to u has NO side-effects in the current unification.
;;    The test is performed in a different environment (EXPENSIVE: copy of
;;    *input* is performed PLUS a whole gdp path1).
;; 2. <fd> is assumed to be positioned at the same level as the alt pair
;;    (path2). Equations can be used to test higher level features in the
;;    total fd as in ({} ((a 1) (b 2))).
;; 3. <fd> should not contain any's or test's (no determintation stage in
;;    the check).


;; Check functions return T if we want to ignore.

;; CAREFUL: these functions are short but tricky...

(defun check-ignore-unless (fd path1 path2)
  (declare (special *input*))
  (when fd
    ;; Protect the environment with a new copy of input of which fd1 must
    ;; be a subtree...
    (let ((*input* (copy-tree *input*)))
      (declare (special *input*))
      (let* ((fd1 (gdp *input* path1))
	     (res (unify fd1 fd path1 path2 (make-frame)
			#'(lambda (msg) (declare (ignore msg)) *fail*)
			#'(lambda (fd fail frame) 
			    (declare (ignore fd fail frame)) t))))
	(if (eq res :fail) t nil)))))


(defun check-ignore-when (fd path1 path2)
  (declare (special *input*))
  (when fd
    (let ((*input* (copy-tree *input*)))
      (declare (special *input*))
      (let* ((fd1 (gdp *input* path1))
	     (res (unify fd1 fd path1 path2 (make-frame)
			#'(lambda (msg) (declare (ignore msg)) *fail*)
			#'(lambda (fd fail frame) 
			    (declare (ignore fd fail frame)) t))))
	(if (eq res :fail) nil t)))))



(provide "$fug5/ignore")