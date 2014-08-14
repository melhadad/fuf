;;; -*- Mode:Lisp; Syntax:Common-Lisp; Package:FUG5 -*-
;;; ------------------------------------------------------------
;;; File        : FDLIST.L
;;; Description : Supporting functions to handle FDs as lists and sets
;;; Author      : Michael Elhadad
;;; Created     : 05-Jul-88
;;; Modified    : 04-Aug-89
;;;               01 May 90 - moved exports to fug5
;;;               16 May 90 - path-p and leaf-p.
;;; Language    : Common Lisp
;;; Package     : FUG5
;;; ------------------------------------------------------------
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

(defun FD-to-list (FD path)
  "Converts a list represented as an FD ((cat list) (car x) (cdr y)) and
  appearing at level path within the total FD into a regular list"
  (declare (special *input*) (ignore fd))
  (fd-to-list-aux (gdp *input* path) path))

(defun FD-to-list-aux (fd path)
  (if (leaf-p FD) (return-from FD-to-list-aux nil))
  (let ((first (incr-gdp 'car fd path))
	(rest  (incr-gdp 'cdr fd path)))
    (if (eq rest 'none)	(setq rest nil))
    (if (and first (not (eq first 'none)))
	(cons first (FD-to-list-aux rest (path-extend path 'cdr))))))

(defun top-fd-to-list (FD)
  (let ((*input* fd))
    (declare (special *input*))
    (fd-to-list fd (make-path))))

(defun FD-to-prolog (FD path)
  "Converts a list represented as an FD ((cat list) (car x) (cdr y)) and
  appearing at level path within the total FD into a regular list"
  (let ((*input* FD))
    (declare (special *input*))
    (fd-to-prolog-aux (gdp *input* path) path)))

(defun FD-to-prolog-aux (fd path)
  (if (leaf-p fd) (return-from FD-to-prolog-aux (gensym)))
  (let ((first (incr-gdp 'car fd path))
	(rest  (incr-gdp 'cdr fd path)))
    (if (null first) (setq first (gensym)))
    (cond ((null rest) (cons first (gensym)))
	  ((eq rest 'none) (cons first nil))
	  (t
	   (cons first (FD-to-prolog-aux rest (path-extend path 'cdr)))))))
  


(defun list-to-FD (l)
  "Converts a regular list into its equivalent FD representation"
  (cond ((null l) 'none)
	((leaf-p l) l)
	(t `((car ,(car l))
	     (cdr ,(list-to-FD (cdr l)))))))

(set-macro-character #\~
  #'(lambda (stream char)
      (declare (ignore char))
      (let ((next (read stream t nil t)))
	(cond ((consp next) (list-to-FD next))
	      ((or (symbolp next) (numberp next))
	       (intern (format nil "~~~s" next)))
	      (t next))))
  t)

(defun print-fd (fd &optional (stream t))
  "Print an fd with tilda notation for lists."
  (cond ((leaf-p fd) (format stream "~s" fd))
	((path-p fd) (format stream "~s" fd))
	((safe-assoc 'car fd)
	 (format stream "[~{~a~%~}]" 
		 (mapcar #'(lambda (f) (print-fd f nil)) 
			 (fd-to-prolog fd nil))))
	(t (format stream "(~{(~s ~a)~%~})"
		   (mapcan #'(lambda (pair) 
			       (list (car pair) (print-fd (second pair) nil)))
			   fd)))))

;; ------------------------------------------------------------
(provide "$fug5/fdlist")
;; ------------------------------------------------------------
