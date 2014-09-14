;;; -*- Mode:Lisp; Syntax:Common-Lisp; Package: FUG5 -*-
;;; -----------------------------------------------------------------------
;;; File:         test.l
;;; Description:  Testing function
;;; Author:       Michael Elhadad
;;; Created:      19 Dec 1991
;;; Modified:     Aug 3 92: test-with use uni-string instead of uni-fd.
;;;               Jul 8 14: add test3, get-test-result
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

;;; A package to test a sequence of cases and alert on mismatches
;;; Use def-test to define test cases.

(defmacro def-test (name result input)
  `(multiple-value-bind (test found) (gethash ',name *tests*)
     (declare (ignore test))
     (if found
       (format t "Redefining test ~s~%" ',name)
       (push ',name *ordered-tests*))
     (setf (gethash ',name *tests*) ',(list result input 'not-tested))))

(defun get-test (test)
  (second (gethash test *tests*)))

(defun get-test-result (test)
  (third (gethash test *tests*)))

(defun add-test-result (test fd)
  (setf (third (gethash test *tests*)) fd))

(defun clear-tests ()
  "Reset test tables"
  (setf *ordered-tests* nil)
  (clrhash *tests*))

;; --------------------------------------------------
;; TEST
;; --------------------------------------------------
(defun uni-string-limit (fd &key (limit *test-limit*))
  (uni-string fd :limit limit))

(defun test3 (&key item from to (limit *test-limit*))
  (let ((total 0)
	(correct 0)
	(problems nil))
    (mapc #'(lambda (key)
	      (let* ((val (gethash key *tests*))
		     (input (second val))
		     (result (first val)))
		(format t "~%====================~%")
		(format t "~&~s --> ~s~%"
			key (if (consp result) (car result) result))
		(multiple-value-bind (fd new)
		    (uni-fd-string input :limit limit)
		  (add-test-result key fd)
		  (incf total)
		  (cond
		    ((and (consp result) (member new result :test #'equalp))
		     (format t "~s~%OK~%" new)
		     (incf correct))
		    ((and (not (consp result)) (equalp new result))
		     (format t "OK~%")
		     (incf correct))
		    (t
		     (push key problems)
		     (cond ((consp result)
			    (format t "Expected one of:~%~{         ~s~%~}"
				    result))
			   (t (format t "Expected ~s~%" result)))
		     (format t "Instead  ~s~%" new)
		(format t "~%====================~%"))))))
	  (cond ((consp item) item)
		((null item) (extract-list from to *ordered-tests*))
		((symbolp item) (list item))))
    (format t "~%~%~s test~:P run - ~s correct.~%" total correct)
    (when problems
      (format t "The following tests are incorrect: ~s~%"
	      (nreverse problems)))
    (values problems)))



(defun test (&key (fun 'uni-string-limit) (test 'equalp) from to
		  (timed nil) item)
  (let ((total 0)
	(correct 0)
	(problems nil))
    (format t "~%====================~%")
    (mapc #'(lambda (key)
	      (let* ((val (gethash key *tests*))
		     (input (second val))
		     (result (first val))
		     (ignore (format
			      t "~&~s --> ~s~%"
			      key (if (consp result) (car result) result)))
		     (new (if timed
			    (time (funcall fun input))
			    (funcall fun input))))
		(declare (ignore ignore))
		(incf total)
		(cond
		 ((and (consp result) (member new result :test test))
		  (format t "~s~%OK~%" new)
		  (incf correct))
		 ((and (not (consp result)) (funcall test new result))
		  (format t "OK~%")
		  (incf correct))
		 (t
		  (push key problems)
		  (cond ((consp result)
			 (format t "Expected one of:~%~{         ~s~%~}"
				 result))
			(t (format t "Expected ~s~%" result)))
		  (format t "Instead  ~s~%" new))))
	      (format t "====================~%~%"))
	  (cond ((consp item) item)
		((null item) (extract-list from to *ordered-tests*))
		((symbolp item) (list item))))
    (format t "~%~%~s test~:P run - ~s correct.~%" total correct)
    (when problems
      (format t "The following tests are incorrect: ~s~%"
	      (nreverse problems)))
    (values)))


(defun extract-list (from to list)
  ;; Deal with the reversing of the list (since ordered-tests is kept in
  ;; reversed order)
  (let* ((list (reverse list))
	 (start (if from (position from list) 0))
	 (end   (if to (1+ (position to list :from-end t)))))
    (if (or (null end) (> end start))
      (subseq list start end)
      (subseq list end start))))


;; --------------------------------------------------
;; TEST-WITH
;; --------------------------------------------------
;; Test with the addition of a complement fd
;; For example: (test-with '((proc ((voice passive)))) :from t100)
(defun test-with (fd &key (from 1) (to 200) (timed nil)
		     (fun 'uni-string-limit) (item nil))
  (let ((total 0))
    (format t "~%====================~%")
    (mapc
     #'(lambda (test)
	 (let*
	     ((val (gethash test *tests*))
	      (input (second val))
	      (result (first val))
	      (new (if timed
		     (time (funcall fun (u fd (prep-input input))))
		     (funcall fun (u fd (prep-input input))))))
	   (incf total)
	   (format t "~&~s --> ~s~%" test result)
	   (format t "Without fd: ~s~%With fd   : ~s~%"
		   result new)
	   (format t "====================~%~%")))
     (cond ((consp item) item)
	   ((null item) (extract-list from to *ordered-tests*))
	   ((symbolp item) (list item))))
    (format t "~%~%~s test~:P run.~%" total)
    (values)))


;; --------------------------------------------------
;; DO-TENSES: iterate on all tenses, with negation, passive
;; --------------------------------------------------
;; @TODO:
;; - Extract tense, voice, polarity from all legal positions
;;   and override as needed.
(defun do-tenses (fd &key (from 1) (to 36)
		     (passive t)  (polarity t) (question t))
  "Take a clause fd and generate it at all tenses between from and to.
   If passive is non-nil, each tense is done active and passive.
   If polarity is non-nil, each tense is done both positive and negative."
  (when (symbolp fd)
    (multiple-value-bind (val found) (gethash fd *tests*)
      (if found
	(setf fd (second val))
	(return-from do-tenses (values)))))
  ;; Check fd is a clause
  (let ((cat (top-gdc fd {cat})))
    (if (not (eq cat 'clause))
        (return-from do-tenses (values))))
  (do* ((i from (1+ i))
	(tense (or (find-symbol (format nil "TENSE-~s" i))
		   (find-symbol (format nil "tense-~s" i)))
	       (or (find-symbol (format nil "TENSE-~s" i))
		   (find-symbol (format nil "tense-~s" i)))))
       ((> i to)
	(values))
       (format t "~%=======================~%")
       (format t "~s~%" tense)
       (let* (
              ;; Make sure there is no tense with REMOVE
              (cfd (filter-flags (u fd `((tense remove)
                                         (proc ((tense remove)))
                                         (process ((tense remove)))))))
              (tfd (cons `(tense ,tense) cfd)))
	 (uni tfd :limit *test-limit*)
	 (when passive
           (format t "~%-----------------------~%")
           (format t "Passive~%")
	   (uni (u '((proc ((voice passive)))) tfd) :limit *test-limit*))
	 (when polarity
           (format t "~%-----------------------~%")
           (format t "Negative~%")
	   (uni (cons '(polarity negative) tfd) :limit *test-limit*))
	 (when question
           (format t "~%-----------------------~%")
           (format t "Yes-no~%")
	   (uni (cons '(mood yes-no) tfd) :limit *test-limit*))
	 (when (and polarity passive)
           (format t "~%-----------------------~%")
           (format t "Yes-no Passive~%")
	   (uni (u '((proc ((voice passive))) (polarity negative)) tfd)
		:limit *test-limit*))
	 (when (and polarity question)
           (format t "~%-----------------------~%")
           (format t "Yes-no Negative~%")
	   (uni (cons '(mood yes-no)
		      (cons '(polarity negative) tfd)) :limit *test-limit*))
	 (when (and polarity passive question)
           (format t "~%-----------------------~%")
           (format t "Yes-no Passive Negative~%")
	   (uni (u '((mood yes-no) (polarity negative)
		     (proc ((voice passive)))) tfd)
		:limit *test-limit*)))))

;; --------------------------------------------------
;; DO-WH-CONSTITUENTS: iterate on all constituents
;;    in a clause, and ask a WH question on each.
;;    partic, lex-roles, pred-modif, circum.
;; --------------------------------------------------
;; @TODO: Recurse on constituents within constituents
;;        for long-distance wh.

(defun get-attrs (fd)
  (mapcar #'car fd))

(defun get-attrs-under (fd path)
  (get-attrs (top-gdc fd path)))

;; Extract a list of constituents and return them
;; as paths: {^ partic agent} {^ pred-modif location} ...
(defun get-constituents (fd)
  (let ((partic (get-attrs-under fd {partic}))
        (lex-roles (get-attrs-under fd {lex-roles}))
        (pred-modif (get-attrs-under fd {pred-modif}))
        (circum (get-attrs-under fd {circum})))
    (append
     (loop for p in partic collect (make-path :l `(^ partic ,p)))
     (loop for l in lex-roles collect (make-path :l `(^ lex-roles ,l)))
     (loop for p in pred-modif collect (make-path :l `(^ pred-modif ,p)))
     (loop for c in circum collect (make-path :l `(^ circum ,c))))))

(defun do-wh-constituents (fd)
  "Take a clause fd and generate a wh-question on each constituent."
  (when (symbolp fd)
    (multiple-value-bind (val found) (gethash fd *tests*)
      (if found
          (setf fd (second val))
          (return-from do-wh-constituents (values)))))
  ;; Check fd is a clause
  (let ((cat (top-gdc fd {cat})))
    (if (not (eq cat 'clause))
        (return-from do-wh-constituents (values))))
  (let (;; remove mood and scope
        (cfd (filter-flags (u fd `((scope remove)
                                   (mood remove)
                                   (proc ((mood remove)))
                                   (process ((mood remove))))))))
    (format t "~%=======================~%")
    (loop for c in (get-constituents cfd)
         do (let ((wfd (append `((mood wh) (scope ,c)) cfd)))
              (format t "~%-----------------------~%")
              (format t "~s~%" c)
              (uni wfd :limit *test-limit*)))))

;; ============================================================
(provide "$fug5/test")
;; ============================================================
