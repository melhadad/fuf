;;; -*- Mode:Lisp; Syntax:Common-Lisp; Package:FUG5 -*-
;;; -----------------------------------------------------------------------
;;; File:         BACKTRACK.L
;;; Description:  Handling of backtraking with UNDO and LIMIT on BPs
;;; Author:       Michael Elhadad
;;; Created:      02-Nov-89
;;; Modified:     17 May 90: moved macros to macros.l
;;;               20 Feb 91: use equality instead of equalp
;;;               22 Feb 91: allow for lists of bk-classes instead of one
;;;                          put a special trace flag for bk-class
;;;               01 Mar 91: allow for lists of classes in define-bk-class
;;;                          also.  Changed match-bk-class.
;;;               28 Jul 91: undo agenda operations for WAIT.
;;;               19 Sep 91: fixed member-or-inter to work for any comb.
;;;               13 Nov 91: changed match-bk-class to have 2 args path1/2
;;;               15 Nov 91: added get-bk-class to map through the path
;;;               10 Dec 91: undo constituent-agenda operations (ac).
;;;               24 Dec 91: removed constituent-agenda ops.
;;;               10 Jul 92: renamed undo to undo-list to avoid conflict in MCL
;;;               20 Oct 92: Added level to trace-format
;;;               11 Jan 96: Commented out code in update-pair (Charles
;;;                          Brendan notified the problem)
;;; Package:      FUG5
;;; Macros:       backtrack, *fail*, update-pair, enrich
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
(format t "Backtracker...~%")

; -------------
; Handling of backtraking :
; we use a continuation (fail) which contains at any point the state
; of the computation before we choose one branch of an alternative.
; When we have a problem, we backtrack by simply calling fail.
; To have things a little smoother, we also throw away the stack of
; recursion which led us to a deadend by using a (throw ...).
; Of course, we need to mark the stack each time we make a choice
; which can be revised (that is, choose one branch of an alternative).
; We do this with (catch ...).
; Each time we do a physical modification, we list it in the UNDO list.
; Upon backtracking, we undo all current UNDOs.
; The following macros implement all of this machinery.
; -------------

;; What happens when more than limit bps have been used:
;; throw signal-stop to the top tag.
(defun signal-stop (counter fd)
  "Let the user know that the unification has been stop in the middle"
  (declare (ignore fd))
  (format t ";;; Unification stopped after ~s backtracking points.~%" counter))


(defun define-bk-class (path-spec class)
  "Add a new class definition for certain paths meeting path-specification.
   For the moment, path-spec identifies a path by the last atom of the
   path. That is, {a b c} matches the path-spec c."
  (setf (gethash path-spec *bk-classes*) class))

(defun clear-bk-class (&optional path-spec)
  (if path-spec
      (remhash path-spec *bk-classes*)
    (clrhash *bk-classes*)))


(defun member-or-inter (elt class)
  "Check if elt and class intersect.
   Works for any combination of consp and symbolp."
  (cond
   ((symbolp elt)
    (cond ((symbolp class) (eq elt class))
	  ((consp class) (member elt class))
	  (t (error "Bad bk-class specification: ~s~%" class))))
   ((consp elt)
    (cond ((symbolp class) (member class elt))
	  ((consp class) (intersection elt class))
	  (t (error "Bad bk-class specification: ~s~%" elt))))))

(defun adjoin-or-union (elt class)
  "Put together elt and class in a flat list with no repetitions.
   Either elt or class can be symbols or lists."
  (cond
   ((null elt) class)
   ((null class) elt)
   ((symbolp elt)
    (cond ((symbolp class) (if (eq elt class) elt (list elt class)))
	  ((consp class) (adjoin elt class))
	  (t (error "Bad bk-class specification: ~s~%" class))))
   ((consp elt)
    (cond ((symbolp class) (adjoin class elt))
	  ((consp class) (union elt class))
	  (t (error "Bad bk-class specification: ~s~%" elt))))))


(defun input-pair (pair)
  (cond ((consp pair) (and (>= (length pair) 3)
			   (eq (third pair) :i)))
	(t t)))


(defun get-bk-class (path)
  ;; Get the list of classes of which path is a member
  ;; Stop at the first element from the end that matches a bk-class
  ;; Return 2 values: class found
  (labels
      ((get-iter (rest)
         (if (null rest)
	   (values nil nil)
	   (multiple-value-bind (class found) (gethash (car rest) *bk-classes*)
	     (if found
	       (values class found)
	       (get-iter (cdr rest)))))))
    (get-iter (reverse (path-l path)))))


;; <<MAKE SURE PATH1 or PATH2 DOES NOT INTERFERE>>
(defun match-bk-class (path1 path2 pair class frame)
  "Test if Path is declared a special long distance failure and
   if the class of the current bp cannot handle it"
  (declare (special *changes-made* *failure-address*
		    *is-bk-failure* *class-of-failure*))

  ;; Determine address of failure (AF)
  ;; Need to switch the AF only in the following conditions
  (when (and (or  (null *is-bk-failure*)
		  *changes-made*
		  (input-pair pair))
	     (not (eq path1 *same*)))
    (let ((switched nil))
      (setf *changes-made* nil)
      ;; Can it be that path1 is a new special path?
      (unless (path-equal path1 *failure-address*)
	(if *is-bk-failure*
	  (trace-format
	   *trace-bk-class* frame 15
	   "BK: Switch from ~s to ~s" *failure-address* path1))
	(setf *failure-address* path1)
	(setf switched t)
	(multiple-value-setq (*class-of-failure* *is-bk-failure*)
	    (get-bk-class path1)))
      ;; Can it be that path2 is a different special path?
      (unless (or (path-equal path1 path2)
		  (and (not switched)
		       (path-equal path2 *failure-address*)))
	(multiple-value-bind (class2 flag2)
	    (get-bk-class path2)
	  (when flag2
	    (trace-format
	     *trace-bk-class* frame 15
	     "BK2: Switch from ~s to ~s" *failure-address* path2))
	  (setf *failure-address* path2)
	  (setf *is-bk-failure* flag2)
	  (setf *class-of-failure*
		(if switched
		  (adjoin-or-union *class-of-failure* class2)
		  class2))))))

  ;; Determine whether current bpoint can handle the address of failure
  (if *is-bk-failure*
    ;; match if the class can handle the special path:
    (let ((match (member-or-inter class *class-of-failure*)))
      (if (and match (> *bk-frames-skipped* 0))
	(progn
	  (trace-format
	   *trace-bk-class* frame 15
	   "BK: Special path ~s caught by alt ~s class ~s after ~s frame~:P~%"
	   *failure-address* (frame-name frame) class *bk-frames-skipped*)
	  (setf *bk-frames-skipped* 0))
	(incf *bk-frames-skipped*))
      match)
    ;; Otw, it's always a match if path is not special
    t))


;; -----------------------------------------------------------------------
;; UNDO Management
;; -----------------------------------------------------------------------


;; --------------------
;; Modifs log an undo history in the current frame.
;; --------------------

(defun update-pair (pair value path frame)
  "Update the value of pair with value. Path must be such that
	  pair = (gdpp *input* path).
	  Adds a record to the undo-list of frame.
          If pair = :unknown, recompute the gdpp."
  (declare (special *input* *changes-made* *is-bk-failure*))
  (if (eq pair :unknown) (setf pair (gdpp *input* path frame)))
  (when (and (consp pair)
	     (not (equality (second pair) value)))
    (setf *changes-made* t)     ;; Note that fd has changed
    (when *is-bk-failure*
      (trace-format *trace-bk-class* frame 5 "BK: Change at level ~s" path))
    (nconc (frame-undo frame) `((u ,(second pair) ,(third pair) ,path)))
    (cond
     ;; This thing shouldn't happen
     ;; ((path-null path)
     ;; (setf *input* value))
     (t
      (if (= (length pair) 2)
	  (nconc pair (list :e))
	(setf (third pair) :e))
      (setf (second pair) value)))))

(defun enrich (fd pair frame)
  (declare (special *changes-made* *is-bk-failure*))
  (setf *changes-made* t)     ;; Note that fd has changed
  (if *is-bk-failure*
    (trace-format *trace-bk-class* frame 0 "BK: Enrich with ~s" pair))
  (nconc (frame-undo frame) (list (cons 'r (last fd))))
  (let ((len (length pair)))
    (nconc fd (list (cond ((= len 2) (nconc pair (list :e)))
			  ((= len 3) (setf (third pair) :e) pair)
			  ;; That's for special types, maybe
			  (t pair))))))


;; Add an entry to the agenda: defined in wait.l
;; Remove an entry from the agenda: defined in wait.l
;; Agenda entries are identified by a unique id to make sure the right ones
;; are added and removed.
;; The code for add-agenda is (aa id)
;; The code for remove-agenda is (ar id)

(defun undo-list (fd undo-list)
  "Actually undo the modifications made on fd at level path"
  (declare (special *wrong-branches*) (ignore fd))
  (incf *wrong-branches*)
  (mapc #'undo-one (nreverse (cdr undo-list)))) ; remove root of the beginning

(defun undo-one (undo-what)
  "Undo one modification on an fd. Undo-what is a record of undo added
       by either enrich or update."
  (let ((type (car undo-what))
	(data (cdr undo-what)))
    (declare (special *number-of-undo* *input*))
    (incf *number-of-undo*)
    (cond ((eq type 'r) ; undo an enrich - skip next elt
	   (setf (cdr data) (cddr data)))

	  ((eq type 'u) ; undo an update - reset old value
	   (let ((old-value (car data))
		 (old-status (second data))
		 (new-pair (the-last-arc-of-path *input* (third data))))
	     (cond ((path-null (third data))
		    (setf *input* old-value))
		   ((consp new-pair)
		    (setf (third new-pair) old-status)
		    (setf (second new-pair) old-value))
		   ((null new-pair)
		    ;; dont do anything: the part containing new-pair has
		    ;; probably been already removed
		    )
		   (t
		    (cerror "Continue"
			    "Error in UNDO:~%~
                             - new-pair is an atom! ~s~%~
		             - old-value is ~s~%~
		             - path is ~s~%"
		       new-pair old-value (third data))))))

	  ((eq type 'aa) ; undo an add-agenda: remove it for true - data=id
	   (setf *agenda* (remove-if
			   #'(lambda (x) (= (agenda-item-id x) data))
			   *agenda*)))

	  ((eq type 'ar) ; undo a remove-agenda
	   (let ((item (find-agenda (- data))))
	     (when item
	       (setf (agenda-item-id item) data))))

	  #+ignore((eq type 'ac) ; undo an add-constituent-agenda: remove it for true
	   (setf *constituent-agenda*
		 (remove-if #'(lambda (x)
				(= (constituent-agenda-item-id x) data))
			    *constituent-agenda*)))

	  #+ignore((eq type 'acr) ; undo a remove-constituent-agenda
	   (let ((item (find-constituent-agenda (- data))))
	     (when item
	       (setf (constituent-agenda-item-id item) data))))

	  (t (error "Error in UNDO:~%- Unknown type of undo: ~s~%" type)))))




;; -----------------------------------------------------------------------
(provide "$fug5/backtrack")
;; -----------------------------------------------------------------------
