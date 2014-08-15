;;; -*- Mode:Lisp; Syntax:Common-Lisp; Package:FUG5 -*-
;;; -----------------------------------------------------------------------
;;; File:         TRACE.L
;;; Description:  Tracing functions for FUG5
;;; Author:       Michael Elhadad
;;; Created:      06-Mar-88
;;; Modified:     15 Nov 89
;;;               30 Apr 90 - Moved exports from here to fug5.
;;;               16 May 90 - path-p and leaf-p.
;;;               17 May 90 - moved macros to macros.l
;;;               20 Jun 90 - allowed for equations in walk-fd.
;;;               27 Jun 90 - added trace-category
;;;               25 Jul 90 - fixed trace-category
;;;               22 Feb 91 - added trace-bk-class
;;;               25 Feb 91 - refined trace for bk-class with
;;;               *frames-skipped*
;;;               05 May 91 - allow to untrace flags not in the grammar so
;;;               that flags in external functions can be disabled.
;;;               23 Dec 91 - added call to use-grammar is all-tracing-flags
;;;                         - added trace-enable-alt trace-disable-alt
;;;               31 Dec 91 - fixed bug in walk-fd for ({...pattern} x)
;;;               06 Jan 92 - added hyper-trace
;;;               17 May 92 - added trace-bp
;;;               20 May 92 - fixed bug in trace-category (R.Sparks)
;;;               24 Sep 92 - added %break% handling.
;;;               20 Oct 92 - added level to trace-format
;;; Package:      fug5
;;; Macros:       trace-format, trace-indent, tracing-flag, trace-demo,
;;;               control-demo, handle-trace
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
(format t "Tracer...~%")


;; ------------------------------------------------------------
;; TRACING
;; ------------------------------------------------------------
(defun internal-trace-on () (setq *bigtracing* t))
(defun internal-trace-off () (setq *bigtracing* nil))

(defun trace-on ()
  "unifier is traced according to directives in grammar"
  (setq *global-tracing* t))

(defun trace-off ()
  "Reverse off trace-on."
  (setq *global-tracing* nil))

(defun trace-level (&optional level)
  "Set level of detail in printing tracing information
                       00: feature level action
                       05: unimportant alt-level action
                       10: alt-level action - branch number trying
                       12: demo messages
                       15: freeze, ignore, bk-class
                       20: constituent level action
                       30: important failure - end of alt"
  (if level
    (setq *level-tracing* level)
    level))

(defun trace-determine (&key (on t))
  "Print or not the sentence found with an any when determine fails"
  (setf *trace-determine* on))

(defun trace-bk-class (&optional (on t))
  "Trace when a bk-class path fails"
  (setf *trace-bk-class* on))

(defun trace-wait (&optional (on t))
  "Trace wait activity"
  (setf *trace-wait* on))

(defun trace-cset (&optional (on t))
  "Trace cset activity"
  (setf *trace-cset* on))

(defun trace-alts (&optional (on t))
  "Trace alt backtracking only"
  (setf *trace-alts* on))

(defun trace-bp (&optional (freq *trace-bp-freq*))
  "Print a . every freq backtracking points."
  (cond ((numberp freq)
	 (setf *trace-bp* t)
	 (setf *trace-bp-freq* freq))
	((null freq)
	 (setf *trace-bp* nil))
	(t (error "Invalid arg: ~s~%
No argument or nil to turn off trace, n to emit . every n backtracking points."
		  freq))))


;; New lines are bug fixes from Randall Sparks <randall@advtech.uswest.com>
(defun trace-category (cat &optional (status t))
  "If cat is :all, trace all categories.
   If cat is nil, untrace all categories.
   If status is t, trace category cat, otw, untrace it.
   Works if cat is a symbol or a list of symbols."
  (cond
   ((null cat) (setf *traced-categories* nil))
   ((eq cat :all) (setf *traced-categories* :all))
   (status
    (cond ((symbolp cat)
	   (if (eq *traced-categories* :all)			; new
	       (setf *traced-categories* (list cat))		; new
	       (pushnew cat *traced-categories*)))
	  ((and (consp cat) (every #'symbolp cat))
	   (if (eq *traced-categories* :all)			; new
	       (setf *traced-categories* cat)			; new
	       (setf *traced-categories* (union cat *traced-categories*))))
	  (t (error "Argument must be a symbol or a list of symbols"))))
   ((null status)
    (cond ((eq *traced-categories* :all)			; new
	   (format t "Can't untrace particular categories when ~
                      *traced-categories* is set to :ALL.~%")	; new
	   *traced-categories*)					; new
	  ((symbolp cat)
	   (setf *traced-categories* (remove cat *traced-categories*)))
	  ((and (consp cat) (every #'symbolp cat))
	   (setf *traced-categories* (set-difference *traced-categories* cat)))
	  (t (error "Argument must be a symbol or a list of symbols"))))))


(defun hyper-trace-category (cat &optional (status t))
  "If cat is :all, hyper-trace all categories.
   If cat is nil, un-hyper-trace all categories.
   If status is t, hyper-trace category cat, otw, un-hyper-trace it.
   Works if cat is a symbol or a list of symbols."
  (cond
   ((null cat) (setf *hyper-traced-categories* nil))
   ((eq cat :all) (setf *hyper-traced-categories* :all))
   (status
    (cond ((symbolp cat)
	   (pushnew cat *hyper-traced-categories*))
	  ((and (consp cat) (every #'symbolp cat))
	   (setf *hyper-traced-categories*
		 (union cat *hyper-traced-categories*)))
	  (t (error "Argument must be a symbol or a list of symbols"))))
   ((null status)
    (cond ((symbolp cat)
	   (setf *hyper-traced-categories*
		 (remove cat *hyper-traced-categories*)))
	  ((and (consp cat) (every #'symbolp cat))
	   (setf *hyper-traced-categories*
		 (set-difference *hyper-traced-categories* cat)))
	  (t (error "Argument must be a symbol or a list of symbols"))))))



(defun handle-trace (flag frame &optional force-check)
  "Turns on or off tracing when a flag is found in the grammar.
  If force-check is true, does not check if flag is a valid tracing-flag"
  (when (and (or (tracing-flag flag) force-check)
	     (trace-enabled flag))
    (cond ((eq flag *all-trace-off*)
	   (setq *local-tracing* nil))
	  ((eq flag *all-trace-on*)
	   (setq *local-tracing* t))
	  ((eq flag *trace-break*)
	   (break "Break in grammar"))
	  ((member flag (frame-trace-flags frame))
	   ;; trace off
	   (trace-format (not force-check) frame 0 "UNTRACING FLAG ~s" flag)
	   (decf (frame-trace-level frame))
	   (setf (frame-trace-flags frame)
		 (remove flag (frame-trace-flags frame))))
	  (t
	   ;; trace on
	   (unless (member flag (frame-trace-flags frame))
	     (push flag (frame-trace-flags frame))
	     (incf (frame-trace-level frame))
	     (trace-format (not force-check) frame 0
			   "TRACING FLAG ~s" flag))))))


;; ------------------------------------------------------------
;; ENABLE and DISABLE stuff
;; ------------------------------------------------------------

(defun trace-enabled (flag)
  (or (eq flag *all-trace-on*)
      (eq flag *all-trace-off*)
      (and flag (not (member flag *trace-disabled*)))))

(defun trace-enable (lflags &optional (grammar *u-grammar*))
  "Enable lflags to act as real tracing flags. (cf. trace-disable)"
  (let ((atf  (all-tracing-flags grammar))
	(lflags (if (symbolp lflags) (list lflags) lflags)))
    (mapc #'(lambda (flag)
	      (if (member flag atf)
		  (progn
		    (setq *trace-disabled* (remove flag *trace-disabled*))
		    (format t "~s is enabled.~%" flag))
		(format t "~s is not a tracing flag.~%" flag)))
	  lflags))
  (values))

(defun trace-disable (lflags &optional (grammar *u-grammar*))
  "Disable tracing flag.  Alt will behave as if it were not traced."
  (let ((atf  (all-tracing-flags grammar))
	(lflags (if (symbolp lflags) (list lflags) lflags)))
    (mapc #'(lambda (flag)
	      (if (member flag atf)
		(format t "~s is disabled.~%" flag)
		(format t "Warning: ~s does not look like a tracing flag.~%"
			flag))
	      (pushnew flag *trace-disabled*))
	  lflags)
    (format t "~s flags still enabled.~%"
	    (max 0 (- (length atf) (length *trace-disabled*)))))
  (values))

(defun trace-enable-all (&optional (grammar *u-grammar*))
  "Enable all existing flags"
  (setq *trace-disabled* nil)
  (format t "All ~s flags enabled.~%" (length (all-tracing-flags grammar)))
  (values))

(defun trace-disable-all (&optional (grammar *u-grammar*))
  "Disable all active tracing flags. Use trace-disable to selectively
   reenable one of the flags."
  (setq *trace-disabled* (all-tracing-flags grammar))
  (format t "All ~s flags disabled.~%" (length *trace-disabled*))
  (values))

(defun trace-disable-match (string &optional (grammar *u-grammar*))
  "Disable all tracing flags whose name matches string"
  (let ((atf (all-tracing-flags grammar)))
    (setq *trace-disabled*
	  (union *trace-disabled* (match-symbols atf string)))
    (format t "~s are disabled.~%~s flags still enabled.~%"
	    *trace-disabled*
	    (max 0 (- (length atf) (length *trace-disabled*)))))
  (values))

(defun trace-enable-match (string &optional (grammar *u-grammar*))
  "Enable all tracing flags whose name matches string"
  (let ((match 	(match-symbols (all-tracing-flags grammar) string)))
    (setq *trace-disabled* (set-difference *trace-disabled* match))
    (format t "The following flags are re-enabled: ~s~%" match)
    (values)))


(defun trace-enable-alt (alt-name &key (expansion t) (grammar *u-grammar*))
  "Enable all tracing flags under a def-alt of def-conj"
  (let ((gr (get-grammar alt-name :expansion expansion)))
    (if (null gr)
      (values)
      (trace-enable (all-tracing-flags gr) grammar))))


(defun trace-disable-alt (alt-name &key (expansion t) (grammar *u-grammar*))
  "Disable all tracing flags under a def-alt of def-conj"
  (let ((gr (get-grammar alt-name :expansion expansion)))
    (if (null gr)
      (values)
      (trace-disable (all-tracing-flags gr) grammar))))


;; ------------------------------------------------------------
;; Utilities for enable/disable

(defun match-symbols (lsymb string)
  "Given a list of symbols and a pattern, returns the sublist of
   symbols whose name match string and those who don't (2 values)"
  (let ((match nil) (no-match nil) (string (string-upcase string)))
    (mapc #'(lambda (s)
	      (if (search string (symbol-name s))
		  (push s match)
		(push s no-match)))
	  lsymb)
    (values match no-match)))


(defun all-tracing-flags (&optional (grammar *u-grammar*))
  "Return a list of all the tracing flags defined in grammar in
the order in which they appear in grammar."
  (reverse (remove-duplicates (walk-fd (use-grammar grammar) nil))))

(defun enabled-tracing-flags (&optional (grammar *u-grammar*))
  "Return the list of tracing flags enabled in grammar."
  (set-difference (all-tracing-flags grammar) *trace-disabled*))

(defun disabled-tracing-flags (&optional (grammar *u-grammar*))
  "Return the list of tracing flags disabled in grammar."
  ;; Do the intersection because there can be several grammars in the Lisp
  ;; world and I have only one *trace-disabled* variable.
  (intersection *trace-disabled* (all-tracing-flags grammar)))


;          An FD has the following syntax:
; 	   FD   <=  ({F} PAR1 {F} PAR2 {F} ... {F} PARn)
;          PAR  <=  ("alt" {F} {I} (FD1 FD2 ... FDn))
;          PAR  <=  ("ralt" {F} {I} (FD1 FD2 ... FDn))
; 	   PAR  <=  ("opt" {F} FD)
; 	   PAR  <=  (sym RHT)
; 	   PAR  <=  (sym PATH)
;          PAR  <=  (PATH RHT)
;          PAR  <=  (PATH PATH)
;          PAR  <=  (SPC special-case)
;          SPC  <=  "pattern "| "cset" | "test" | "control" | "fset"
;          SPC  <=  a-symbol-declared-special
; 	   RHT  <=  sym
; 	   RHT  <=  FD
;          PATH <=  an-existing-path-in-fd
; 	   F    <=  an-atomic-tracing-flag | (trace {...} F)
;          I    <=  (index {...} PATH) | (index {...} an-existing-atom)
(defun walk-fd (fd collect)
  (cond ((leaf-p fd) collect)
	((path-p fd) collect)
	((leaf-p (car fd))
	 (walk-fd (cdr fd) (cons (car fd) collect)))
	((eq (caar fd) 'alt)
	 (walk-fd (cdr fd) (walk-alt (car fd) collect)))
	((eq (caar fd) 'ralt)
	 (walk-fd (cdr fd) (walk-alt (car fd) collect)))
	((eq (caar fd) 'opt)
	 (walk-fd (cdr fd) (walk-opt (car fd) collect)))
	((or (member (caar fd) *special-attributes*)
	     (and (path-p (caar fd))
		  (member (car (path-last (caar fd)))
			  *special-attributes*)))
	 (walk-fd (cdr fd) collect))
	((path-p (second (car fd)))
	 (walk-fd (cdr fd) collect))
	(t (walk-fd (cdr fd) (walk-fd (second (car fd)) collect)))))


(defun walk-alt (alt-pair collect)
  (multiple-value-bind (branches trace index) (branches alt-pair)
    (declare (ignore index))
    (when trace (push trace collect))
    (mapc #'(lambda (fd)
	      (setq collect (walk-fd fd collect)))
	  branches)
    collect))


(defun walk-opt (opt-pair collect)
  (multiple-value-bind (fd trace) (option opt-pair)
    (when trace (push trace collect))
    (walk-fd fd collect)))


;; ------------------------------------------------------------
(provide "$fug5/trace")
;; ------------------------------------------------------------
