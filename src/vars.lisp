;;; -*- Mode:Lisp; Syntax:Common-Lisp; Package: FUG5 -*-
;;; -----------------------------------------------------------------------
;;; File:         vars.l
;;; Description:  Global variables used in FUF
;;; Author:       Michael Elhadad
;;; Created:      11 Feb 1992
;;; Modified:     20 Jul 1992: move defstructs here to ease compilation.
;;;               01 Jun 1993: make path-equal work for 2 symbols
;;;               09 Jun 1993: add ^~ notation in paths to escape from
;;;                            list with notation ^n~.
;;;               07 Jun 1994: add *conflate-leaves*
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

;; ============================================================
;; STRUCTURES
;; ============================================================
;;; Data structures for backtracking
;;- Frame: keeps a stack of frames representing the state
;;  of the computation at a backtraking point. That is all
;;  the variables on trace, undo and tests that are kept as one variable
;;  passed to all functions.
;;- Test: keeps a list of all the test statements met during unification
;;  of a branch to be tested at Determination-time. Path serves to keep
;;  the context of the statement within *input*.

(defstruct frame
  (undo (list 'root))      ;; list of UNDOs
  (tests nil)              ;; all the TEST expressions met during unif.
  (trace-level 0)          ;; how many trace-flags are embedded
  (trace-flags nil)        ;; which trace-flags are active
  (name :anonymous))       ;; name of the frame

(defstruct test
  test                     ;; the sexpr to be evaluated
  path)                    ;; path to determine the pointer-references
                           ;; from the context of the test.

;; ------------------------------------------------------------
;; Define PATH structure, reader and writer plus handling functions.
;; ------------------------------------------------------------

(defstruct (path
	    (:print-function print-path)
	    (:copier copy-path))
  l)

(defun print-path (path stream depth)
  (declare (ignore depth))
  (format stream "{~{~s~^ ~}}" (path-l path)))

(defun copy-path (path)
  (make-path :l (copy-tree (path-l path))))

(defun path-equal (p1 p2)
  (or (eq p1 p2)
      (and (path-p p1) (path-p p2) (equal (path-l p1) (path-l p2)))))

(defun equality (a b)
  (if (path-p a)
    (path-equal a b)
    (equalp a b)))

(defun curly-brace->path-reader (stream sub-char)
  "Reads {} into path structures."
  (declare (ignore sub-char))
  (make-path :l (normalize-path (read-delimited-list #\} stream t))))

(set-macro-character #\{ #'curly-brace->path-reader T)
;;; This prevents } from being part of form
(set-macro-character #\} (get-macro-character #\)))

(defun normalize-path (list &optional result &aux n)
  "Transforms JR's chars into their normal meaning:
   ^ and ^~ stand for themselves
   ^n stands for n ^ in sequence
   ^n~ stands for n ^ followed by ^~
   ~n stands for cdr n-1 times followed by car."
  (let* ((s (if (symbolp (car list)) (symbol-name (car list))))
	 (l (if s (length s))))
    (cond ((null list) (nreverse result))
	  ;; don't test for (eq '^) so that reader works even in packages
	  ;; that do not import fug5::^.
	  ((and (symbolp (car list))
		(string= s "^"))
	   (normalize-path (cdr list) (cons '^ result)))
	  ((and (symbolp (car list))
		(string= s "^~"))
	   (normalize-path (cdr list) (cons '^~ result)))
	  ((and (symbolp (car list))
		(char= (char s 0) #\^))
	   (setf n (parse-integer
		    s :start 1
		    :end (if (char= (char s (- l 1)) #\~) (- l 1) l)))
	   (normalize-path
	    (cdr list)
	    (nconc (if (char= (char s (- l 1)) #\~) (list '^~) nil)
		   (make-sequence 'list n :initial-element '^)
		   result)))
	  ((and (symbolp (car list))
		(char= (char (symbol-name (car list)) 0) #\~)
		(> (length (symbol-name (car list))) 1)
		(setf n (parse-integer (symbol-name (car list)) :start 1)))
	   (normalize-path
	    (cdr list)
	    (nconc (list 'car)
		   (make-sequence 'list (- n 1) :initial-element 'cdr)
		   result)))
	  (t (normalize-path (cdr list) (cons (car list) result))))))


;; This allows us to compile source files containing {}
;; and to reload them.
(defmethod make-load-form ((obj path) &optional env)
  (make-load-form-saving-slots obj))

;; ============================================================
;; VARIABLES
;; ============================================================

(defvar *fuf-dir* "c:/documents and settings/michael/application data/fuf/"
  "The root folder containing the FUF and SURGE package")

(defvar *fuf-src-dir* (concatenate 'string *fuf-dir* "src/")
  "The folder containing the FUF source code")

(defvar *u-grammar* nil "The unification grammar")

(defvar *conflate-leaves* t
  "When a path points to a leaf, copies the leaf or keep the pointer")

(defvar *disjunctive-attributes* '(alt opt ralt)
  "List of attributes defining a type of disjunction.")

;; Cset is added to this list when the cset parameter is known
(defvar *special-attributes* (append '(pattern fset control test)
				     *disjunctive-attributes*)
  "List of attributes whose value is not a regular fd.
   Is updated when new unification method is defined.")

(defvar *lexical-categories*
  '(verb noun adj prep conj relpro adv punctuation modal ordinal cardinal
	 phrase)
  "The Lexical Categories not to be unified")

(defvar *top* nil "Tracing in depth of top level")

(defvar *input* nil)

(defvar *cat-attribute* 'cat
  "The attribute marking the name of a category for constituents.")

(defvar *cset-attribute* 'cset
  "The attribute identifying the constituent set in an FD")

(defvar *added-cset* nil
  "Check if a cset has been added during unification.")

(defvar *fail* :fail "Value returned when failing")

(defvar *same* :same-path
  "Value passed to backtracking points when failing at a non-leaf location")

(defvar dummy-frame (make-frame) "Used in undo-one")

;; How to recognize a path that requires intelligent backtracking:
;; it is declared in *bk-classes* along with its class.
(defvar *bk-classes* (make-hash-table))
(defvar *is-bk-failure* nil)
(defvar *failure-address* (make-path))
(defvar *class-of-failure* nil)
(defvar *changes-made* nil)

(defvar *ignore-control* nil "Whether controls are evaluated or not")
;; *ignore-control* should be t when normalizing an fd.

(defvar *any-at-unification* t "Whether tests for ANY are done at unifcation
  time or at determination time")

(defvar *use-given* t "Whether u(nil,given) fails or not.")
;; *use-given* should be nil when normalizing an fd or checking a grammar.

(defvar *use-any* t "Whether u(nil,any) fails")
;; *use-any* should be nil when normalizing an fd or checking a grammar.

(defvar *use-wait* t "Whether determine checks the waiting agenda or not.")

(defvar *agenda-policy* :force
  "Can be either :force or :keep.
   Determine what to do with frozen alt at the end of unification:
   - keep them unevaluated in result
   - force their evaluation.")

(defvar *input-pair* '(:dummy nil :i)
  "A pair that indicates that the failure was caused by the input and
required true backtracking - to fool bk-class")

(defvar *fuf-systems* (make-hash-table)
  "The table of alts defined by a def-alt statement")

(defvar *fuf-conjs* (make-hash-table)
  "The table of conjs defined by a def-conj statement")

(defvar *fuf-need-refresh* nil
  "Flag to indicate that an alt has been (re)defined and global FUG needs
to be refreshed.")

(defvar *fuf-refreshed* nil
  "A table linking grammar definitions to their expanded form.")

(defvar *default-external-function*)

(defvar *dictionary* nil "The lexicon")
(defvar *irreg-plurals* nil
  "List of singular-form paired with plural-form")
(defvar *irreg-verbs* nil
  "List of root verb paired with list of present-third-person-singular,
  past, present-participle and past-participle forms")
(defvar *irreg-adjs* nil
  "List of adjectives paired with comparative and superlative forms")
(defvar *pronouns-person* nil)
(defvar *pronouns-number* nil)
(defvar *pronouns-gender* nil)
(defvar *pronouns-case* nil)

;; ------------------------------------------------------------
;; Default on person is third, on number is singular, on tense is present
(defvar *indefinite-article* "a**"
  "An indefinite article when value of next word is still unknown
	(we don't know if it is a or an)")
(defvar *a-an-marker* "***"
  "Marker to add to a noun that must be preceded by a determiner an
   (like an acronym)")

(defvar *restart* :restart)

(defvar *linelength* 72)
(defvar *filelinelength* 80)
(defvar *position* 0)

(defvar *max-name-length* 10
  "Maximum size of a node name that fits on a line")

(defvar *tests* (make-hash-table)
  "Hash table test-name to input / result")
(defvar *ordered-tests* nil
  "The list of names of tests in the order where they have been defined")
(defvar *test-limit* 2000
  "Max number of bk points to use when running a test.")

(defvar *trace-marker* #\% "The first character a tracing flag must have")
(defvar *all-trace-off* '%TRACE-OFF% "This flag shuts off all tracing")
(defvar *all-trace-on*  '%TRACE-ON% "This flag re-allows tracing")
(defvar *trace-break* '%BREAK% "Break in grammar")
(defvar *global-tracing* nil "Want trace messages to be printed")
(defvar *bigtracing* nil "Generates internal traces")
(defvar *trace-determine* t "Trace the determination stage or not")
(defvar *trace-disabled* nil "List of disabled tracing flags")
(defvar *local-tracing* t
  "Has tracing been turn on or off by %TRACE-on/off%")
(defvar *traced-categories* :all "List of traced categories.
   The unifier emits a message before starting work on a traced category.")
(defvar *hyper-traced-categories* nil "List of hyper-traced categories.
   The unifier prints each constituent before starting work on a
   hyper-traced category.")
(defvar *trace-bk-class* nil "Trace when a bk-class fails")
(defvar *bk-frames-skipped* 0 "How many frames skipped by a bk-class")
(defvar *trace-wait* t "Trace wait activity")
(defvar *trace-cset* t "Trace cset activity")
(defvar *trace-alts* nil "Trace alts")
(defvar *trace-bp* nil "Print out . during unification")
(defvar *trace-bp-freq* 10 "How many backtracking points to a . in trace")
(defvar *level-tracing* 0 "Level of detail in printing tracing information
                       00: feature level action
                       05: unimportant alt-level action
                       10: alt-level action (branch number trying)
                       12: demo messages
                       15: freeze, ignore, bk-class
                       20: constituent level action
                       30: important failure (end of alt)")

(defvar *typed-features* nil
  "The symbols which have subtypes.")

(defvar *warn-type-redefinition* nil
  "Warn when a type is redefined with define-feature-type.")

(defvar *agenda* nil
  "The list of frozen alts waiting for more info")

(defvar *agenda-id* 0
  "The id of the latest allocated agenda id")

(defvar *constituent-agenda* nil
  "Stack of constituents whose unification and traversal have been
delayed")

(defvar *constituent-agenda-id* 0
  "Id of latest allocated constituent-agenda")


;; -----------------------------------------------------------------------
(provide "$fug5/vars")
;; -----------------------------------------------------------------------
