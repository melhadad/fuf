;;; -*- Mode:Lisp; Syntax:Common-Lisp; Package:FUG5 -*-
;;; -----------------------------------------------------------------------
;;; File:         PATTERN.L
;;; Description:  Pattern unifier
;;; Author:       Michael Elhadad
;;; Created:      27-Jul-87
;;; Modified:     12-Aug-88
;;;               18 May 90: path is a special type (in constraint->fd)
;;;               15 Jun 90: added pair argument.
;;;               28 Jun 90: pattern-unify with NIL pattern.
;;;               02 Jul 90: add path2 arg.
;;;               05 May 91: fix to allow for paths in patterns (mergeable)
;;;               19 May 91: fix major bug in necessary-mins.
;;;               13 Nov 91: changed call to *fail* for bk-class
;;;               17 Dec 91: Renamed first to gen-first
;;;               22 May 92: get around bug in Allegro compiler with block nil
;;;               20 Oct 92: Added level to trace-format
;;;               01 Dec 92: Fixed all mergeable deal with paths.
;;;               15 Dec 92: Fixed major bug of not using equality (vs eq).
;;;               08 Feb 93: Authorize (pattern given/any)
;;;               09 Feb 93: Add missing :test #'equality in necessary-mins
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
(format t "Pattern unifier...~%")

; ------------------------------------------------------------
; COMMENTS
; ------------------------------------------------------------

; Syntax : (UP &rest <patterns>)
; Works with any number of patterns in input.
; Returns a generator of all possible unifications.
; A Unification is a pair (resulting-pattern list-of-constraints)
; Constraints are lists of atoms that must be made equivalent.
; If patterns are incompatible, returns *THE-EMPTY-GENERATOR*.

; What is a pattern :
;     a pattern is a constraint expressed on the ordering of the nodes
;     present in a FD (cf. unification grammar module).
;     Patterns are used at the linearization stage to order words in
;     sentences.

; Syntax of patterns.
;     If nodes are noted Ni, a pattern is a list
;     P == (Ni|...|#)*
;     ... and # are special symbols noted DOTS and POUND in the program.
;     A sequence DOTS DOTS is forbidden.
;     A sequence DOT POUND DOTS is forbidden.
;     A given node N must appear at most ONCE in a pattern.
;     Examples : (dots subject dots verb dots object)
;                (focus comment)
;                (prep pound dots np)

; What is pattern unification.
;     When we unify 2 FDs together, we can find constraints on the ordering
;     coming from different patterns specified for the same FD. Unifying
;     patterns means creating a single pattern which expresses both sets
;     of constraints on the ordering of the FD.
;     For example, (dots verb dots object) unified with (subj dots) gives
;     as one of the possible results (subj dots verb dots object).
;     We also want to be able to deal with cases like unifying together
;     (focus dots) with (prot verb goal). Here we must also specify that
;     FOCUS and PROT must be made equivalent in order to find a compatible
;     ordering (that would be (focus dots verb goal).
;     That is, when unifying patterns, we may have to add constraints
;     on the FDs we deal with, not only on the patterns.
;     In this module, we will call this kind of binding of nodes together
;     a CONSTRAINT added to the FD. We don't resolve constraints here, but
;     just record them to pass them up to the FD-unifier.
;     The result of a pattern unification must therfore be a pair
;     (resulting-pattern constraints-added).

; Mergeable nodes :
;     Cf KAY's notation (a *b c) to indicate that b can receive bindings.
;     Certain nodes in patterns are marked as being "mergeable", that is,
;     the pattern unifier can add constraints on them. By default, nodes
;     are not mergeable.
;     The notation is *b for a mergeable node. In the program (* n).
;     A constraint (that is, a set of nodes) is mergeable if it contains at
;     most one non-mergeable node.
;     For example, FOCUS could be a mergeable node which would be made
;     equivalent to PROT in an active sentence (cf McKeown-Paris/ACL-87).

; Pattern unification is non-deterministic
;     As everything else in unification grammars, unifying 2 patterns can
;     give MANY different results. For example : (... a ... b ...) with
;     (... x ...) gives 5 different compatible patterns as a result :
;     [We note N1/N2 the constraint that N1 and N2 must be made equivalent]
;     (... x ... a ... b ...)
;     (... a/x ... b ...)
;     (... a ... x ... b ...)
;     (... a ... b/x ...)
;     (... a ... b ... x ...)

; When it fails.
;     2 patterns can be simply incompatible. In this case, unification fails.
;     Simple examples are : (a b) with (x)
;                           (a b) with (x a)
;                           (# a) with (x)   [(# a) MUST have 2 elts)]

; Method used.
;     Since we have to deal with a combinatorial problem, and we know
;     in advance that we won't need all the solutions in general, I have
;     chosen to program this procedure using principles of LAZY EVALUATION.
;     The idea is that we compute only what we need to find the FIRST
;     solution, and keep track of what we need to compute the NEXT solution.
;     All of this is incredibly well expressed using the SCHEME notion of
;     STREAMS (or GENERATORS) [not to be confused with Common-Lisp notion
;     of streams as I/O objects].
;     All the tools to manipulate generators are in the package GENERATOR.
;     We refer you there for an introduction.
;
;     The main function to call is UP.
;     UP only sets up the environment to call the workhorse function
;     UNIFY-PATTERN.
;     The algorithm is the following :
;     INPUT : a list of patterns.
;     The basic step is to find which element must be the FIRST one
;     in the unified pattern.
;     We define 2 types of minima :
;     NECESSARY MIN : appear as first elements in one pattern.
;     POSSIBLE  MIN : appear preceded by dots only in a pattern.
;     Minima never have other predecessor than dots.
;     Now, we can merge minima together to create a constraint.
;     So the basic recursive step is :
;     1/ Find all necessary minima in the current patterns.
;     2/ Find all possible  minima ...
;     3/ Choose any number of possible min.
;        [This is NON-determinist - backtraking point here]
;     4/ Create a constraint that all necessary min. and possible min.
;        picked up must be made equivalent.
;     4bis/ If this constraint is not MERGEABLE fail.
;     5/ Choose one element of all the min. retained as a representant.
;     6/ This representant becomes the first elt. of the result, and the
;        constraint is added to the set of constraints.
;     7/ Remove all elements used at this stage from the pattern and recurse
;        on the rest of the elements.

;     When do we stop ?
;     When either we have dealt with all elements (all remaining patterns
;     are empty) or we have an incompatibility (when one pattern is empty
;     and another is not).

;     When do we add pound and dots to the result ?
;     Special care must be exercised to handle pound and dots. See code
;     for details.
; ------------------------------------------------------------



;; ---------------------------------------------------------------------
;; PATTERN-UNIFY
;; ---------------------------------------------------------------------

(defun pattern-unify (fd1 fd2 pair1 pair2 path1 path2 frame fail success
			  &key (pair :unknown))
 "Called by unify when we have to unify 2 pairs containing patterns.
  Sets all the environment before calling UP to compute all possible
  unified patterns and constraints, and try them one after the other
  (handles the backtracking).
  PAIR1 and PAIR2 must be sublists of FD1 and FD2 both starting with PATTERN.
  PAIR2 must be equal to (car FD2).
  FD1 must be a sublist of *INPUT*.
  FD1 must be equal to (gdp *input* PATH).
  FAIL must be a continuation to call when we fail in this function."

 (cond
  ((or (and *use-given* (eq (safe-second pair2) 'given))
       (and *any-at-unification* (eq (safe-second pair2) 'any)))
   (if (safe-second pair1)
     (unify fd1 (cdr fd2) path1 path2 frame fail success :pair pair)
     (*fail* fail frame path1 path2 pair1
	     "fail in trying ~s with ~s at level ~s"
	     (safe-second pair1) (safe-second pair2) path1)))
  ((eq (safe-second pair2) 'any)
   (unless (safe-second pair1)
     (push (make-test :test '(any-p path) :path (path-extend path1 'pattern))
	   (frame-tests frame)))
   (unify fd1 (cdr fd2) path1 path2 frame fail success :pair pair))
  ((null (safe-second pair1))
   (update-pair pair1 (safe-second pair2)
		(path-extend path1 'pattern) frame)
   (trace-format (frame-trace-flags frame) frame 0
		 "Enriching input with ~s at level ~s"
		 pair1 path1)
   (unify fd1 (cdr fd2) path1 path2 frame fail success :pair pair))
  ((null (safe-second pair2))
   (unify fd1 (cdr fd2) path1 path2 frame fail success :pair pair))
  (t
   (let ((patterns-stream (up (safe-second pair1) (safe-second pair2))))
     (one-pattern-unify fd1 fd2 pair1 pair2 path1 path2 frame
			patterns-stream fail success pair)))))


(defun one-pattern-unify (fd1 fd2 pair1 pair2 path1 path2 frame
			      patterns-stream fail success pair)
  "The workhorse of the pattern unification, called by PATTERN-UNIFY.
  Tries all the unifications possible given by patterns-stream."

  ; Here is what we do :
  ; 0/ The stream of possible patterns has been computed in PATTERN-UNIFY
  ; 1/ We mark a backtraking point (with catch).
  ; 2/ A pattern unified comes with constraints associated. We translate
  ;    these constraints to an FD syntax (with constraint->fd), that is
  ;    we translate a pair (a b) to an fd (a (b))
  ; 3/ We update the pattern in FD1 (with the setf).
  ; 4/ We try the first pattern and its constraints (recursive call to unify
  ;    with the new grammar fd2 including the constraints but w/o the pattern)
  ; 5/ We note that next time we'll try with the next possible pattern
  ;    (continuation "fail" will be called with (next patterns-stream))

  (if (empty patterns-stream)
      (*fail* fail frame path1 path2 pair
	      "Fail on unifying pattern ~s with ~s at level ~s"
	      (second pair1) (second pair2) path1)
      (let* ((pattern-constraint (gen-first patterns-stream))
	     (new-pattern (car pattern-constraint))
	     (new-fd (constraints->fd (second pattern-constraint) path2)))
	(trace-format (frame-trace-flags frame) frame 0
		      "Unifying ~s with ~s" (second pair1) (second pair2))
	(trace-format (frame-trace-flags frame) frame 0
		      "Trying pattern : ~s" new-pattern)
	(trace-format (frame-trace-flags frame) frame 0
		      "Adding constraints : ~s" new-fd)
	(update-pair pair1 new-pattern (path-extend path1 'pattern) frame)
	(backtrack frame new-frame nil :pattern ;; patterns have no bk-class
	  (unify fd1
		 ; add constraints, remove pattern from fd2
		 (append new-fd (cdr fd2))
		 path1 path2
		 new-frame
		 ; next time, we'll have only NEXT to try.
		 (make-failure fail
		   (one-pattern-unify
		    fd1 fd2 pair1
		    pair2 path1 path2 frame
		    (next patterns-stream) fail success pair))
		 success
		 :pair pair)))))


; ------------------------------------------------------------
; TOP LEVEL FUNCTION : (UP &rest PATTERNS)
; ------------------------------------------------------------

(defun up (&rest patterns)
  "Pattern Unification. Returns a generator producing all possible
  unifications of the arguments"
  (let* ((patterns (pre-process-patterns patterns))
	 (necessary-init (necessary-mins patterns))
	 (possible-init  (possible-mins patterns)))
    (unify-patterns patterns necessary-init possible-init
		    (stream-of-mins necessary-init possible-init)
		    nil ; nothing in the unification so far
		    nil ; no constraint added so far
		    (freeze nil)))) ; if we fail now, nothing else to try.


; ------------------------------------------------------------
; UTILITIES
; ------------------------------------------------------------

(defun empty-pattern (pattern)
  "Predicate : a pattern is empty (whether null or just (dots))"
  (or (null pattern) (equal pattern '(dots))))

(defun poundp (pattern)
  "Predicate : a pattern starts with POUND"
  (eq (car pattern) 'pound))

; ------------------------------------------------------------
; CONSTRAINTS to FD
; ------------------------------------------------------------

;; Constraint is a list of paths or attributes.
;; Convention is that attribute att stands for {^ att}.
;; Convert (p1 ... pn) to ((p1 p2) .... (p1 pn))
;; All of this happens at level path (without pattern).
;; Must convert everything to absolute paths to avoid problems.  So a is
;; converted to (path-extend p a).
(defun constraint->fd (constraint path)
  "Converts a constraint of the form (a b c) to an fd of the form
  ((a (^ b)) (a (^ c))) expressing the fact that all nodes must be unified"
  (let* ((constraint (make-absolute constraint (path-extend path 'pattern)))
	 (first (car constraint)))
    (mapcar #'(lambda (item) (list first item)) (cdr constraint))))

(defun constraints->fd (constraints path)
  "Transforms a list of constraints in an equivalent Functional description.
  cf constraint->fd"
  (mapcan #'(lambda (constraint) (constraint->fd constraint path))
	  constraints))


; ------------------------------------------------------------
; MERGEABLE
; ------------------------------------------------------------

(defun marked-mergeable (node)
  "Predicate : a node in a pattern is marked as mergeable or not.
  Used in PRE-Process-patterns"
  (and (consp node) (eq (car node) '*)))

(defun mark-mergeable (node)
  "Mark a node as being mergeable."
  (list '* node))

(defun unmark-mergeable (pattern)
  (mapcar #'(lambda (node)
              (if (marked-mergeable node) (second node) node))
          pattern))

(defun mergeablep (node)
  "Predicate : a node in a pattern is marked to be mergeable or not"
  (declare (special *mergeable*))
  (member node *mergeable* :test #'equality))

(defun mergeable (constraint)
  "Predicate : a constraint (list of nodes appearing in patterns) is
  mergeable or not"
  (< (length (remove-if #'mergeablep constraint)) 2))

(defun post-process-patterns (patterns)
  "Undo all the modifs done by PRE-process-patterns"
  (declare (special *mergeable*))
  (mapc #'(lambda (pattern)
	    (mapc #'(lambda (node)
		      (cond ((marked-mergeable node)
			     (setq *mergeable*
				   (remove (second node) *mergeable*)))
			    (t
			     (setq *mergeable*
				   (remove node *mergeable*)))))
		  pattern))
	patterns)
  patterns)

(defun pre-process-patterns (patterns)
  "Transforms a list of marked/unmarked nodes in a list of atoms flagged
  on their Plists and ready to be processed by UNIFY-PATTERN"
  (declare (special *mergeable*))
  (setf *mergeable* nil)
  (mapcar #'dotspound->pounddots
	  (mapcar #'(lambda (pattern)
		      (mapcar #'(lambda (node)
				  (if (marked-mergeable node)
				    (progn
				      (pushnew (second node) *mergeable*
					       :test #'equality)
				      (second node))
				    node))
			      pattern))
		  patterns)))


(defun dotspound->pounddots (p)
  (cond ((null p) p)
	((null (car p)) p)
	((and (eq (car p) 'dots) (eq (second p) 'pound))
	 (append '(pound dots) (dotspound->pounddots (cddr p)))
	 (format *error-output* "~%!!! Found a #... in ~s~%
		 !!!I convert it to ...# (equivalent form).~%
		 !!!Edit your grammar to fix it for next time~%" p))
	(t (cons (car p) (dotspound->pounddots (cdr p))))))


(defun clean-pattern (pattern)
  "Removes DOTS and POUND from pattern. (leaves only names of constituents)
   Then remove mergeable marks (* c) -> c"
  (remove-if #'(lambda (item) (member item '(dots pound)))
	     (unmark-mergeable pattern)))


; ------------------------------------------------------------
; MINIMA
; Functions to find possible and necessary minima in a list of
; patterns.
; ------------------------------------------------------------

(defun possible-mins (patterns)
  "Given a list of patterns, find a list of elements that can (but must not)
  be the first of the pattern unifying all patterns. That is, elts appearing
  in such a way : (... e [rest of pat]) and have no predecessor elsewhere"
  (remove-duplicates
   (mapcan #'(lambda (second) ; check if there is a predecessor somewhere else
	       (block pos     ; than in the list second comes from.
		 (when (member second '(dots pound))
		       (return-from pos nil))
	         (mapc #'(lambda (pattern)
			   (let* ((after (member second pattern
						 :test #'equality))
				  (before (ldiff pattern after)))
			     (when (and after
					(not (equal before '(dots))))
				   (return-from pos nil))))
		       patterns)
		 (return-from pos (list second))))
	   (mapcar #'second ; take the seconds of the patterns
		   (remove-if-not #'(lambda (pattern) ; starting with dots
				      (and (cdr pattern)
					   (eq (car pattern) 'dots)))
				  patterns)))
   :test #'equality))


(defun necessary-mins (patterns)
  "Find set of elements in a list of patterns which
  must appear in a starting position: all non dots elements and those that
  precede them in another pattern."
  (remove-duplicates
   (mapcan #'(lambda (first)
	       ; filter out elts that are pound/dots or have a predecessor
	       (when (not (member first '(dots pound)))
		 (let ((predec nil))
		   (mapc #'(lambda (pattern)
			     (let* ((after (member first pattern
						   :test #'equality))
				    (before (ldiff pattern after)))
			       (when (and after before
					  (not (equal before '(dots))))
				 (push (car (last before)) predec))))
			    patterns)
		   (cons first predec))))
	   (mapcar #'car patterns))
   :test #'equality))



(defun stream-of-mins (necessary possible)
  "Returns a stream generating all possible minima from a list of patterns.
  There are 3 types of minima :
  - necessary min. An element starting one of the patterns that has no pred.
  - possible min. Appears in a pattern as (... e [rest]) and has no pred.
  - merging. A merging of any number of possible mins (creating a constraint).
  A min. is a list of minimal elts. that always contains all the necessary mins
  and one of the mergings.
  See also UP, necessary-mins and possible-mins."
  (filter #'mergeable
	  (if (null necessary)
	      (powerset possible)
	      (mapgen #'(lambda (constraint) (append necessary constraint))
		      (powerset possible)))))

(defun remove-min-pattern (patterns min)
  "Removes all elements of min from all patterns in a list of patterns.
  Removes also fronting pounds.
  See stream-of-mins to see how min should be computed"
  (mapcar #'(lambda (list)
	      (cond ((member (car list) min :test #'equality) (cdr list))
		    ((eq (car list) 'pound)  (cdr list))
		    ((and (eq (car list) 'dots)
			  (member (second list) min :test #'equality))
		     (cddr list))
		    (t list)))
	  patterns))


; ------------------------------------------------------------
; UNIFICATION
; Workhorse of the module
; ------------------------------------------------------------

(defun unify-patterns (patterns necessary possible min-stream
				ordered constraints fail)
  "Unify a list of patterns by accumulating minima in the list ORDERED.
  Returns a generator containing all possible unifications.
  An ordering is a pair (ordered-elts constraints).
  Should be called by top-level function UP."
  (cond ((every #'empty-pattern patterns)
	 (unless (some #'null patterns) ; is this a "hard" end or need a dots ?
	   (setq ordered (append ordered '(dots))))
	 (cons (list ordered constraints) fail)); returns a stream.
	; a pound at the end only if only dots/pound remain.
	((every #'(lambda (p) (or (equal p '(dots)) (equal p '(pound))))
		patterns)
	 (setq ordered (append ordered '(pound)))
	 (cons (list ordered constraints) fail))
	((some #'null patterns)                 ; if some but not all are empty
	 (funcall fail))                        ; we have reached the end of
	                                        ; one pattern too soon.
	((empty min-stream)
	 (funcall fail))     ; no more min to try.
	; if we have at least a pound and only dots or pounds in front,
	; min-stream is just {}, we don't have to fail.
	; We add a pound in front of ordered and go down our way.
	; but if there is no pound, we need to try another solution.
	((and (null (gen-first min-stream))
	      (not  (some #'poundp patterns)))
	 (funcall fail))
	(t
	 (let ((the-min (gen-first min-stream)))
	   (let* ((next-patterns (remove-min-pattern patterns the-min))
		  (next-necessary (necessary-mins next-patterns))
		  (next-possible  (possible-mins next-patterns))
		  (next-min-stream
		   (stream-of-mins next-necessary next-possible))
		  (next-constraints
		   (if (cdr the-min)
		       (cons the-min constraints)
		       constraints))
		  (next-ordered
		   (cond ((and (null the-min) (some #'poundp patterns))
			  (append ordered (list 'pound)))
			 ((null necessary)
			  (if (or (cdr the-min) (mergeablep (car the-min)))
			      (setq the-min (mark-mergeable (car the-min)))
			      (setq the-min (car the-min)))
			  (if (some #'poundp patterns)
			      (append ordered (list the-min))
			      (append ordered (list 'dots the-min))))
			 (t
			  (if (or (cdr the-min) (mergeablep (car the-min)))
			      (setq the-min (mark-mergeable (car the-min)))
			      (setq the-min (car the-min)))
			  (append ordered (list the-min))))))

	     (unify-patterns
	      next-patterns
	      next-necessary
	      next-possible
	      next-min-stream
	      next-ordered
	      next-constraints
	      ; find next-fail
	      (freeze
	       (unify-patterns patterns necessary possible
			       (next min-stream)
			       ordered constraints fail))))))))


;; -----------------------------------------------------------------------
(provide "$fug5/pattern")
;; -----------------------------------------------------------------------
