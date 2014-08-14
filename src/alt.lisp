;;; -*- Mode:Lisp; Syntax:Common-Lisp; Package: FUG5 -*-
;;; -----------------------------------------------------------------------
;;; File:         alt.l
;;; Description:  Unification of disjunctions
;;; Author:       Michael Elhadad
;;; Created:      22 May 1990
;;; Modified:     find-good-branches returns a list of all branches
;;;               compatible with index - allows for none in index.
;;;               allows for typed constants in index.
;;;               15 Jun 90: Added pair argument.
;;;               02 Jul 90: Added path2 arg.
;;;               22 Feb 91: Allowed list of bk-class instead of only 1.
;;;               28 Jul 91: New syntax for alts (:x and order free).
;;;                          Add IGNORE and WAIT (freeze).
;;;               11 Aug 91: Add trace fro IGNORE and WAIT.
;;;               29 Oct 91: Fix trace-format calls.  Add *trace-wait*.
;;;               09 Dec 91: Added after-wait in alt-unify and descendants.
;;;               02 Jan 91: Fixed indexed to accept random order too.
;;;               03 Jan 91: Untrace alt when freezing.
;;;               20 Oct 92: Added level to trace-format and trace-demo
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
(format t "Alt unifier...~%")

;; ------------------------------------------------------------
;; Functions to deal with NON-deterministic unifications
;; These functions modify their argument FAIL each time a choice
;; is made.
;; ------------------------------------------------------------


;; ------------------------------------------------------------
;; UTILITIES to manipulate alt expressions 
;; Kept for old syntax only.  Not useful anymore.
;; ------------------------------------------------------------

(defun alt-trace-expr (sexpr)
  (or (leaf-p sexpr)
      (and (consp sexpr) (eq (car sexpr) 'trace))))

(defun alt-trace-flag (sexpr)
  (if (leaf-p sexpr)
      sexpr
    (car (last sexpr))))

(defun alt-index-expr (sexpr)
  (and (consp sexpr) (eq (car sexpr) 'index)))

(defun alt-index-attr (sexpr)
  (car (last sexpr)))

(defun alt-demo-expr (sexpr)
  (and (consp sexpr) (eq (car sexpr) 'demo-message)))

(defun alt-bk-class-expr (sexpr)
  (and (consp sexpr) (eq (car sexpr) 'bk-class)))

(defun alt-bk-class-class (sexpr)
  (let ((expr (last sexpr)))
    (if (symbolp (car expr))
      expr
      (car (last sexpr)))))


(proclaim '(inline alt-trace-expr alt-trace-flag 
	    alt-index-expr alt-index-attr 
	    alt-demo-expr
	    alt-bk-class-expr alt-bk-class-class))


;; Utility to display an underlined demo message when tracing
(defun show-demo-msg (demo traced frame)
  (when demo
    (trace-demo traced frame 12 "")
    (trace-demo traced frame 12 demo)
    (trace-demo traced frame 12 "~a" 
		(make-sequence 'string (length demo)
			       :initial-element #\-))))

;; ---------------------------------------------------------------------
;;  BRANCHES:   takes an alt pair and return the branches, possibly
;;              a tracing flag and an index declaration 
;;              (9 values: branches traced indexed demo bk-class order wait
;;              ignore-unless ignore-when)
;;  OPTIONS:    same as branches for an opt pair. returns 3 values.
;; ---------------------------------------------------------------------

;; NEW Syntax of alt-pair is (:x notation is prefered, old one kept for
;; compatibility).  [Jul 28, 1991]
;; (alt { traceflag | (trace on traceflag) | (:trace flag) }
;;      { (index on path) | (:index path)}
;;      { (demo str) | (:demo str)}
;;      { (bk-class class) | (:bk-class class) }
;;      { (:order {:random | :sequential}) }
;;      { (:wait list) }
;;      { (:ignore list) }
;;      ( fd1 ... fdn ))
;; NOW ALLOW ANNOTATIONS IN ANY ORDER.

(defun find-alt-annotation (key alt)
  "Extract the value val of annotation (key val) in an alt pair.
   Return nil if no annotation for key is found."
  (let ((pair (safe-assoc key alt)))
    (if pair
      (second pair)
      ;; Deal with old syntax for compatibility
      (case key
	(:trace (alt-trace-flag (safe-assoc 'trace alt)))
	(:index (alt-index-attr (safe-assoc 'index alt)))
	(:demo  (safe-second (safe-assoc 'demo alt)))
	(:bk-class (alt-bk-class-class (safe-assoc 'bk-class alt)))
	(otherwise nil)))))
		

(defun branches (alt-pair)
  (setq alt-pair (cdr alt-pair))
  (let ((traced nil) (indexed nil) (demo nil) (bk-class nil)
	(order nil) (wait nil) (ignore-unless nil) (ignore-when nil)
	(branches nil))

    ;; First special case of trace that can be a single atom
    (when (alt-trace-expr (car alt-pair))
      (setq traced (alt-trace-flag (pop alt-pair))))

    ;; Now all annotations are in pair formats
    (unless traced (setq traced (find-alt-annotation :trace alt-pair)))
    (setq indexed (find-alt-annotation :index alt-pair))
    (setq demo (find-alt-annotation :demo alt-pair))
    (setq bk-class (find-alt-annotation :bk-class alt-pair))
    (setq order (find-alt-annotation :order alt-pair))
    (setq wait (normalize-wait (find-alt-annotation :wait alt-pair)))
    (setq ignore-unless (find-alt-annotation :ignore-unless alt-pair))
    (setq ignore-when (find-alt-annotation :ignore-when alt-pair))

    ;; branches must be last
    (setq branches (car (last alt-pair)))

    ;; Normalize values of annotations
    (if (path-p indexed) (setq indexed (path-l indexed)))
    (unless (listp indexed) (setq indexed (list indexed)))
    (when (stringp demo) (setq demo (format nil demo)))
    (unless (listp wait) (setq wait (list wait)))
    (unless (eq order :random) (setq order :sequential))

    (values branches traced indexed demo bk-class
	    order wait ignore-unless ignore-when)))

;; Syntax of opt-pair is:
;; (opt { trace } { (bk-class class) } fd)
(defun option (opt-pair)
  (setq opt-pair (cdr opt-pair))
  (let ((trace nil) (bk-class nil) (fd nil))
    (when (alt-trace-expr (car opt-pair))
      (setq trace (alt-trace-flag (pop opt-pair))))
    (if (alt-bk-class-expr (car opt-pair))
	(setq bk-class (alt-bk-class-class (pop opt-pair))
	      fd (car opt-pair))
      (if (null opt-pair)
	  (setq fd trace
		trace nil)
	(setq fd (car opt-pair))))
    (values fd trace bk-class)))


;; ---------------------------------------------------------------------
;; FIND-GOOD-BRANCHES: filter out branches that do not match the key of an
;; index:  hence the constraint on the index of an alt:
;; the value of an index can be a pointer - but it MUST NOT point outside
;; of the branch or within a disjunction in the branch.
;; ---------------------------------------------------------------------

(defun find-good-branches (key index branches)
  (remove-if-not
   #'(lambda (branch)
       (let ((val-in-branch (top-gdp branch (make-path :l index))))
	 (or (null val-in-branch)
	     (and key (eq 'given val-in-branch))
	     (eq 'any val-in-branch)
	     (subsume val-in-branch key)
	     (and (if *use-given*
		      (not (underp val-in-branch))
		    t)
		  (subsume key val-in-branch)))))
   branches))
      


;; ---------------------------------------------------------------------
;; OPT-UNIFY
;; ---------------------------------------------------------------------

(defun opt-unify (fd1 fd2 path1 path2 frame fail success &key (pair :unknown))
  (multiple-value-bind (option flag bk-class) (option (car fd2))
    (when (trace-enabled flag) (handle-trace flag frame t))
    (trace-format flag frame 10
     "Trying with option ~s at level ~s" flag path1)
    (backtrack frame new-frame bk-class flag
      (unify fd1 option path1 path2 new-frame
	     (make-failure fail
	       (when (trace-enabled flag) 
		 (handle-trace flag frame t)
		 (trace-format flag frame 10
		   "Trying without option ~s at level ~s~%"
		   flag path1))
	       (unify fd1 (cdr fd2) path1 path2 frame fail success 
		      :pair pair))
	     #'(lambda (fd fail frame)
		 (when (trace-enabled flag) 
		   (trace-format flag frame 10
				 "Success with option ~s at level ~s~%"
				 flag path1)
		   (handle-trace flag frame t))
		 (unify fd (cdr fd2) path1 path2 frame fail success 
			:pair pair))
	     :pair pair))))

;; ---------------------------------------------------------------------
;; ALT-UNIFY
;; ---------------------------------------------------------------------

;; alt-unify parses the alt construct and based on annotations, dispatches
;; to the appropriate specialized function.

(defun alt-unify (fd1 fd2 path1 path2 frame fail success 
		      &key indexed-given order-given 
		           force-wait after-wait (pair :unknown)
		      &aux key)
  (declare (special *input*))
  (multiple-value-bind (branches traced indexed demo bk-class 
				 order wait ignore-unless ignore-when) 
                       (branches (car fd2))
    ;; Handle tracing messages
    (when (trace-enabled traced)
      (show-demo-msg demo traced frame)
      (push traced (frame-trace-flags frame))
      (incf (frame-trace-level frame)))

    ;; indexed and order can be given as arg or from annotations. arg
    ;; prevails.
    ;; force-wait t means don't consider the wait annotation.
    (when order-given (setq order order-given))
    (when force-wait (setq wait nil))
    (unless indexed (setq indexed indexed-given))
    (when indexed 
      (setq key (gdp *input* (make-path :l (append (path-l path1) indexed)))))

    ;; Dispatch based on annotations to specialized function
    (cond 
     ((null branches) 
      (error "An alt must have at least one branch"))

     ;; NOTE: WAIT has priority on IGNORE
     ;; WAIT annotation: if need to freeze, push on agenda and continue
     ((not (check-wait wait path2))
      (let ((id (add-agenda frame (car fd2) wait path1 path2 after-wait
			    indexed order ignore-unless ignore-when pair)))
	(trace-format (or *trace-wait* (trace-enabled traced)) frame 15
		      "Freezing alt ~s: waiting for ~s [agenda ~s]" 
		      traced wait id)
	(when (trace-enabled traced)
	  (pop (frame-trace-flags frame))
	  (decf (frame-trace-level frame)))
	(unify fd1 (cdr fd2) path1 path2 frame fail success :pair pair)))

     ;; IGNORE annotations
     ((check-ignore-unless ignore-unless path1 path2)
      (trace-format traced frame 15
		    "Ignoring alt ~s: ignore-unless matches" traced)
      (unify fd1 (cdr fd2) path1 path2 frame fail success :pair pair))
     ((check-ignore-when ignore-when path1 path2)
      (trace-format traced frame 15
		    "Ignoring alt ~s: ignore-when matches" traced)
      (unify fd1 (cdr fd2) path1 path2 frame fail success :pair pair))

     ;; INDEX annotation
     ((not (member key '(nil any given)))
      (alt-unify-indexed 
       fd1 (cdr fd2) path1 path2 bk-class frame fail success after-wait 
       traced key branches indexed :random order :pair pair))

     ;; OK, proceed
     (t (when (and indexed demo)
	  (trace-demo traced frame 5
		      "Key not specified in input.  Need to search"))
	(trace-format (and (trace-enabled traced) indexed) frame 5
		      "No value given in input for index ~s - No jump" 
		      indexed)
	(if (eq order :random)
	  (ralt-unify-simple 
	   fd1 (cdr fd2) path1 path2 bk-class frame fail success after-wait 
	   traced (copy-list branches) 1 branches :pair pair)
	  (alt-unify-simple 
	   fd1 (cdr fd2) path1 path2 bk-class frame fail success after-wait 
	   traced branches 1 nil :pair pair))))))




;; ---------------------------------------------------------------------
;; ALT-UNIFY-SIMPLE: alt is not indexed. Traverse each branch, and backtrack
;;                   to the next branch if fail. When no more branch, fail.
;;                   (change the fail argument when trying a branch)
;; NOTE: orig-branches is just used for tracing purposes when called from
;; alt-unify-indexed to identify the current branch in the textual
;; representation of the grammar.
;; ---------------------------------------------------------------------

(defun alt-unify-simple (fd1 fd2 path1 path2 bk-class frame fail success after-wait 
			 traced branches branch-number orig-branches
			 &key (pair :unknown))
  ; eat the tracing flags between branches
  (when (and (tracing-flag (car branches))
	     (trace-enabled (car branches)))
    (handle-trace (pop branches) frame))
  (cond ((null branches)
	 (when (trace-enabled traced)
	   (trace-format traced frame 30
			 "Fail in alt ~s at level ~s~%" traced path1)
	   (handle-trace traced frame t))
	 (*fail* fail frame *same* path2 pair))
	(t
	  (trace-format traced frame 10
			"Entering alt ~s -- Branch #~s" 
			traced 
			(if orig-branches
			    (1+ (position (car branches) 
					  orig-branches
					  :test #'equalp))
			  branch-number))
	  (backtrack frame new-frame bk-class traced
	    (unify fd1 (car branches) path1 path2 new-frame
		   (make-failure fail
		     (alt-unify-simple 
		      fd1 fd2 path1 path2 bk-class 
		      frame fail success after-wait traced
		      (cdr branches) (1+ branch-number) orig-branches
		      :pair pair))
		   #'(lambda (fd fail frame)
		       (when (trace-enabled traced)
			 (trace-format traced frame 10
				       "Success with branch ~s in alt ~s~%"
				       branch-number traced)
			 (handle-trace traced frame t))
		       (when after-wait (funcall after-wait))
		       (unify fd fd2 path1 path2 frame fail success
			      :pair pair))
		   :pair pair)))))

;; ---------------------------------------------------------------------
;; ALT-UNIFY-INDEXED: alt is indexed.
;; Prune the set of branches to retain only those compatible with index.
;; If only one branch, unification is deterministic, just jump to the right
;; branch and use it. 
;; If more than one, search on restricted set.
;; ---------------------------------------------------------------------

(defun alt-unify-indexed (fd1 fd2 path1 path2 bk-class frame fail success after-wait 
			      traced key branches indexed
			      &key random (pair :unknown))
  (let ((good-branches (find-good-branches key indexed branches)))
    (cond ((null good-branches) 
	   (trace-format traced frame 30
			 "fail on indexed alt ~s: no branch in ~
			the grammar for ~s~%" traced key)
	   (*fail* fail frame *same* path2 pair))
	  ((= 1 (length good-branches))
	   (trace-format 
	    traced frame 10
	    "Entering alt ~s -- Jump indexed to branch #~s: ~
             ~s matches input ~s"
	    traced 
	    (1+ (position (car good-branches) branches :test #'equalp))
	    (top-gdp (car good-branches) (make-path :l indexed))
	    key)
	   (unify fd1 (car good-branches) path1 path2 frame fail 
		  #'(lambda (fd fail frame)
		      (when traced
			(trace-format 
			 traced frame 10
			 "Success with branch #~s ~s in alt ~s~%"
			 (1+ (position (car good-branches) branches 
				       :test #'equalp))
			 key
			 traced)
			(handle-trace traced frame t))
		      (when after-wait (funcall after-wait))
		      (unify fd fd2 path1 path2 frame fail success 
			     :pair pair))
		  :pair pair))
	  (t
	   (if traced
	       (trace-format 
		traced frame 5
		"Entering indexed ~a ~s -- Trying with ~s branches out of ~s"
		(if random "Ralt" "alt")
		traced (length good-branches) (length branches)))
	   (if (eq random :random)
	       (ralt-unify-simple fd1 fd2 path1 path2 bk-class frame 
				  fail success after-wait 
				  traced good-branches 1 branches
				  :pair pair)
	     (alt-unify-simple 
	      fd1 fd2 path1 path2 bk-class frame fail success after-wait 
	      traced good-branches 1 branches :pair pair))))))





;; -----------------------------------------------------------------------
(provide "$fug5/alt")
;; -----------------------------------------------------------------------
