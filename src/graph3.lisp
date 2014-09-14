;;; -*- Mode:Lisp; Syntax:Common-Lisp; Package:FUG5 -*-
;;; -----------------------------------------------------------------------
;;; File:         GRAPH.L
;;; Description:  Unification of graphs represented as fds.
;;;               With undo and NO stack and SUCCESS and GIVEN.
;;;               and unifs not at top level.
;;;               With FREEZE and IGNORE.
;;; Author:       Michael Elhadad
;;; Created:      02-Nov-88
;;; Modified:     28 Nov 89
;;;               30 Apr 90 - Defined *special-attributes*
;;;               07 May 90 - Implemented fset-unify.
;;;               10 May 90 - Made path a special type.
;;;               21 May 90 - added ralt
;;;               04 Jun 90 - allow for paths in attribute slot (equations)
;;;               21 Jun 90 - defined *use-given*
;;;               22 Jun 90 - defined external
;;;               02 Jul 90 - added path2 arg to all unify fctns.
;;;               20 Feb 91 - used equality instead of equalp.
;;;               02 May 91 - allow for (pattern given) to work
;;;               28 Jul 91 - Add WAIT and FREEZE (call check-agenda
;;;                           instead of alt-unify).
;;;               13 Aug 91 - Use unify-lattice
;;;               18 Aug 91 - Use attr-p instead of symbolp
;;;               19 Sep 91 - add *use-any*
;;;               13 Nov 91 - add pair1 arg to leaf12 to use :i flag.
;;;                           changed all calls to *fail*
;;;               15 Nov 91 - changed unify-path so that ((a {b}) (b 1))
;;;                           with ((a {c})) gives ((c {b}) ...) instead of
;;;                           ((c {a}) (a {b}) ...)
;;;               25 Nov 91 - made of cset a parameter (used global *cset*)
;;;               05 Dec 91 - made of cset a regular special attribute.
;;;               18 Dec 91 - added *use-wait*
;;;               12 Jan 92 - fixed unify-path to avoid loop with triangles.
;;;               20 Oct 92: Added level to trace-format
;;;               08 Feb 93: Add (pattern given) again.
;;;               01 Dec 95: Add remove handling (CHARLES BRENDAN)
;;;               11 Jan 96: Update from Charles
;;;               15 Jan 96: Make end-of-list compatible with none.
;;;               15 Jan 96: Add new fd-to-list with end-of-list
;;;               21 Jan 96: Fix bug in update-pairs/remove
;;; Package:      FUG5
;;; Macros:       leaf-p, fd-boundp
;;; Status:       Experimental
;;; -----------------------------------------------------------------------
;;;
;;; FUF - a functional unification-based text generation system. (Ver. 5.3')
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
(format t "Graph unifier with REMOVE...~%")

;; ----------
;; EMPTY-FD : an empty contains only nils and nones but is not none.
;; ----------
(defun empty-fd (fd)
  (declare (special *cset*))
  (labels ((empty-or-none (fd)
	     (cond ((null fd) t)
		   ((eq fd 'none) t)
		   ((leaf-p fd) (return-from empty-fd nil))
		   ((path-p fd) (return-from empty-fd nil))
		   ((consp fd)
		    (every #'(lambda (pair)
			       (cond
				((leaf-p pair) t)
				((path-p pair) t)
				((path-p (car pair))
				 (return-from empty-fd nil))
				((path-p (safe-second pair))
				 (return-from empty-fd nil))
				((member (car pair) '(fset pattern)) t)
				((eq (car pair) *cset*) t)
				((member (car pair) *special-attributes*)
				 (return-from empty-fd nil))
				(t (empty-or-none (safe-second pair)))))
			   fd))
		   (t (error "Unknown type for fd: ~s" fd)))))
	  (if (eq fd 'none) nil (empty-or-none fd))))



;; ----------
;; COPY-SPECIAL: return a fresh copy of a special pair appearing at level path
;; ----------
;; A special pair is a special attribute, or a path value.

(defun copy-special-pair (pair &optional path)
  (let ((att (car pair))
	(val (second pair))
	(status (third pair)))
    (list att (copy-special val att path) status)))

(defun copy-special (val &optional att path)
  (cond ((leaf-p val) val)
	((path-p val)
	 (absolute-path (copy-path val)
			(path-extend path att)))
	((member att '(pattern fset))
	 (copy-list val))
	((and (member att *special-attributes*)
	      (functionp (special-copier-function att)))
	 (funcall (special-copier-function att) val))
	(t (copy-tree val))))


;; ------------------------------------------------------------
;; UNIFICATION OF LEAVES
;; ------------------------------------------------------------
;; Called when BOTH fd1 and fd2 are atoms (also if fd1 is empty).
;; fd2 is not external (handled in leaf2).
;; The table is the following:
;;  fd2 | NIL  ANY  NONE  GIVEN  X1
;; fd1  +------------------------------------
;; NIL  | NIL  ANY  NONE  FAIL   X1
;; ANY  | ANY  ANY  FAIL  ANY    X1
;; NONE | NONE FAIL NONE  FAIL   FAIL
;; X2   | X2   X2   FAIL  X2     u(X1,X2)  <-- use type hierarchy.

;; NOTE: this call does not update *input*.
(defun unify-leaf12 (fd1 fd2 path1 path2 frame fail success pair1 &aux result)
  (declare (special *input*))
  (cond
   ((empty-fd fd1)
    (cond
     ((eq 'given fd2)
      (if *use-given*
	  (*fail* fail frame path1 path2 pair1
		  "Fail in trying NIL with GIVEN at level ~s" path1)
	(funcall success nil fail frame)))
     ((underp fd2)
      (if *use-given*
	  (if (under-symbol fd2)
	      (*fail* fail frame path1 path2 pair1
		      "Fail in trying NIL with ~s at level ~s" fd2 path1)
	    (funcall success (under-symbol fd2) fail frame))
	(funcall success (under-symbol fd2) fail frame)))
     (t (funcall success fd2 fail frame))))
   ((eq fd1 'any)
    (cond ((eq fd2 'none)
	   (*fail* fail frame path1 path2 pair1
		   "Fail in trying ANY with NONE at level ~s" path1))
	  ((member fd2 '(nil any given))
	   (funcall success 'any fail frame))
	  (t (funcall success fd2 fail frame))))
   ((eq fd1 'end-of-list)
    (if (eq fd2 'none)
        (funcall success 'end-of-list fail frame)
        (error "Encountered End-Of-List prematurely ")))
   ((eq fd1 'none)
    (cond ((member fd2 '(nil none))
	   (funcall success 'none fail frame))
	  (t (*fail* fail frame path1 path2 pair1
		     "Fail in trying NONE with ~s at level ~s" fd2 path1))))
   ((eq fd2 'remove)
    (funcall success nil fail frame))
   (t
    (cond ((member fd2 '(nil any given))
	   (funcall success fd1 fail frame))
	  ((eq fd2 'none)
	   (*fail* fail frame path1 path2 pair1
		   "Fail in trying ~s with NONE at level ~s" fd1 path1))
	  ((subsume fd2 fd1) (funcall success fd1 fail frame))
	  ((and (if *use-given* (not (underp fd2)) t)
		(setf result (unify-lattice fd1 fd2)))
	   (funcall success result fail frame))
	  (t (*fail* fail frame path1 path2 pair1
		     "Fail in trying ~s with ~s at level ~s"
		     fd1 fd2 path1))))))


(defun unify-leaf2 (fd1 fd2 path1 path2 frame fail success pair1)
  ;; Called when:
  ;; - fd1 is anything, fd2 is a non null leaf.
  ;; - pair1 is either :unknown or the pair containing fd1.
  ;; RESULT:  updates  *input* physically if needed.
  (declare (special *input*))
  (cond
   ((member (car (path-last path1)) *special-attributes*)
    (*fail* fail frame path1 path2 pair1
	    "Fail in trying special ~s with non-special ~s at level ~s"
	    fd1 fd2 path1))
   ((externalp fd2)
    (let ((value (call-external fd2 path2 frame)))
      (if (eq value :fail)
	  (*fail* fail frame path1 path2 pair1
		  "External function ~s failed at level ~s"
		  fd2 path2)
	(unify fd1 value path1 path2 frame
	       fail
	       #'(lambda (fd fail frame)
		   (trace-format (frame-trace-flags frame) frame 5
				 "Success with external constraint.")
		   (funcall success fd fail frame))
	       :pair pair1))))
   ((or (leaf-p fd1) (empty-fd fd1))
    (unify-leaf12 fd1 fd2 path1 path2 frame fail
		  #'(lambda (fd fail frame)
                      (if (and (consp pair1) (equal (second pair1) fd))
                          (trace-format
                           (frame-trace-flags frame) frame 0
                           "Passing with ~s at level ~s"
                           pair1 path1)
                          (trace-format
                           (frame-trace-flags frame) frame 0
                           "Updating ~s with ~s at level ~s"
                           pair1 fd path1))
		      (update-pair pair1 fd path1 frame)
		      (funcall success fd fail frame))
		  pair1))
   ((path-p fd1)
    (let* ((new-path (absolute-path fd1 path1))
	   (new-pair (gdpp *input* new-path frame)))
      (unify (safe-second new-pair) fd2 new-path path2 frame fail success
	     :pair new-pair)))
   ((consp fd1)
    (cond ((or (eq fd2 'any) (eq fd2 'given))
	   (funcall success fd1 fail frame))
	  ((eq fd2 'remove)
	   (cond ((equal '(car) (path-last path2))
		  (update-pair pair1 (path-append (path-butlast path2)
						  '{cdr car})
			       path1 frame))
		 ((equal '(cdr) (path-last path2))
		  (update-pair pair1 (path-append (path-butlast path2)
						  '{cdr cdr})
			       path1 frame))
		 (t  (update-pair pair1 nil path1 frame)))
	   (funcall success fd1 fail frame))
	  (t
	   (*fail* fail frame path1 path2 pair1
		   "Fail in trying ~s with ~s at level ~s" fd1 fd2 path1))))
   (t (error "Unknown type for fd1: ~s" fd1))))



(defun unify-leaf1 (fd1 fd2 path1 path2 frame fail success pair1)
  ;; Called when:
  ;; - fd1 is a leaf.
  ;; - fd2 is a consp
  ;; - fd2 starts with a non disjunctive pair
  ;;   therefore (car fd2) is a safe first arg. for unify.
  ;;   It may however be a special attribute or a path!
  ;; - pair1 is either :unknown or the pair containing fd1.
  ;; RESULT: *input* is always physically updated IN THIS FUNCTION
  ;; (that is, the pair containing fd1 is updated).
  ;; NOTE: need to recurse on fd2 anyway - as it may contain disjunctions
  ;;       and equations.
  (declare (special *input*))

  ;; Handle ANY special case.
  (when (eq fd1 'any)
    (push (make-test :test '(any-p path) :path path1)
	  (frame-tests frame))
    (setf fd1 nil))

  (let* ((new-path1 (cond
		     ((path-p (caar fd2))
		      (absolute-path (caar fd2) path2))
		     (t (path-extend path1 (caar fd2)))))
	 (new-path2 (cond
		     ((path-p (caar fd2))
		      (absolute-path (caar fd2) path2))
		     (t (path-extend path2 (caar fd2)))))
	 (new-pair (if (and fd1 (attr-p (caar fd2)))
		       'none
		     (gdpp *input* new-path1 frame)))
	 (att (if (consp new-pair)
		  (car new-pair)
		(car (path-last new-path1)))))
    (if (eq pair1 :unknown)
	(setf pair1 (gdpp *input* path1 frame)))
    (cond
     ((and (eq att 'fset)    ;; none and ((fset (l))) are ok.
	   (eq fd1 'none))
      (unify (safe-second pair1) (cdr fd2)
	     path1 path2 frame fail success :pair pair1))

     ((member att *special-attributes*)
      (if fd1
	  (*fail* fail frame path1 path2 pair1
		  "Fail in trying ~s with special ~s" fd1 (car fd2))
	(progn
	  (update-pair new-pair
		       (copy-special (safe-second (car fd2)) att path2)
		       new-path1 frame)
	  (trace-format (frame-trace-flags frame) frame 0
			"Enriching input with ~s at level ~s"
			new-pair path2)
	  (unify (safe-second pair1) (cdr fd2)
		 path1 path2 frame fail success :pair pair1))))

     (t
      (unify (safe-second new-pair) (cadar fd2) new-path1 new-path2
	     frame fail
	     #'(lambda (fd fail frame)
		 (declare (ignore fd))
		 (unify (safe-second pair1) (cdr fd2)
			path1 path2 frame fail success
			:pair pair1))
	     :pair new-pair)))))



;;; --------------------------------------------------
;;; UNIFY
;;; --------------------------------------------------

(defun unify (fd1 fd2 path1 path2 frame fail success &key (pair :unknown))
  ;; Pair1 is either :unknown or the pair containing fd1.
  (declare (special *input*))
  (cond
   ((null fd2)
    (funcall success fd1 fail frame))
   ((leaf-p fd2)
    (unify-leaf2 fd1 fd2 path1 path2 frame fail success pair))
   ((path-p fd2)
    (unify-path fd1 fd2 path1 path2 frame fail success pair))
   ;; fd2 is a tracing flag
   ((tracing-flag (car fd2))
    (when (trace-enabled (car fd2))
      (handle-trace (car fd2) frame))
    (unify fd1 (cdr fd2) path1 path2 frame fail success :pair pair))
   ;; (car fd2) is a pair
   ((consp (car fd2))
    (case (caar fd2)
      ;; These atts can only appear in a grammar - never in fd1
      (test (test-unify fd1 fd2 path1 path2 frame fail success :pair pair))
      (control
       (control-unify fd1 fd2 path1 path2 frame fail success :pair pair))
      (opt (opt-unify fd1 fd2 path1 path2 frame fail success :pair pair))
      ;; CHECK THE AGENDA HERE AND AWAKE THE FROZEN THEN PROCEED TO ALT-UNIFY
      (alt (check-agenda fd1 fd2 path1 path2 frame fail success :pair pair))
      (ralt (check-agenda fd1 fd2 path1 path2 frame fail success
		       :pair pair :order-given :random))
      ;; Fd2 starts with a non-disjunctive pair:
      ;; ---------------------------------------
      (t
       (cond
	((leaf-p fd1)
	 (unify-leaf1 fd1 fd2 path1 path2 frame fail success pair))
	;; This is why we have an argument path2:
	;; you want relative paths to be relative to the position in the
	;; grammar not to the position in the input.
	((path-p fd1)
	 (let* ((new-path1 (absolute-path fd1 path1))
		(new-pair (gdpp *input* new-path1 frame)))
	   (unify (safe-second new-pair) fd2 new-path1 path2
		  frame fail success :pair new-pair)))
	((consp (car fd1))
	 (unify-pairs fd1 fd2 path1 path2 frame fail success pair))
	(t (error "Unknown type: ~s (Ill-formed input)" fd1))))))
   (t (error "Unknown type: ~s (Ill-formed grammar)" fd2))))



(defun unify-pairs (fd1 fd2 path1 path2 frame fail success super-pair)
  ;; Called when:
  ;; fd1 starts with a pair
  ;; fd2 starts with a non-disjunctive pair
  (declare (special *input*))
  (let* ((pa (if (path-p (caar fd2)) (absolute-path (caar fd2) path2)))
	 (endpath (if pa (path-extend (path-butlast pa) 'cdr)))
	 (endlist? (if pa (equal '(cdr end-of-list :i) (gdp *input* endpath))))
	 (pat (if pa (path-butlast (path-butlast pa))))
	 (temp2 (if pat (gdpp *input* pat frame)))
	 (pair1 (cond ((attr-p (caar fd2))
		       (safe-assoc (caar fd2) fd1))
		      ((path-p (caar fd2))
		       (gdpp *input* (absolute-path (caar fd2) path2)
			     frame))
		      (t (error "A pair must be of the form: ~% ~
                                 (symbol value) or (path value).~% ~
                                 Offending pair: ~s at level ~s"
				(car fd2) path2))))
	 (pair2 (car fd2))
	 (att  (if (consp pair1) (car pair1) (caar fd2)))
	 (val1 (safe-second pair1))
	 (val2 (safe-second pair2))
	 (fset (find-fset fd1)))
    ;(format t "~%P1:  ~A    P2:  ~A     Att:  ~A~%" pair1 pair2 att)
    ;(format t "~%Val1:  ~A    Val2:  ~A~%" val1 val2)
    (cond

     ((and (null pair1) (not (equal temp2 '(cdr end-of-list :i))))
      ;; there is NO pair (att. val) in the input : enrich it
      ;; ----------------------------------------------------
      ;; NOTE: if val2 is ANY, we don't fail NOW, since a value
      ;; can be added later. We check for ANY in DETERMINE
      ;; (at the end), and fail there if necessary.
      ;; However, if you don't bother, just set the flag
      ;; *any-at-unification* to T (will work as GIVEN).
      ;; if val2 is NONE, we need to enrich fd1 so that no
      ;; other value wil ever be added for att.
      (cond

       ;; FSET: if fd1 contains an fset, enrich with none if att not in fset.
       ((and fset (not (member att fset)))
	(enrich fd1 (list att 'none) frame)
	(unify-pairs fd1 fd2 path1 path2 frame fail success super-pair))

       ;; PAIR2 is an FSET: check that there is no existing invalid att.
       ((eq att 'fset)
	(fset-unify fd1 fd2 pair1 pair2 path1 path2 frame fail success
		    :pair super-pair))

       ;; found an rset:  recursive unification
       ((and (eq att 'rset) (path-p val2))
	 (format t "~%Recursive Unification at ~A.~%" val2)
	 (unify fd1 (get-fd *u-grammar*) val2 val2 frame fail success
		:pair super-pair))

       ;; If val2 is given or any, fail.
       ((and
	 ;; Commented out May 2, 1991 - Allow (pattern any) to work
	 ;; (not (member att *special-attributes*))
	 (or (and *any-at-unification* (eq 'any val2))
	     (and *use-given* (eq 'given val2))))
	(*fail* fail frame path1 path2 pair1
		"Fail in trying ~s with ~s at level ~s"
		val1 val2 (path-extend path1 att)))

       ;; If val2 is a path - check for type conflict
       ((and (path-p val2)
	     (or (member att *special-attributes*)
		 (member (car (path-last val2)) *special-attributes*))
	     (not (eq (car (path-last val2)) att)))
	(*fail* fail frame path1 path2 pair1
		"Cannot unify special with non-special:  ~s at level ~s~%~
                ****** Probably ERROR in grammar *****"
		pair2 path1))

       ;; If val2 is an `atomic' value (including a path), add a copy of
       ;; the pair
       ((or (path-p val2)
	    (member att *special-attributes*))
	(let ((new-pair (copy-special-pair pair2 path2)))
	  (trace-format (frame-trace-flags frame) frame 0
			"Enriching input with ~s at level ~s"
			new-pair path2)
	  (enrich fd1 new-pair frame))
	(unify fd1 (cdr fd2) path1 path2 frame fail success :pair super-pair))

       ;; val2 is a complex fd: recurse on val2.
       (t
	(let ((new-pair (list att nil))
	      (new-path2 (if (attr-p (car pair2))
			     (path-extend path2 att)
			   (absolute-path (car pair2) path2))))
	  (if (equal (car pair2) 'remove)
	      (degrade fd1 new-pair frame)
	      (enrich fd1 new-pair frame))
	  (unify nil val2 (path-extend path1 att) new-path2 frame fail
		 #'(lambda (fd fail frame)
		     (declare (ignore fd))
		     (unify fd1 (cdr fd2) path1 path2
			    frame fail success :pair super-pair))
		 :pair new-pair)))))

     ;; there is already a pair (att. val) in the input.
     ;; ------------------------------------------------
     ;; special cases : cset, fset, pattern, paths and special atts.

     (t
      (cond

       ;; before doing hard work with paths, try simple stuff
       ((equality val1 val2)
	 (unify fd1 (cdr fd2) path1 path2 frame fail success :pair super-pair))

       ;; Each time check that pair1 is not none (it is a consp)

       ;; pattern
       ((and (consp pair1) (eq att 'pattern))
	(cond
	 ((and (null val1)
	       (or (and *use-given* (eq val2 'given))
		   (and *any-at-unification* (eq val2 'any))))
	  (*fail* fail frame path1 path2 pair1
		  "Fail in trying ~s with ~s at level ~s"
		  val1 val2 (path-extend path1 att)))
	 (t
	  (pattern-unify fd1 fd2 pair1 pair2 path1 path2 frame fail success
			 :pair super-pair))))

       ;; fset: fail if some invalid att is already here.
       ;; So, first unify all non-fset atts with none.
       ;; Then, unif fset1 fset2 is intersection of fset1 and fset2.
       ;; If result is nil (no constituent acceptable), the parent must
       ;; become NONE.
       ((and (consp pair1) (eq att 'fset))
	(when (null val1)
	  (update-pair pair1 val2 (path-extend path1 'fset) frame))
	(fset-unify fd1 fd2 pair1 pair2 path1 path2 frame fail success
		    :pair super-pair))

       ;; user-defined special attribute
       ((and (consp pair1) (member att *special-attributes*))
	(special-unify fd1 fd2 pair1 pair2 path1 path2 frame fail success
		       :pair super-pair))

       ;; remove: remove the attribute from fd1
       ((and (eq val2 'remove) (attr-p (caar fd2)))
          (update-pair pair1 nil (path-extend path1 att) frame)
          (unify fd1 (rest fd2) path1 path2 frame fail
               #'(lambda (fd fail frame)
                   (declare (ignore fd))
                   (unify fd1 (rest fd2) path1 path2
                          frame fail success :pair super-pair))
               :pair pair1))

       ;; general case :
       (t
	(let* ((new-path1 (if (attr-p (caar fd2))
			      (path-extend path1 att)
			    (absolute-path (caar fd2) path2)))
	       (new-path2 (if (attr-p (caar fd2))
			      (path-extend path2 att)
			    new-path1))
	       (newpath1 (if endlist? (path-butlast new-path1) new-path1))
	       (newpath2 (if endlist? (path-butlast new-path2) new-path2)))
	  (unify
	   val1 val2 newpath1 newpath2 frame fail
	   #'(lambda (fd fail frame)
	       (declare (ignore fd))
	       (unify fd1 (cdr fd2) path1 path2
		      frame fail success :pair super-pair))
	   :pair pair1))))))))

;(if (equal temp2 '(cdr end-of-list :i))
;		     '(i nil :e)

(defun unify-path (fd1 fd2 path1 path2 frame fail success pair1)
  ;; Called when:
  ;; - fd2 is a path.
  ;; - fd1 can be anything
  ;; - pair1 is either :unknown or the pair containing fd1.
  ;; RESULT: *input* is physically updated - that is, the pair pointed to
  ;; by fd2 is modified.
  ;; NOTE: Careful to config: ((f {m}) (s {m}) (m ((a 1)))) with ((f {s}))
  ;; NOTE: On configs ((a ((r {b})))) with ((b ((n {a})))) (a triangle
  ;; loop) it is important to have the test for fd1 nil to avoid an
  ;; infinite loop (never try to do (gdpp {a r n})).
  (cond
   ((null fd1)
    (update-pair pair1 (absolute-path fd2 path2) path1 frame)
    (funcall success (second pair1) fail frame))
;    (let* ((newpath (absolute-path fd2 path2))
;	   (newpair (gdpp *input* newpath))
;	   (val (safe-second newpair)))
;      (if (and (or (symbolp val) (consp val))
;	       (not (member val *special-attributes*))
;	       (not (member val '(given none any))))
;	  (progn (update-pair pair1 val path1 frame)
;	         (funcall success (second pair1) fail frame))
;          (progn (update-pair pair1 (absolute-path fd2 path2) path1 frame)
;                 (funcall success (second pair1) fail frame)))))
   (t
    (let* ((pointed-path (absolute-path fd2 path2))
	   (pair2 (gdpp *input* pointed-path frame))
	   (last2 (car (path-last pointed-path)))
	   (last1 (car (path-last path1)))
	   (val1 fd1)
	   (new-path path1))
      (when (and
	     (not (eq last1 last2))
	     (or (member last1 *special-attributes*)
		 (member last2 *special-attributes*)))
	(*fail* fail frame path1 path2 pair1
		"Fail in trying special with non-special: ~s and ~s at level ~s"
		fd1 fd2 path1))
      (cond ((path-p fd1)
	     (setf pair1 (gdpp *input* (absolute-path fd1 path1) frame))
	     (setf val1 (safe-second pair1))
	     (setf new-path fd1))
	    (t (setf val1 fd1)
	       (setf new-path path1)))
      (cond
       ((eq pair1 pair2)
	(funcall success (safe-second pair1) fail frame))
       (t
	(unify val1 (safe-second pair2) new-path pointed-path frame fail
	       #'(lambda (fd fail frame)
		   (cond ((fd-boundp fd)
			  (trace-format (frame-trace-flags frame) frame 0
					"Updating ~s with value ~s at level ~s"
					pair2 fd pointed-path)
			  (update-pair pair2 fd pointed-path frame))
			 (t
			  (trace-format (frame-trace-flags frame) frame 0
					"~s becomes a pointer to ~s at level ~s"
					pair2 new-path pointed-path)
			  (update-pair pair2 new-path pointed-path frame)))
		   (funcall success fd1 fail frame))
	       :pair pair1)))))))


;; Companion to the "enrich" function--for nonmonotonic unification
(defun degrade (fd pair frame)
  (declare (special *changes-made* *is-bk-failure*))
  (setf *changes-made* t)     ;; Note that fd has changed
  (if *is-bk-failure*
    (trace-format *trace-bk-class* frame 0 "BK: Degrade with ~s" pair))
  (nconc (frame-undo frame) (list (cons 'r (last fd))))
  (delete (first pair) fd :key #'first))


;; Updated version with end-of-list to support ~n
(defun list-to-FD (l)
  "Converts a regular list into its equivalent FD representation"
  (cond ((null l) 'end-of-list)
        ((leaf-p l) l)
        (t `((car ,(car l))
             (cdr ,(list-to-FD (cdr l)))))))


;; -----------------------------------------------------------------------
;; (provide "$fug5/graph")
;; -----------------------------------------------------------------------
