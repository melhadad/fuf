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
;;;               05 Jun 94: Add loop detection in path-unify. 
;;;               07 Jun 94: Rewritten path-unify to maintain canonicity.
;;; Package:      FUG5
;;; Macros:       leaf-p, fd-boundp
;;; Status:       Experimental
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
(format t "Graph unifier...~%")

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
   ((eq fd1 'none)
    (cond ((member fd2 '(nil none))
	   (funcall success 'none fail frame))
	  (t (*fail* fail frame path1 path2 pair1
		     "Fail in trying NONE with ~s at level ~s" fd2 path1))))
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
	       #+ignore(make-failure fail
		  (trace-format (frame-trace-flags frame) frame 5
				"Couldn't use external constraint.")
		  (funcall fail nil))
	       fail
	       #'(lambda (fd fail frame)
		   (trace-format (frame-trace-flags frame) frame 5
				 "Success with external constraint.")
		   (funcall success fd fail frame))
	       :pair pair1))))
   ((or (leaf-p fd1) (empty-fd fd1))
    (unify-leaf12 fd1 fd2 path1 path2 frame fail
		  #'(lambda (fd fail frame)
		      (trace-format 
		       (frame-trace-flags frame) frame 0
		       "Updating ~s with ~s at level ~s"
		       pair1 fd path1)
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
   ((path-p fd2) (unify-path fd1 fd2 path1 path2 frame fail success pair))
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
	;; fd1 is a path coming from a canonic fd - so it must be the phys rep.
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
  (let* ((pair1 (cond ((attr-p (caar fd2))
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
    (cond 
      
     ((null pair1)
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

       ;; If val2 is given or any, fail.
       ((and 
	 ;; Commented out May 2, 1991 - Allow (pattern any) to work
	 ;; (not (member att *special-attributes*))
	 (or (and *any-at-unification* (eq 'any val2))
	     (and *use-given* (eq 'given val2))))
	(*fail* fail frame path1 path2 pair1
		"fail in trying ~s with ~s at level ~s" 
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

       ;; If val2 is an `atomic' value (but not a path), add a copy of
       ;; the pair  
       ((member att *special-attributes*)
	(let ((new-pair (copy-special-pair pair2 path2)))
	  (trace-format (frame-trace-flags frame) frame 0
			"Enriching input with ~s at level ~s" 
			new-pair path2)
	  (enrich fd1 new-pair frame))
	(unify fd1 (cdr fd2) path1 path2 frame fail success :pair super-pair))

       ;; val2 is a complex fd or a path: recurse on val2.
       (t
	(let ((new-pair (list att nil))
	      (new-path2 (if (attr-p (car pair2))
			     (path-extend path2 att)
			   (absolute-path (car pair2) path2))))
	  (enrich fd1 new-pair frame)
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
		  "fail in trying ~s with ~s at level ~s" 
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

       ;; general case :
       (t
	(let* ((new-path1 (if (attr-p (caar fd2))
			      (path-extend path1 att)
			    (absolute-path (caar fd2) path2)))
	       (new-path2 (if (attr-p (caar fd2))
			      (path-extend path2 att)
			    new-path1)))
	  (unify 
	   val1 val2 new-path1 new-path2 frame fail 
	   #'(lambda (fd fail frame)
	       (declare (ignore fd))
	       (unify fd1 (cdr fd2) path1 path2
		      frame fail success :pair super-pair))
	   :pair pair1))))))))


#+ignore(defun unify-path (fd1 fd2 path1 path2 frame fail success pair1)
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
  ;; INSURE CANONICITY: make sure physical rep is inserted at most canonic path.
  ;; compare fd2 with phys-rep of fd1.
  (let ((phys1 (phys-rep path1))
	(abs2 (absolute-path fd2 path2))
	(pair2 (gdpp *input* pointed-path frame))
	(last2 (car (path-last pointed-path)))
	(last1 (car (path-last path1)))
	(val1 fd1)
	(new-path path1))
    (cond 
     ((null fd1)
      (update-pair pair1 (absolute-path fd2 path2) path1 frame)
      (funcall success (second pair1) fail frame))
   (t
    (let* ((pointed-path (absolute-path fd2 path2)))
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

       ;; Loop detection: (u '((a ((b 1)))) '((a {})))
       ((path-prefix path1 pointed-path)
	(update-pair pair1 pointed-path path1 frame)
	(unify pointed-path val1 path1 path2 frame fail success))

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
	       :pair pair1))))))))

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
  ;; INSURE CANONICITY: make sure physical rep is inserted at most canonic path.
  ;; compare fd2 with phys-rep of fd1.
  (let ((abs2 (absolute-path fd2 path2))
	(abs1 (if (path-p fd1) (absolute-path fd1 path1) path1)))
    (multiple-value-bind (val1 phys1 missing1 node1 pair11) (gdc *input* abs1)
      (multiple-value-bind (val2 phys2 missing2 node2 pair2) (gdc *input* abs2)
	(declare (ignore val2))
	(format t "~&phys1 = ~s - phys2 = ~s" phys1 phys2)
	(let ((last1 (first (path-last path1)))
	      (last2 (first (path-last abs2))))
	  (cond 
	   ((and (not (eq last1 last2))
		 (or (special-p last1) (special-p last2)))
	    (*fail* fail frame path1 path2 pair11
		    "Fail in trying special with non-special: ~s and ~s at level ~s"
		    fd1 fd2 path1))

	   ((or (eq pair11 pair2) (path-equal phys1 phys2))
	    (funcall success (safe-second pair1) fail frame))

	   ((path-prefix phys1 phys2)
	    

	   (t
	    (unless (path-null missing2)
	      (setf pair2 (add-missing node2 pair2 phys2 missing2 frame))
	      (setf phys2 (path-append phys2 missing2)))
	    (unless (path-null missing1)
	      (setf pair11 (add-missing node1 pair11 phys1 missing2 frame))
	      (setf val1 (safe-second pair11))
	      (setf phys1 (path-append phys1 missing1)))
	    (unify 
	     val1 (safe-second pair2) phys1 phys2 frame fail
	     #'(lambda (fd fail frame)
		 (cond 
		  ((and *conflate-leaves* (fd-boundp fd))
		   (trace-format (frame-trace-flags frame) frame 0
				 "Updating ~s with value ~s at level ~s" 
				 pair2 fd phys2)
		   (update-pair pair1 fd abs1 frame)
		   (update-pair pair2 fd phys2 frame))
		  ;; Loop detection: insert value in shorter path 
		  ((path-prefix phys1 phys2)
		   (update-pair pair2 fd phys2 frame)
		   (update-pair pair11 phys2 phys1 frame)
		   (unless (or (eq pair1 pair2) (eq pair1 pair11))
		     (update-pair pair1 phys2 abs1 frame)))
		  ((path-prefix phys2 phys1)
		   (update-pair pair11 fd phys1 frame)
		   (update-pair pair2 phys1 phys2 frame)
		   (unless (or (eq pair1 pair2) (eq pair1 pair11))
		     (update-pair pair1 phys1 abs1 frame)))
		  ;; Canonize: insert value in smaller path
		  ((funcall *smaller-path-fct* phys1 phys2)
		   (update-pair pair11 fd phys1 frame)
		   (update-pair pair2 phys1 phys2 frame)
		   (unless (or (eq pair1 pair2) (eq pair1 pair11))
		     (update-pair pair1 phys1 abs1 frame)))
		  (t
		   (update-pair pair2 fd phys2 frame)
		   (update-pair pair11 phys2 phys1 frame)
		   (unless (or (eq pair1 pair2) (eq pair1 pair11))
		     (update-pair pair1 phys2 abs1 frame))))
		 (funcall success fd1 fail frame))
	     :pair pair11))))))))



;; -----------------------------------------------------------------------
(provide "$fug5/graph")
;; -----------------------------------------------------------------------

