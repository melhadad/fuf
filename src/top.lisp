;;; -*- Mode:Lisp; Syntax:Common-Lisp; Package:FUG5 -*-
;;; -----------------------------------------------------------------------
;;; File:         TOP.L
;;; Description:  Top level functions of FUG5 package with UNDO, INDEXING,
;;;               No stack and SUCCESS and BREADTH-FIRST, and LIMIT on BPs.
;;;               AND DISJUNCTIONS IN INPUT and toplevel-uni macro.
;;;               WITHOUT CATCH/THROW
;;;               WITH FREEZE AND IGNORE
;;; Author:       Michael Elhadad
;;; Created:      02-Nov-89
;;; Modified:     30-Apr-96
;;;               - Defined *special-attributes*
;;;               01 May 90
;;;               - CAT is now a parameter.  Default value is *cat-attribute*
;;;               - register-categories-not-unified defined.
;;;               02 May 90
;;;               - FIND-FSET defined.
;;;               08 May 90
;;;               - U-DISJUNCTIONS defined.
;;;               - toplevel-unify defined.
;;;               10 May 90
;;;               - PATH is a special type.
;;;               21 May 90 - added RALT
;;;               11 Jun 90 - Added success optional arg. passed to
;;;               determine
;;;               21 Jun 90 - return *fail* when fails at top-level.
;;;               27 Jun 90 - added *traced-categories* in unify-cat.
;;;               02 Jul 90 - added path2 arg.
;;;               11 Feb 91 - Removed catch/throw
;;;               07 Apr 91 - add the catch/throw for limit stop in
;;;               toplevel.
;;;               03 May 91 - add ordinal/cardinal to morphology
;;;               28 Jul 91 - add check-agenda in unify-cat
;;;               16 Aug 91 - Fixed bug in find-cset/make-absolute to allow
;;;                           paths in patterns/cset.
;;;               04 Sep 91 - Fixed u-rel and added nu-rel.
;;;               21 Nov 91 - Changed unify-cat to work even if input does
;;;                           not contain (cat xx).
;;;               25 Nov 91 - Made cset a parameter like cat.
;;;               04 Dec 91 - add complex (cset ((= l) (+ l) (- l))) in
;;;               find-cset.
;;;               10 Dec 91 - Deal with interaction wait/cset with
;;;               after-wait and constituent-agenda.
;;;               13 Dec 91 - added grammar cat-att and cset-att args to
;;;               determine to let det-constituents work.
;;;               17 Dec 91 - fixed is-marked to work with :e.
;;;               19 Dec 91 - added calls to use-grammar
;;;               23 Dec 91 - removed calls to add-constituent-agenda.
;;;               06 Jan 92 - added hyper-trace.
;;;               13 Jan 92 - added *from-top* to uni-string.
;;;               06 Feb 92 - avoid :fail in unify-top.
;;;                         - added :no-cset arg to determine.
;;;               24 Feb 92 - added remove-duplicates call in breadth-first.
;;;               15 May 92 - added *from-top* nil to uni-fd
;;;               09 Jul 92 - removed comman in ,(make-path) to work with
;;;                           systems that don't self-evaluate struct (Mac CL).
;;;               20 Oct 92: Added level to trace-format
;;;               01 Jun 93: Moved find-cset functions to file findcset.l
;;;               06 Jun 93: Fixed prep-input to work on simple paths arg.
;;;               26 Oct 93: Update calls to call-linearizer.
;;;                          Defined unregister-category-not-unified
;;;               16 Jan 94: Fixed u-disjunctions.
;;;               08 Feb 12: Added relocate in hyper-trace
;;;               08 Jul 14: Added uni-fd-string
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
(format t "Top level...~%")

(defun clean-fd (fd)
  (filter-flags (filter-nones fd)))

(defun fu (fd1 fd2)
  (clean-fd (u fd1 fd2)))

(defun funi (fd1 fd2)
  (clean-fd (uni-fd fd1 :grammar fd2)))


;; ------------------------------------------------------------
;; USER FUNCTIONS : (UNI FD &optional GRAMMAR NON-INT LIMIT)
;;                  (UNI-FD FD &optional GRAMMAR NON-INT LIMIT)
;;                  (UNI-STRING FD &optional GRAMMAR NON-INT LIMIT)
;;                  (UNIF FD &optional GRAMMAR LIMIT)
;;                  (LIST-CATS &optional GRAMMAR)
;;                  (U fd1 fd2 LIMIT)
;; By default all function use *u-grammar*
;; ------------------------------------------------------------

;; Entry to unification - find default argument *u-grammar*
(defun uni (input &key grammar non-interactive (limit 10000)
		       (cat-attribute *cat-attribute*)
		       (cset-attribute *cset-attribute*))
  "Generate an English sentence represented by the FD input.
       Unify input with the grammar then linearize the result.
       If output is just <fail>, that means the unification has
       failed.
       cf. also UNI-FD and UNIF."
  (let ((*from-top* t))   ;; know that we are calling from uni toplevel
    (declare (special *from-top*))
    (multiple-value-bind (sentence unknown-cats)
	(call-linearizer
	 (unify-top input (or grammar *u-grammar*)
		    :non-interactive non-interactive
		    :limit limit
		    :cat-attribute cat-attribute
		    :cset-attribute cset-attribute)
	 :cat-attribute cat-attribute)
      (print-sentence sentence)
      (if (and unknown-cats (not non-interactive))
	  (morphology-help unknown-cats))
      (values))))

;; Returns the linearized output as a string
(defun uni-string (input &key grammar non-interactive (limit 10000)
			      (cat-attribute *cat-attribute*)
			      (cset-attribute *cset-attribute*))
  "Generate an English phrase represented by the FD input.
   Returns a string."
  (let ((*from-top* t))
    (declare (special *from-top*))
    (apply #'concatenate
	   (cons 'string
		 (call-linearizer
		  (unify-top input (or grammar *u-grammar*)
			     :non-interactive non-interactive
			     :limit limit
			     :cat-attribute cat-attribute
			     :cset-attribute cset-attribute)
		  :cat-attribute cat-attribute)))))


;; Returns 2 values: the unified fd and the linearized output as a string
(defun uni-fd-string (input &key grammar non-interactive (limit 10000)
			      (cat-attribute *cat-attribute*)
			      (cset-attribute *cset-attribute*))
  "Generate an English phrase represented by the FD input.
   Returns the unified fd and a string."
  (let ((*from-top* t))
    (declare (special *from-top*))
    (let ((fd (uni-fd input
		      :grammar grammar
		      :non-interactive non-interactive
		      :limit limit
		      :cat-attribute cat-attribute
		      :cset-attribute cset-attribute)))
      (let ((str (apply #'concatenate
			(cons 'string
			      (call-linearizer fd
					       :cat-attribute cat-attribute)))))
	(values fd str)))))


;; Returns just the unified fd, without linerization.
(defun uni-fd (input &key grammar non-interactive
		          (limit 10000) (cat-attribute *cat-attribute*)
			  (cset-attribute *cset-attribute*))
  "Unify an input FD with a grammar. Returns the unified FD (no
       linearization).
       cf. also UNI and UNIF."
  (let ((*from-top* nil))
    (declare (special *from-top*))
    (values (unify-top input (or grammar *u-grammar*)
		       :non-interactive non-interactive
		       :limit limit
		       :cat-attribute cat-attribute
		       :cset-attribute cset-attribute))))


;; Like uni-fd but works even if input fd does not contain a
;; (cat x) feature.
(defun unif (fd &key (grammar *u-grammar*) )
  "Unify the FD fd with the FUG5 grammar, EVEN when fd doesn't have any
   (CAT x) pair and returns the unified FD (no linearization).
   cf. also UNI and UNI-FD."
  (do ((l (list-cats grammar) (cdr l))
       (result nil (uni-fd (cons (car l) fd) :grammar grammar)))
      ((or (null l) result) result)))

(defun list-cats (&optional (grammar *u-grammar*))
  "Return the list of (CAT x) pairs of the FUG grammar"
  (mapcar #'car (cadar grammar)))


;; ------------------------------------------------------------
;; INTERNAL FUNCTIONS
;; ------------------------------------------------------------

(defmacro toplevel-uni (fd1 fd2 limit cset-attribute &rest expr)
  "Defines the top-level environment where a function calling unify can be
   defined: contains all special variables and initial backtracking point."
  `(if (eq ,fd1 *fail*)
     ,fd1
     (let ((*special-attributes* (cons ,cset-attribute *special-attributes*))
	 (*cset* ,cset-attribute)
	 (*input* (prep-input (get-fd ,fd1) ,cset-attribute))
	 (,fd2 (use-grammar ,fd2))
	 (*agenda* nil)
	 (*agenda-id* 0)
	 (path (make-path))
	 (frame (make-frame))
	 (*counter* 0)         ; how many backtraking points met so far.
	 (*counter-limit* ,limit) ; max. bp allowed.
	 (*first-bp* nil)      ; first bp not to fail
	 (*wrong-branches* 0)  ; how many times did we choose a wrong branch
	 (*number-of-undo* 0)  ; how many operations did we undo total
	 (*bk-frames-skipped* 0) ; how many frames skipped by bk-class
	 (*is-bk-failure* nil) ; last failure was not of a special class
	 (*failure-address* (make-path))
	 (*class-of-failure* nil)
	 (*changes-made* nil)
	 (*print-pretty* t))   ; system variable:all output is pretty printed
    (declare (special *counter* *counter-limit* *first-bp* *cset*
		      *bk-frames-skipped* *is-bk-failure* *failure-address*
		      *class-of-failure* *changes-made* *agenda*
		      *wrong-branches* *number-of-undo* *input*))
    (gensym 0)                ; just to initialize the counter.
    (catch :top-fuf
      (backtrack frame new-frame nil :top      ; toplevel has no bk-class
		 (setf *first-bp* this-one)    ; remember first bp
		 ,@expr)))))

(defun default-continuation (fd fail frame)
  (declare (ignore fd)
	   (special *input*))
  (values *input* fail frame))

(defun default-fail (msg)
  (declare (ignore msg))
  *fail*)


(defun u-disjunctions (fd1 fd2
			   &key
			   (limit 1000)
			   (cat-attribute *cat-attribute*)
			   (cset-attribute *cset-attribute*)
			   (failure 'default-fail)
			   (success 'default-continuation))
  "Unifier - accepts disjunctions (alt and opt) in fd1 and fd2.
   Given in fd1 will always fail.  Does not recurse on constituents."
  ;; Method: first, build one disjunction-free version of fd1 by unifying
  ;; nil with fd1 considered as a grammar.  Then, unify this version of fd1
  ;; with fd2 using the generator of fd1 as a fail continuation.
  (setf fd2 (use-grammar fd2))
  (toplevel-uni
   nil fd1 limit cset-attribute
   (unify nil fd1 path path new-frame
	  (make-failure failure *fail*)
	  #'(lambda (fd fail frame)
	      (let ((path (make-path))
		    (*input* fd))
		(declare (special *input*))
		(unify *input* fd2 path path frame fail
		       #'(lambda (fd fail frame)
			   (determine fd fail frame success
				      fd2
				      cat-attribute
				      cset-attribute
				      :no-cset))))))))


(defun u (fd1 fd2
	      &key
	      (limit 10000)
	      (cat-attribute *cat-attribute*)
	      (cset-attribute *cset-attribute*)
	      (failure 'default-fail)
	      (success 'default-continuation))
  "Unifier. Takes 2 functional descriptions. Returns the most general
       unifier of both. Does NOT recurse on constituents."
  (toplevel-uni
   fd1 fd2 limit cset-attribute
   (unify *input* fd2 path path new-frame
	  (make-failure failure *fail*)
	  #'(lambda (fd fail frame)
	      (determine fd fail frame success
			 fd2 cat-attribute cset-attribute :no-cset)))))


(defun u-rel (xpath fd2
		  &key
		  (total *input*)
		  (limit 1000)
		  (cat-attribute *cat-attribute*)
		  (cset-attribute *cset-attribute*)
		  (failure 'default-fail)
		  (success 'default-continuation))
  "Unifier of sub-fds. Takes a path in a total fd and an FD.
   Returns the total fd with unif of the sub-fd appearing at level xpath in
   total with fd2.  The total fd is not physically modified."
  (toplevel-uni
   total fd2 limit cset-attribute
   (unify (gdp *input* xpath) fd2 xpath xpath new-frame
	  (make-failure failure *fail*)
	  #'(lambda (fd fail frame)
	      (determine fd fail frame success
			 fd2 cat-attribute cset-attribute :no-cset)))))


(defun nurel (xpath fd2
		  &key
		  (total *input*)
		  (limit 1000)
		  (cat-attribute *cat-attribute*)
		  (cset-attribute *cset-attribute*)
		  (failure 'default-fail)
		  (success 'default-continuation))
  "Like u-rel but modifies total physically"
  (let ((*input* total)
	(*cset* cset-attribute)
	(*special-attributes* (cons cset-attribute *special-attributes*))
	(*agenda* nil)
	(*agenda-id* 0)
	(frame (make-frame))
	(*counter* 0)         ; how many backtraking points met so far.
	(*counter-limit* limit) ; max. bp allowed.
	(*first-bp* nil)      ; first bp not to fail
	(*wrong-branches* 0)  ; how many times did we choose a wrong branch
	(*number-of-undo* 0)  ; how many operations did we undo total
	(*print-pretty* t))   ; system variable:all output is pretty printed
    (declare (special *counter* *counter-limit* *cset*
		      *first-bp*
		      *agenda*
		      *wrong-branches* *number-of-undo* *input*))
    (gensym 0)                ; just to initialize the counter.
    (catch :top-fuf
      (backtrack frame new-frame nil :top      ; toplevel has no bk-class
		 (setf *first-bp* this-one)    ; remember first bp
		 (unify (gdp *input* xpath) fd2 xpath xpath new-frame
			(make-failure failure *fail*)
			#'(lambda (fd fail frame)
			    (determine fd fail frame success
				       fd2 cat-attribute
				       cset-attribute :no-cset)))))))


;; Top-level for unification.
(defun unify-top (input grammar &key non-interactive
			(limit 10000)
			(cat-attribute *cat-attribute*)
			(cset-attribute *cset-attribute*)
			(failure 'default-fail)
			(success 'default-continuation))
  "Unify a fd with a grammar.
   Preprocess input, unify top-level, then constituents.
   If non-interactive is nil, statistics are printed."
  (toplevel-uni
   input grammar limit cset-attribute
   (unify-sub-constituents
    *input* grammar path new-frame
    (make-failure failure *fail*)
    #'(lambda (fd fail frame)
	(unless non-interactive
	  (format t "~%[Used ~D backtracking points ~
		      - ~D wrong branches - ~D undo~:P]~%"
		  *counter* *wrong-branches* *number-of-undo*))
	(determine fd fail frame
		   #'(lambda (fd fail frame)
		       (unless non-interactive
			 (format t "~%[Used ~D backtracking points ~
		      - ~D wrong branches - ~D undo~:P]~%"
				 *counter* *wrong-branches* *number-of-undo*))
		       (funcall success fd fail frame))
		   grammar cat-attribute cset-attribute))
    cat-attribute
    cset-attribute)))


;; ------------------------------------------------------------
;; PREPROCESSING
;; ------------------------------------------------------------
;; Prepare input by converting === to ((lex "___"))
;; AND converting all relative paths to absolute paths.
;; Also add :i as a flag to indicate which pair comes from the input.
;; The flag is used by bk-class to determine the address of failure.
(defun prep-input (in-fd &optional (cset-attribute *cset-attribute*)
			 (path (make-path)))
  (cond
   ((leaf-p in-fd) in-fd)
   ((path-p in-fd) (absolute-path in-fd path))
   (t
    (mapcar
     #'(lambda (pair)
	 (cond ((leaf-p pair) pair)
	       ((eq (second pair) '===)
		(list
		 (car pair)
		 `((lex ,(let ((name (third pair)))
			   (cond ((stringp name) name)
				 ((symbolp name)
				  (string-downcase (string name)))
				 (t (prin1-to-string name))))
			:i))))
	       ((path-p (second pair))
		(list
		 (car pair)
		 (absolute-path (second pair)
				(path-extend path (car pair)))
		 :i))
	       ((leaf-p (second pair))
		(list (car pair) (second pair) :i))
	       ((eq (car pair) cset-attribute)
		(list (car pair) (second pair) :i))
	       ((member (car pair) *special-attributes*)
		(list (car pair) (second pair) :i))
	       (t (list
		   (car pair)
		   (prep-input (second pair) cset-attribute
			       (path-extend path (car pair)))))))
     in-fd))))


;; ------------------------------------------------------------
;; POST-PROCESSING
;; ------------------------------------------------------------

(defun PRINT-SENTENCE (list-of-word-strings)
  (setq *position* 0)
  (mapc #'(lambda (s) (cformat "~a" s)) list-of-word-strings)
  (terpri)   (setq *position* 0))



;; ------------------------------------------------------------
;; UNIFY-CAT : unify an fd at one level.
;; ------------------------------------------------------------

(defun unify-cat (input grammar path frame fail success arc
			&optional (cat-attribute *cat-attribute*)
			(cset-attribute *cset-attribute*) force-wait)
  "Unify a fd at one level (constituents are not unified).
  2 tricks to speed up :
  a/ Find only the relevant part of the grammar to match with.
  b/ Don't unify lexical categories, which would not be enriched anyway.
  Lexical categories for each cat-attribute are registered with function
  register-categories-not-unified.
  NOTE: Check if grammar waits on empty cats,
  if it does and input does not contain a (cat xxx), return :frozen.
  ARC is the pair containing fd or :unknown if not known."

  (declare (special *input*) (ignore cset-attribute))
  (when (path-p input)
    (setf input (gdp *input* path)))
  (cond ((leaf-p input)
	 (funcall success input fail frame))
	(t
	 (setf (frame-trace-flags frame) nil)
	 (setf (frame-trace-level frame) 0)
	 (let ((icat (safe-assoc cat-attribute input))
	       (frozen nil))
	   (cond
	    ((member (second icat) (categories-not-unified cat-attribute))
	     (funcall success input fail frame))
	    (t
	     (when (and *global-tracing* *local-tracing*)
	       (cond
		((and (null force-wait)
		      (null (safe-second icat))
		      (freeze-on-cat grammar cat-attribute))
		 (trace-format *trace-wait* frame 15 "FREEZING CAT AT PATH ~s" path)
		 (setf frozen t))
		((or (eq *traced-categories* :all)
		     (member (second icat) *traced-categories*
			     :test #'(lambda (x y)
				       (or (subsume x y) (subsume y x)))))
		 (trace-format t frame 20
			       "~%~%>========================================")
		 (trace-format t frame 20
			       "STARTING CAT ~s AT LEVEL ~s"
			       (second icat) path)
		 (trace-format t frame 20
			       "========================================~%~%")
		 (when (or (eq *hyper-traced-categories* :all)
			   (member (second icat) *hyper-traced-categories*
				   :test #'(lambda (x y)
					     (or (subsume x y) (subsume y x)))))
		   (trace-format t frame 20
				 "CONSTITUENT ~s =~%~s~% = ~s~%~%"
				 path (gdp *input* path) (relocate *input* path))))))
	     (check-agenda
	      input grammar path path frame fail
	      #'(lambda (fd fail frame)
		  (cond (frozen (funcall success :frozen fail frame))
			(t (funcall success fd fail frame))))
	      :pair arc
	      :force-wait force-wait
	      :after-wait #'(lambda ()
			      (if frozen
				(mark-arc2 arc path frame)
				(mark-arc arc path frame)))
	      :indexed-given (list cat-attribute))))))))


(defun freeze-on-cat (gram cat-attribute)
  "Does grammar wait for cat to be instantiated?"
  (multiple-value-bind (br tr ind dem bk ord wait iu iw) (branches (car gram))
    (declare (ignore br tr ind dem bk ord iu iw))
    (equalp (car wait) (list (make-path :l (list '^ cat-attribute)) 'given))))


(defun categories-not-unified (&optional (cat-attribute *cat-attribute*))
  (get cat-attribute :categories-not-unified))

(defun register-categories-not-unified
  (&optional (cat-attribute *cat-attribute*)
	     (list-cats *lexical-categories*))
  (setf (get cat-attribute :categories-not-unified) list-cats))

(register-categories-not-unified)

(defun register-category-not-unified
  (nucat &optional (cat-attribute *cat-attribute*))
  (cond ((symbolp nucat)
	 (pushnew (get cat-attribute :categories-not-unified) nucat))
	((and (consp nucat) (every #'symbolp nucat))
	 (setf (get cat-attribute :categories-not-unified)
	       (union (get cat-attribute :categories-not-unified) nucat)))
	(t (error "Parameter must be a symbol or a list of symbols"))))

(defun unregister-category-not-unified
  (cat &optional (cat-attribute *cat-attribute*))
  (setf (get cat-attribute :categories-not-unified)
	(remove cat (get cat-attribute :categories-not-unified))))

(defun mark-arc (arc path frame)
  "Mark this path as already unified by unify-cat.
       This can be undone as a simple enrich so that it will be undone"
  (when (not (consp arc))
    (setf arc (gdpp *input* path frame)))
  (nconc (frame-undo frame) (list (cons 'r (last arc))))
  (nconc arc (list '*done*)))

(defun mark-arc2 (arc path frame)
  "Mark this path as already unified by unify-cat but not yet traversed by
   breadth-first.  This is to deal with interaction wait/cset"
  (when (not (consp arc))
    (setf arc (gdpp *input* path frame)))
  (nconc (frame-undo frame) (list (cons 'r (last arc))))
  (nconc arc (list '*done-after-wait*)))

(defun arc-is-marked (arc)
  (and (consp arc)
       (> (length arc) 2)
       (eq (car (last arc)) '*done*)))

(defun arc-is-marked-after-wait (arc)
  (and (consp arc)
       (> (length arc) 2)
       (eq (car (last arc)) '*done-after-wait*)))


;; ------------------------------------------------------------
;; UNIFY-SUB-CONSTITUENTS : unify lower levels of an fd
;; ------------------------------------------------------------
;; Constituents pushed on constituent-agenda will be traversed at
;; determination time.

(defun unify-sub-constituents (fd grammar path frame fail success
				  &optional (cat-attribute *cat-attribute*)
				  (cset-attribute *cset-attribute*))
  "Recursively unify the sub-constituents of an fd with BREADTH-FIRST
      traversal."
  (unify-cat fd grammar path frame fail
	     #'(lambda (fd fail frame)
		 (cond ((eq fd :frozen)
			#+ignore(add-constituent-agenda
			 (make-path) frame grammar cat-attribute cset-attribute)
			(funcall success *input* fail frame))
		       (t
			(let ((cset (find-cset fd path cat-attribute cset-attribute)))
			  (trace-format
			   *trace-cset* frame 20
			   "Expanding constituent {} into cset ~s." cset)
			  (unify-breadth-first
			   cset fd grammar frame fail success cat-attribute
			   cset-attribute)))))
	     :unknown        ;; pair containing fd
	     cat-attribute
	     cset-attribute))


(defun unify-breadth-first (lpath fd grammar frame fail success
				  &optional (cat-attribute *cat-attribute*)
				  (cset-attribute *cset-attribute*) force-wait)
  (cond
   ((null lpath) (funcall success *input* fail frame))
   (t (let ((arc (gdpp *input* (car lpath) frame)))
	(cond
	 ((arc-is-marked arc)
	  (unify-breadth-first (cdr lpath) fd grammar frame fail
			       success cat-attribute cset-attribute))
	 ((arc-is-marked-after-wait arc)
	  ;; we are waking up from a frozen constituent
	  ;; just do the traversal now - unify-cat is already done
	  (let ((cset (find-cset (safe-second arc) (car lpath)
				 cat-attribute cset-attribute)))
	    (when *trace-cset*
	      (if cset
		(trace-format
		 *trace-cset* frame 20
		 "Expanding constituent ~s after wait into cset ~s."
		 (car lpath) cset)
		(trace-format
		 *trace-cset* frame 20
		 "Constituent ~s is a leaf." (car lpath))))
	    (unify-breadth-first
	     (remove-duplicates (append (cdr lpath) cset)
				:test #'path-equal :from-end t)
	     fd grammar frame fail success
	     cat-attribute cset-attribute force-wait)))
	 (t
	  (unify-cat
	   (safe-second arc) grammar (car lpath) frame fail
	   #'(lambda (fd fail frame)
	       (cond
		((eq fd :frozen)  ;; delay traversal of this constituent
		 #+ignore(add-constituent-agenda
		  (car lpath) frame grammar cat-attribute cset-attribute)
		 (unify-breadth-first
		  (cdr lpath)
		  fd grammar frame fail success
		  cat-attribute cset-attribute force-wait))
		(t
		 (let ((cset (find-cset fd (car lpath) cat-attribute cset-attribute)))
		   (when *trace-cset*
		     (if cset
		       (trace-format
			*trace-cset* frame 20
			"Expanding constituent ~s into cset ~s."
			(car lpath) cset)
		       (trace-format
			*trace-cset* frame 20
			"Constituent ~s is a leaf." (car lpath))))
		   (unify-breadth-first
		    (append (cdr lpath) cset)
		    fd grammar frame fail success
		    cat-attribute cset-attribute force-wait)))))
	   arc
	   cat-attribute
	   cset-attribute
	   force-wait
	   )))))))

;; -----------------------------------------------------------------------
(provide "$fug5/top")
;; -----------------------------------------------------------------------
