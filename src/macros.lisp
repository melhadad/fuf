;;; -*- Mode:Lisp; Syntax:Common-Lisp; Package: FUG5 -*-
;;; -----------------------------------------------------------------------
;;; File:         macros2.l
;;; Description:  All macros used in FUG system. WITHOUT CATCH/THROW
;;; Author:       Michael Elhadad
;;; Created:      17 May 1990
;;; Modified:     Changed *fail* to check class of point where failure
;;;               happens and its class and implement smart backtracking.
;;;               Changed backtrack to have a bk-class arg.
;;;               11 Feb 1991: Removed catch/throw
;;;               13 Feb 1991: Fixed leaf-p to exclude path-p!!
;;;               07 Apr 1991: added the throw at limit.
;;;               18 Aug 1991: added attr-p
;;;               19 Aug 1991: fixed fd-boundp on any/given is nil
;;;                            Bug was (gdpp '((l {p}) (p any)) {l})
;;;               27 Nov 1991: Added path-prefix, path-len, path-nthcdr
;;;               06 Dec 1991: Changed under-p to allow for sunder and synonyms.
;;;               17 May 1992: Added *trace-bp* to print ...
;;;               10 Jul 1992: Renamed undo to undo-list (avoid conflict MCL)
;;;               20 Oct 1992: Added level to trace-format
;;;               19 Nov 1993: Added special-p macro.
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

;;; --------------------------------------------------
;;; SAFE ASSOC: works even if a-list contains atoms.
;;; --------------------------------------------------

(defmacro safe-assoc (item a-list)
  `(find ,item ,a-list
	 :key #'(lambda (expr) (if (consp expr) (car expr) nil))))

(defmacro safe-second (expr)
  (let ((free (gensym)))
    `(let ((,free ,expr))
       (if (consp ,free)
	   (second ,free)
	 ,free))))

;;; --------------------------------------------------
;;; BACKTRACK
;;; --------------------------------------------------

;;; Avoid annoying warning in SBCL
(declaim #+sbcl(sb-ext:muffle-conditions style-warning))

(defmacro backtrack (frame new-frame bk-class name &rest exprs)
  `(let ((this-one (gensym "c"))
	 (new-frame (copy-frame ,frame)))
     (declare (special *counter* *counter-limit* *first-bp*))
     ;; Check for limited resources: *counter* remembers # bps used
     (incf *counter*)
     (when *trace-bp* 
       (if (= 0 (mod *counter* *trace-bp-freq*)) (format t ".")))
     (when (> *counter* *counter-limit*)
       (signal-stop *counter* *input*)
       (throw :top-fuf *input*))
     ;; frame-trace and frame-test are cumulative
     ;; frame-undo and name is local to each bp
     (setf (frame-name new-frame) (if ,name ,name :anonymous))
     (setf (frame-undo new-frame) (list 'root))
     (trace-format *top* frame 0 "Creating Point ~s" this-one)
     ;; Try to evaluate the expression.  
     ;; If it fails, we get back a continuation to call and the path where
     ;; it failed (failure address).  The failure address is *same* if it
     ;; is a non-leaf failure (something that is not the direct cause of a
     ;; failure but the result of backtracking).  In this case, the failure
     ;; address remains the previous one.
     (multiple-value-bind (fd fail path1 path2 pair) (progn ,@exprs)
       (cond 
	((or (eq fd *fail*) (eq fd *restart*))
	 (trace-format *top* ,frame 0 "~a point ~s~%" 
		       (if (eq fd *fail*) "Fail" "Restart")
		       this-one)
	 (trace-format *trace-alts* ,frame 0 "Fail in alt ~s" 
		       (frame-name new-frame))
	 (if (match-bk-class path1 path2 pair ,bk-class new-frame)
	   (cond ((eq fd *fail*)
		  (funcall fail nil))
		 ((eq fd *restart*)
		  (undo-list *input* (frame-undo new-frame))
		  (funcall fail nil)))
	   (progn
	     (trace-format *top* ,frame 0
			   "Fail - match bk-class ~s - try upstairs~%"
			   ,bk-class)
	     ;; don't call upstairs if you're on top!
	     (if (eq this-one *first-bp*)
	       (funcall fail nil)
	       (*restart* fail new-frame path1 path2 pair)))))
	;; return the args for the continuation as bp dies a natural death
	;; by the termination of the execution of exprs after
	;; call to a success continuation.
	(t (values fd fail path1 path2 pair))))))


;; How to build a fail continuation that responds properly to the protocol
;; implemented by backtrack/*fail*/*restart*
(defmacro make-failure (parent-fail &rest body)
  `#'(lambda (msg) (if msg ,parent-fail (progn ,@body))))


;; When bk-class mismatch, send a restart msg upstairs
(defmacro *restart* (fail frame path1 path2 pair)
  `(let ((undo-list (frame-undo ,frame)))
     (undo-list *input* undo-list)
     ;; Send a msg t to fail: send me your mother continuation
     (values *restart* (funcall ,fail t) ,path1 ,path2 ,pair)))

;; UNDO all surgical modifs. then
;; send a fail to last backtracking point, that will kill it
;; and print a message.
;; Args are: path1 and path2 paths where failure occured in input (total
;; fd) and grammar.  
;; pair is the pair within which failure occured.
(defmacro *fail* (closure frame path1 path2 pair &optional msg &rest args)
  (let ((msg (when msg `((trace-format 
			   (frame-trace-flags ,frame) ,frame 0
			   ,msg
			   ,@args)))))
    `(let ((undo-list (frame-undo ,frame)))
       (undo-list *input* undo-list)
       ,@msg
       (values *fail* ,closure ,path1 ,path2 ,pair))))


;;; --------------------------------------------------
;;; GENERATOR
;;; --------------------------------------------------

(defmacro empty (generator)
  "Tests whether a generator is empty"
  `(null ,generator))

(defmacro freeze (object)
  "Returns a frozen version of object"
  `#'(lambda () ,object))

(defmacro cons-gen (elt gen)
  "Creates a generator generating first elt, then the values of gen"
  `(cons ,elt #'(lambda () ,gen)))





;; ------------------------------------------------------------
;; Utilities for tracing
;; ------------------------------------------------------------

(defmacro trace-indent (ch frame)
  `(progn
     (format t "~&")
     (dotimes (i (frame-trace-level ,frame)) (format t "--"))
     (format t ,ch)))

(defmacro trace-format (flag frame level msg . args)
  "Like format if flag is true, but preceded by an indentation"
  `(when (and *global-tracing* *local-tracing* 
	      ,flag (trace-enabled ,flag)
	      (>= ,level *level-tracing*))
     (trace-indent ">" ,frame)
     (format t ,msg ,@args)
     (terpri)))

(defmacro trace-demo (flag frame level msg . args)
  "Like trace format but with only spaces, no arrow"
  `(when (and *global-tracing* *local-tracing* 
	      ,flag (trace-enabled ,flag)
	      (>= ,level *level-tracing*))
     (format t (make-sequence 'string (+ 3 (* 2 (frame-trace-level ,frame)))
			      :initial-element #\space))
     (format t ,msg ,@args)
     (terpri)))

(defmacro control-demo (msg . args)
  "To be used in control tests.  Output a nice indented message and always
   succeeds.
   NOTE: works even if %TRACE-OFF% has been used but trace-on is on.
   This is intended to replace the messy default message of control."
  `(let ((%flag% (frame-trace-flags %frame%)))
     (when (and *global-tracing* %flag% (trace-enabled %flag%)
		(<= *level-tracing* 12))
       (format t (make-sequence 
		  'string (1+ (* 2 (frame-trace-level %frame%)))
		  :initial-element #\space))
       (format t ,msg ,@args)
       (terpri))
     t))


(defmacro tracing-flag (sexpr)
  "Is the sexpr a flag put in the grammar to switch tracing on or off"
  `(and (symbolp ,sexpr)
	(eql (char (symbol-name ,sexpr) 0) *trace-marker*)))


;; ------------------------------------------------------------
;; CFORMAT
;; ------------------------------------------------------------

;;; A format aware of the length of the line.
;;; Taken from nikl/krecords.l
(defmacro cFormat (&rest ARGS)
  (let (var key)
    (setq var (mapcar #'(lambda (arg) (list (gensym) arg)) args))
    (setq key (mapcar #'car var))
    `(let* (str count ,@ var)
       (setq str (format nil ,@ key ))
       (setq count (length str))
       (cond ((> (setq *position* (+ *position* count)) *linelength*)
	      (terpri)
	      (setq *position* 0)))
       (format t ,@ key))))



;; --------------------------------------------------
;; TYPE
;; --------------------------------------------------

(defmacro subtype (name)
  `(if (symbolp ,name)
       (get ,name :feature-type)))

(defmacro subtype-fctn (name)
  `(if (symbolp ,name)
       (get ,name :feature-type-fctn)))


;; --------------------------------------------------
;; GRAPH
;; --------------------------------------------------


; ----------
; ATTR-P: is expr a simple attribute
; ----------
(defmacro attr-p (expr)
  `(or (symbolp ,expr) (numberp ,expr) (characterp ,expr)))

; ----------
; SPECIAL-P: is expr a special attribute
; ----------
(defmacro special-p (expr)
  `(member ,expr *special-attributes*))

; ----------
; LEAF-P: is expr a leaf in the graph.
; ----------
(defmacro leaf-p (expr)
  `(and (not (path-p ,expr))
	(or (symbolp ,expr) (numberp ,expr) 
	    (arrayp ,expr) (characterp ,expr))))

; ----------
; FD-BOUNDP: fd is a "real" atom (not nil or any) - it won't be changed
; by further unification.
; ----------
(defmacro fd-boundp (expr)
  `(and ,expr
	(not (eq ,expr 'any))
	(not (eq ,expr 'given))
	(leaf-p ,expr)
	(null (subtype ,expr))
	(null (subtype-fctn ,expr))))

; ----------
; EXTERNALP: fd is an external specification
; ----------
(defmacro externalp (x)
  `(or (eq ,x 'external)
       (and (vectorp ,x)
	    (> (array-dimension ,x 0) 0)
	    (eq (aref ,x 0) 'external))))


; ----------
; UNDERP: fd is an under specification for an atom:
; #(under x) matches specializations of x only in the type hierarchy.
; ----------
(defmacro underp (x)
  `(and (vectorp ,x)
	(> (array-dimension ,x 0) 0)
	(member (aref ,x 0) '(under sunder < <= =<))))


(defmacro under-symbol (x)
  `(aref ,x 1))

;; -----------------------------------------------------------------------
(provide "$fug5/macros")
;; -----------------------------------------------------------------------
