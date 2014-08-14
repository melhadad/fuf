;;; -*- Mode:Lisp; Syntax:Common-Lisp; Package:FUG5 -*-
;;; -----------------------------------------------------------------------
;;; File:         type.l
;;; Description:  Functions to define and manipulate types for FUGs
;;; Author:       Michael Elhadad
;;; Created:      02-Nov-88
;;; Modified:     27-Nov-88
;;;               30 Apr 90 Define special attributes with proc. unif.
;;;               20 Jun 90 Allowed for equations in special-unify.
;;;               28 Jun 90 defined reset-typed-features
;;;               02 Jul 90 use path2.
;;;               13 Aug 91 allow for multiple inheritance hierarchies
;;;                         of features (unify-lattice).
;;;               13 Nov 91 changed call to *fail* for bk-class
;;;               27 Nov 91 added unify-cset
;;;               05 Dec 91 updated unify-cset for new-style csets
;;;                         make unify-special call unification method with
;;;                         path+name-of-att instead of just path to allow
;;;                         usage of {^ x}.
;;;               06 Dec 91 Changed subsume to deal with sunder.
;;;               11 Feb 92 Deal with *added-cset* in unify-cset.
;;;               17 May 92 Use path2 instead of path1 in proc.unification.
;;;                         Added reset-procedural-type(s)
;;;               20 Oct 92: Added level to trace-format
;;;               01 Jun 93: Added option == in cset functions.
;;;               30 May 94: Added relocate-fct to define-procedural-type
;;; Package:      FUG5
;;; Macros:       subtype, subtype-fctn
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
(format t "Typer...~%")

;;; ----------------------------------------
;;; TYPED FEATURES:
;;; Define a hierarchy over the atoms.
;;; Usage: (define-feature-type name (specialization1 spec2 ...) function)
;;;        (subtype name)        --> (specialization1 ...)
;;;        (subsume name1 name2) --> t if name2 is a specialization of name1
;;; ----------------------------------------

;; If fctn is given, x is a subtype of name if (funcall fctn x).
(defmacro define-feature-type (name values)
  `(progn 
     (unless (and (symbolp ',name)
		  (listp ',values)
		  (every #'symbolp ',values))
	 (error "Usage (define-feature-type symbol (sym1 ... symn))~%No quotes~%"))
     (if (and *warn-type-redefinition* (get ',name :feature-type))
	 (format t "WARNING: Redefining subtypes for symbol ~s~%" ',name))
     (pushnew ',name *typed-features*)
     (setf (get ',name :feature-type) ',values)))

(defun subsume (t1 t2)
  (let ((strict nil))
    (when (underp t1) 
      (setf strict (member (aref t1 0) '(< sunder)))
      (setf t1 (aref t1 1)))
    (when (underp t2) (setf t2 (aref t2 1)))
    (cond ((equalp t1 t2) (not strict))
	  ((not (symbolp t1)) nil)
	  ((null (subtype t1)) nil)
	  ((member t2 (subtype t1)) t)
	  ((some #'(lambda (sub) (subsume sub t2)) (subtype t1))))))


(defun reset-typed-features ()
  (mapc #'(lambda (f) 
	    (setf (get f :feature-type) nil)
	    (setf (get f :feature-mark) nil))
	*typed-features*)
  (setf *typed-features* nil)
  (values))
	      

;; Perform a breadth-first search on list of open nodes
;; At each node, call function f.
;; Check if node has already been visited with marked.
;; open is the list of open nodes.
(defun bf-search (lnodes f marked)
  (cond ((null lnodes) nil)
	(t (let ((node (pop lnodes)))
	     (cond ((funcall marked node)
		    (bf-search lnodes f marked))
		   (t 
		    (funcall f node)
		    (bf-search (append lnodes (subtype node)) f marked)))))))


(defun remove-all-marks ()
  (mapc #'(lambda (node) (setf (get node :feature-mark) nil))
	*typed-features*))


;; Perform a unification of 2 symbols in the lattice
(defun unify-lattice (a b)
  (unless (and (symbolp a) (symbolp b)) (return-from unify-lattice))
  (remove-all-marks)
  (bf-search (list a)
	     #'(lambda (node)
		 (setf (get node :feature-mark) a))
	     #'(lambda (node)
		 (eq a (get node :feature-mark))))
  (bf-search (list b)
	     #'(lambda (node)
		 (setf (get node :feature-mark) b))
	     #'(lambda (node)
		 (cond ((eq a (get node :feature-mark))
			(return-from unify-lattice node))
		       ((eq b (get node :feature-mark)))))))



;;; ----------------------------------------
;;; PROCEDURAL ATTRIBUTES
;;; Define specialized unification procedures for special types.
;;; The special types are considered "atomic" types (cannot access to
;;; components from outside).
;;; The unification procedure must be deterministic and must be a real
;;; "unification" procedure: that is, the type must be a lattice.
;;; Usage: (define-procedural-type name function 
;;;          :syntax checker 
;;;          :copier copier
;;;          :relocate relocater)
;;; Declares <name> to be a special attribute, whose value can only be
;;; interpreted by <function>.
;;;
;;; <FUNCTION> must be a function of 3 args: the vals to unify and the path
;;; where the result is to be located in the total fd.
;;; It must return :fail if unification fails, otherwise, it must return a
;;; valid object of type <type>.
;;; NOTE: <FUNCTION> must be such that NIL is always acceptable as an
;;; argument and is always neutral, ie, (<FUNCTION> x nil) = x.
;;; NOTE: <FUNCTION> must be such that (<FUNCTION> x x) = x
;;;
;;; <CHECKER> must be a function of 1 arg: 
;;; It must return either True if the object is a syntactically correct
;;; element of <TYPE>, otherwise, it must return 2 values:
;;; NIL and a string describing the correct syntax of <TYPE>.
;;;
;;; <COPIER> must be a function of 1 arg:
;;; it must copy an object of <TYPE> that has no cons in common with its
;;; argument.  By default, COPY-TREE is used.
;;; NOTE: (<COPIER> x) = (<FUNCTION> x nil)
;;;
;;; <RELOCATER>  must be a function of 4 arguments (f val rpath tpath cpath)
;;; It is used to patch the value of a typed feature during relocation.
;;; val: value to be patched (appears under the feature).
;;; rpath: relocation path.
;;; tpath: path where val appears within the total fd.
;;; cpath: path where val appears within the relocated fd.
;;; Must return the patched val.
;;; By default copy-tree is used.
;;; ----------------------------------------

(defun define-procedural-type (name fctn &key syntax 
				    (copier 'copy-tree)
				    (relocater 'copy-tree))
  ;; Check validity of arguments: name is a symbol, fctn a compiled
  ;; function and syntax either nil or a function.
  (unless (symbolp name)
    (error "FUF Error~%Define-procedural-type: NAME ~s must be a symbol" name))
  #+ignore(unless (functionp fctn)
    (error "FCTN must be a function"))
  #+ignore(unless (null syntax)
    (unless (functionp syntax)
      (error "SYNTAX must be NIL or a function of one argument.")))
  #+ignore(unless (functionp syntax)
    (error "COPIER must be a function of one argument."))
  ;; Mark name as a special attribute
  (register-special-attribute name)
  (register-special-unif-function name fctn)
  (register-special-syntax-function name syntax)
  (register-special-copier-function name copier)
  (register-special-relocater-function name relocater)
  name)

;; Undefine a proc. type
(defun reset-procedural-type (name)
  (setf *special-attributes* (remove name *special-attributes*))
  (setf (get name :special-unif-function) nil)
  (setf (get name :special-syntax-function) nil)
  (setf (get name :special-copier-function) nil)
  (setf (get name :special-relocater-function) nil))

(defun reset-procedural-types ()
  (mapc #'reset-procedural-type *special-attributes*)
  (define-procedural-type 'cset #'unify-cset 
    :syntax #'check-cset
    :relocater #'relocate-pattern))
  
(defun register-special-attribute (name)
  (if (member name '(pattern control test alt opt))
      (error "~s is a reserved name." name))
  (pushnew name *special-attributes*))

(defun register-special-unif-function (name fctn)
  (setf (get name :special-unif-function) fctn))

(defun special-unif-function (name)
  (get name :special-unif-function))

(defun register-special-syntax-function (name fctn)
  (setf (get name :special-syntax-function) fctn))

(defun special-syntax-function (name)
  (get name :special-syntax-function))

(defun register-special-copier-function (name fctn)
  (setf (get name :special-copier-function) fctn))

(defun special-copier-function (name)
  (get name :special-copier-function))

(defun register-special-relocater-function (name fctn)
  (setf (get name :special-relocater-function) fctn))

(defun special-relocater-function (name)
  (get name :special-relocater-function))

  
;; ------------------------------
;; SPECIAL-UNIFY: perform type specific unification
;; ------------------------------
(defun special-unify (fd1 fd2 pair1 pair2 path1 path2 frame fail success
			  &key (pair :unknown))
  ;; pair1 and pair2 start with same attribute in car position.
  ;; att is a special attribute.
  ;; fd1 appears at level path in total fd.
  ;; frame, fail and success contain regular backtracking info.
  (let* ((att (car pair1))
	 (new-path1 (if (symbolp (car pair2))
			(path-extend path1 (car pair2))
		      (absolute-path (car pair2) path2)))
	 (new-path2 (if (symbolp (car pair2))
			(path-extend path2 (car pair2))
		      new-path1))
	 (p-path1 (when (path-p (safe-second pair1))
		    (absolute-path (safe-second pair1) new-path1)))
	 (p-path2 (when (path-p (safe-second pair2))
		    (absolute-path (safe-second pair2) new-path2)))
	 (pointed-pair1 (if p-path1
			    (gdpp *input* p-path1 frame)
			  pair1))
	 (pointed-pair2 (if p-path2
			    (gdpp *input* p-path2 frame)
			  pair2))
	 (arg1 (safe-second pointed-pair1))
	 (arg2 (safe-second pointed-pair2)))
    (cond 
     ;; Type checking:
     ((not (eq (car pointed-pair1) att))
      (*fail* fail frame path1 path2 pair
	      "Cannot unify special ~s with non-special ~s at level ~s"
	      att (car pointed-pair1) path1))
     ((not (eq (car pointed-pair2) att))
      (*fail* fail frame path1 path2 pair
	      "Cannot unify special ~s with non-special ~s at level ~s~%~
               ***** Probably ERROR in the grammar *****"
	      att (car pointed-pair2) path1))
     (t
      (let ((unified-value (funcall (special-unif-function att)
				    arg1 arg2 new-path2)))
	(cond
	 ((eq unified-value :fail)
	  (*fail* fail frame path1 path2 pair
		  "Fail on unifying special attribute ~s:~
                 ~s with ~s at level ~s"
		  att arg1 arg2 path1))
	 (t
	  (trace-format (frame-trace-flags frame) frame 0
			"Unifying ~s: ~s with ~s" 
			att arg1 arg2)
	  (trace-format (frame-trace-flags frame) frame 0
			"Trying ~s: ~s" att unified-value)
	  ;; Same logic as unify-pathij:
	  (cond
	   ((and (path-p (safe-second pair1)) (path-p (safe-second pair2)))
	    ;; we have now 4 pairs involved:
	    ;; (p1 {x}) (p2 {y}) (x v1) (y v2)  [p2 is in the grammar]
	    ;; -->  (p1 {x}) (x u(v1,v2)) (y {x}) 
	    (update-pair pointed-pair1 unified-value p-path1 frame) 
	    (unless (eq pointed-pair1 pointed-pair2)
	      (update-pair pointed-pair2 new-path1 p-path2 frame)))
	   ((path-p (safe-second pair1))
	    ;; (p1 {x}) (p2 v2) (x v1)  [p2 is in the grammar]
	    ;; -->  (p1 {x}) (x u(v1,v2))
	    (update-pair pointed-pair1 unified-value p-path1 frame))
	   ((path-p (second pair2))
	    ;; (p1 v1) (p2 {y}) (y v2)
	    ;; -->  (p1 u(v1,v2))  (y {p1})
	    (update-pair pointed-pair1 unified-value p-path1 frame)
	    (unless (eq pointed-pair1 pointed-pair2)
	      (update-pair pointed-pair2 new-path1 p-path2 frame)))
	   (t
	    ;; (p1 v1) (p2 v2)
	    ;; -->  (p1 u(v1,v2))
	    (update-pair pointed-pair1 unified-value new-path1 frame)))
	  (unify fd1 (cdr fd2) path1 path2
		 frame fail success :pair pair))))))))


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

;; ----------
;; RELOCATE-SPECIAL: patch the value of a special feature.
;; ----------

(defun relocate-special (q1 val feature rpath tpath cpath)
  (cond ((leaf-p val) val)
	((path-p val) 
	 (let* ((path (absolute-path val tpath)))
	   (multiple-value-bind (pointed-val point-to missing cycle) 
	       (gdc *input* path)
	     (declare (ignore missing))
	     (cond 
	      (cycle nil) ;; points to an ancestor so it must be to itself because
                          ;; a special cannot have descendants.
	      ((in-scope q1 point-to rpath)
	       (let ((new-phys (get-new-physical-path point-to)))
		 (cond 
		  ((null new-phys)
		   (record-new-physical-path point-to cpath)
		   (relocate-special q1 pointed-val feature rpath point-to cpath))
		  (t new-phys))))
	      (t (relocate-special q1 pointed-val feature rpath point-to cpath))))))
	((eq feature 'fset) (copy-tree val))
	((eq feature 'pattern) (relocate-pattern q1 val rpath tpath))
	((and (member feature *special-attributes*)
	      (functionp (special-relocater-function feature)))
	 (funcall (special-relocater-function feature) val))
	(t (copy-tree val))))

(defun relocate-pattern (q1 val rpath tpath &aux p)
  ;; Patch all values in a pattern val:
  ;; atoms remain atoms.
  ;; in-scope paths are truncated.
  ;; out-of-scope paths are removed.
  ;; dots and pound are kept identical.
  (mapcan #'(lambda (x)
	      (cond ((leaf-p x) (list x))
		    ((eq x 'dots) (list 'dots))
		    ((eq x 'pound) (list 'pound))
		    ((not (path-p x)) nil)
		    ((setf p (in-scope q1 (absolute-path x tpath) rpath))
		     (list (strip-prefix p rpath)))
		    (t nil)))
	  val))


;; -----------------------------------------------------------------------
;; Example of procedural unifier: unify-cset for user defined-csets.
;; -----------------------------------------------------------------------

;; A cset can be (cset (p1 ... pn)) in old syntax
;; or in new syntax: (cset ((= p1 ... pn) (+ q1 .. qm) (- r1 ... ri)))
;; The old syntax is interpreted as (cset ((= p1 ... pn)))
;; + is the add-list and - is the sub-list.
;; Anyone of the three lists can be omitted.

;; If the cset c1 is augmented in any way, set *added-cset* to T.

(defun unify-cset (c1 c2 path)
  (let* ((c1 (new-cset-syntax c1))
	 (c2 (new-cset-syntax c2))
	 (iml1 (make-absolute (cdr (safe-assoc '== c1)) path))
	 (iml2 (make-absolute (cdr (safe-assoc '== c2)) path))
	 (eql1 (make-absolute (cdr (safe-assoc '= c1)) path))
	 (eql2 (make-absolute (cdr (safe-assoc '= c2)) path))
	 (add1 (make-absolute (cdr (safe-assoc '+ c1)) path))
	 (add2 (make-absolute (cdr (safe-assoc '+ c2)) path))
	 (sub1 (make-absolute (cdr (safe-assoc '- c1)) path))
	 (sub2 (make-absolute (cdr (safe-assoc '- c2)) path)))
    (cond 
     ((null c1) 
      (setf *added-cset* (positive-cset c2))
      c2)
     ((null c2) c1)
     (t 
      (let* ((add (union add1 add2 :test #'path-equal))
	     (sub (union sub1 sub2 :test #'path-equal))
	     (iml (union iml1 iml2 :test #'path-equal))
	     (eql (cond ((null eql1) eql2)
			((null eql2) eql1)
			((set-equal eql1 eql2) eql1)
			(t :fail)))
	     (res 
	      (cond ((eq eql :fail) :fail)
		    ((intersection add sub :test #'path-equal) :fail)
		    ((null eql) 
		     (list (cons '== iml) (cons '+ add) (cons '- sub)))
		    ((intersection eql sub :test #'path-equal) :fail)
		    ((not (subsetp add eql :test #'path-equal)) :fail)
		    (t (list (cons '= eql))))))
	;; Did we extend the cset?
	(setf *added-cset* 
	      (or (and (null eql1) eql2)
		  (set-difference add2 add1 :test #'path-equal)
		  (set-difference iml2 iml1 :test #'path-equal)))
	res)))))

(defun set-equal (s1 s2)
  "True if s1 and s2 have same elements in any order"
  (when (and (consp s1) (consp s2))
    (and (subsetp s1 s2 :test #'equality) (subsetp s2 s1 :test #'equality))))

;; old syntax for cset
(defun check-old-cset (value)
  (and (listp value)
       (every #'(lambda (x) (or (leaf-p x) (path-p x))) value)))

;; convert to new syntax for csets
(defun new-cset-syntax (value)
  (cond ((null value) nil)
	((eq value 'none) (list (cons '= nil)))
	((check-old-cset value) (list (cons '= value)))
	((check-cset value) value)
	(t (error "Invalid cset value: ~s" value))))
	
;; value in a (cset value) pair
(defun check-cset (value)
  (or (check-old-cset value)
      (and (listp value)
	   (every #'check-old-cset value)
	   (<= (length value) 3)
	   (every #'(lambda (e) (member (car e) '(+ - = ==))) value))))

;; T if cset contains an = or a + specification.
(defun positive-cset (value)
  (or (safe-assoc '= value) (safe-assoc '+ value) (safe-assoc '== value)))

(defun relocate-cset (q1 val rpath tpath)
  ;; Patch all values in a cset val:
  ;; atoms remain atoms.
  ;; in-scope paths are truncated.
  ;; out-of-scope paths are removed.
  ;; dots and pound are kept identical.
  (let ((nval (new-cset-syntax val)))
    (mapcar #'(lambda (val)
		(relocate-pattern q1 val rpath tpath)) 
	    nval)))


;; Set cset as a special feature
(define-procedural-type 'cset #'unify-cset
  :syntax #'check-cset
  :relocater #'relocate-pattern)


;; -----------------------------------------------------------------------
(provide "$fug5/type")
;; -----------------------------------------------------------------------
