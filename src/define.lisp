;;; -*- Mode:Lisp; Syntax:Common-Lisp; Package: FUG5 -*-
;;; -----------------------------------------------------------------------
;;; File:         define.l
;;; Description:  Definition of grammars in a modular way
;;; Author:       Michael Elhadad
;;; Created:      18 Dec 1991
;;; Modified:     23 Dec 1991: added draw-types, postscript-types
;;;                            added def-grammar
;;;               25 Dec 1991: fixed refresh-fug for empty branches
;;; Package:      FUG5
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

(defun clear-grammar ()
  (clrhash *fuf-systems*)
  (clrhash *fuf-conjs*)
  (setf *fuf-refreshed* nil))

(defmacro def-alt (name &rest args)
  `(multiple-value-bind (old found) (gethash ',name *fuf-systems*)
     (declare (ignore old))
     (when found
       (format t "Redefining alt ~s~%" ',name))
     (setf (gethash ',name *fuf-systems*) ',(list* 'alt name args)
	   *fuf-need-refresh* t)))

(defmacro def-conj (name &rest args)
  `(multiple-value-bind (old found) (gethash ',name *fuf-conjs*)
     (declare (ignore old))
     (when found
       (format t "Redefining conjunct ~s~%" ',name))
     (setf (gethash ',name *fuf-conjs*) ',args)
     (setf *fuf-need-refresh* t)))

(defmacro def-grammar (name args &rest body)
  `(progn
     (defun ,name ,args
       (setf *fuf-need-refresh* t)
       ,@body)
     (setf *u-grammar* (,name))))

(defun use-grammar (g)
  (let ((pair (assoc g *fuf-refreshed*)))
    (if *fuf-need-refresh*
      (let ((refreshed (refresh-fug g)))
	(if (consp pair)
	  (setf (cdr pair) refreshed)
	  (push (cons g refreshed) *fuf-refreshed*))
	(setf *fuf-need-refresh* nil)
	refreshed)
      (if (consp pair)
	(cdr pair)
	g))))

(defun get-grammar (grammar &key (expansion t))
  ;; Check if this is a def-alt or a def-conj
  (cond
   ((consp grammar)
    (if expansion (use-grammar grammar) grammar))
   (t
    (multiple-value-bind (alt found) (gethash grammar *fuf-systems*)
      (if found
	(if expansion
	  (refresh-fug (list alt))
	  (list alt))
	(multiple-value-bind (conj found) (gethash grammar *fuf-conjs*)
	  (if found
	    (if expansion
	      (refresh-fug conj)
	      conj)
	    (progn
	      (format *error-output* "~s is not a grammar.~%" grammar)
	      nil))))))))

;; Use :! and :& notation within tests and fds as well.
(defun get-fd (fd)
  (refresh-fug (if (symbolp fd) (get-test fd) fd)))


;; To avoid infinite expansions, keep stack of names being expanded.
;; Check for tracing flags | opt | special att
(defun refresh-fug (g &optional accu stack-alt stack-conj (rev t))
  "Expand all (! name) and (& name) to their denotation in a fug.
   Regular walk through an fd with disjunctions."
  (cond
   ((null g) (if rev (reverse accu) accu))
   ((leaf-p g) g)
   ((path-p g) g)
   ((not (consp g))
    (error "FUF error: Ill-formed grammar in refresh-fug.~%~
            alts = ~s~%conj = ~s~%"
	   (reverse stack-alt) (reverse stack-conj)))
   ((fd-p-flag (car g))   ; a valid tracing flag
    (refresh-fug (cdr g) (cons (car g) accu) stack-alt stack-conj rev))
   ((not (consp (car g)))
    (error "FUF error: Ill-formed grammar in refresh-fug.~%~
            alts = ~s~%conj = ~s~%"
	   (reverse stack-alt) (reverse stack-conj)))
   ((eq (caar g) :!)
    (multiple-value-bind
	(alt found) (gethash (cadar g) *fuf-systems*)
      (cond
       ((member (cadar g) stack-alt)
	(error "Recursive expansion: ~s uses itself!" (cadar g)))
       (found
	(refresh-fug
	 (cdr g)
	 (append (refresh-fug (list alt) nil
			      (cons (cadar g) stack-alt)
			      stack-conj nil)
		 accu)
	 stack-alt stack-conj rev))
       (t
	(format t "Warning: alt ~s is not defined.~%" (cadar g))
	(refresh-fug (cdr g) accu stack-alt stack-conj rev)))))
   ((eq (caar g) :&)
    (multiple-value-bind
	(conj found) (gethash (cadar g) *fuf-conjs*)
      (cond
       ((member (cadar g) stack-conj)
	(error "Recursive expansion: ~s uses itself!" (cadar g)))
       (found
	(refresh-fug
	 (cdr g)
	 (append (refresh-fug conj nil stack-alt
			      (cons (cadar g) stack-conj) nil)
		 accu)
	 stack-alt stack-conj rev))
       (t
	(format t "Warning: conj ~s is not defined.~%" (cadar g))
	(refresh-fug (cdr g) accu stack-alt stack-conj rev)))))
   ((or (eq (caar g) 'alt) (eq (caar g) 'ralt))
    ;; syntax is (alt {flags} (branches))
    ;; recurse on branches
    (let* ((branches (branches (car g)))
	   (new-br (mapcar #'(lambda (br)
			       (refresh-fug br nil stack-alt stack-conj))
			   branches)))
      (setf new-br (remove-if #'null new-br))
      (if (null new-br)
	(refresh-fug (cdr g) accu stack-alt stack-conj rev)
	(refresh-fug (cdr g)
		     (cons (append (butlast (car g)) (list new-br)) accu)
		     stack-alt stack-conj rev))))
   ((eq (caar g) 'opt)
    ;; Syntax is (opt {flags} fd)
    ;; Recurse on fd
    (let* ((fd (option (car g)))
	   (new-fd (refresh-fug fd nil stack-alt stack-conj)))
      (if (null new-fd)
	(refresh-fug (cdr g) accu stack-alt stack-conj rev)
	(refresh-fug (cdr g)
		     (cons (append (butlast (car g)) (list new-fd)) accu)
		     stack-alt stack-conj rev))))
   ((or (leaf-p (cadar g))
	(path-p (cadar g))
	(member (caar g) *special-attributes*)
	(and (path-p (caar g))
	     (member (car (path-last (caar g))) *special-attributes*)))
    (refresh-fug (cdr g)
		 (cons (car g) accu) stack-alt stack-conj rev))
   ;; Recursive call to sub-fd
   (t
    (refresh-fug (cdr g)
		 (cons (list (caar g)
			     (refresh-fug (cadar g)
					  nil stack-alt stack-conj))
		       accu)
		 stack-alt stack-conj rev))))


(defun fd-children (g)
  (if (consp g)
    (fug-children g)
    (multiple-value-bind (alt found) (gethash g *fuf-systems*)
      (if found
	(fug-children (list alt))
	(multiple-value-bind (conj found) (gethash g *fuf-conjs*)
	  (if found
	    (fug-children conj)))))))


(defun fug-children (g &optional accu (rev t))
  "Find all children of a grammar in terms of expansion.
   Regular walk through an fd with disjunctions."
  (cond
   ((null g) (if rev (nreverse accu) accu))
   ((leaf-p g) nil)
   ((path-p g) nil)
   ((not (consp g)) (error "Ill-formed grammar in refresh-fug"))
   ((fd-p-flag (car g))   ; a valid tracing flag
    (fug-children (cdr g) accu rev))
   ((not (consp (car g))) (error "Ill-formed fd in refresh-fug"))
   ((or (eq (caar g) :!) (eq (caar g) :&))
    (fug-children (cdr g) (cons (cadar g) accu) rev))
   ((or (eq (caar g) 'alt) (eq (caar g) 'ralt))
    ;; recurse on branches
    (let* ((branches (branches (car g)))
	   (c (mapcan #'(lambda (b) (fug-children b nil nil))
		      (reverse branches))))
      (fug-children (cdr g) (append c accu) rev)))
   ((eq (caar g) 'opt)
    ;; Syntax is (opt {flags} fd) - Recurse on fd
    (let* ((fd (option (car g))))
      (fug-children (cdr g)
		    (append (fug-children fd nil nil) accu) rev)))
   ((or (leaf-p (cadar g))
	(path-p (cadar g))
	(member (caar g) *special-attributes*)
	(and (path-p (caar g))
	     (member (car (path-last (caar g))) *special-attributes*)))
    (fug-children (cdr g) accu rev))
   ;; Recursive call to sub-fd
   (t
    (fug-children (cdr g)
		  (append (fug-children (cadar g) nil nil) accu) rev))))


;; ---------------------------------------------------------------------------
;; DRAW-GRAMMAR:  pretty printer of tree for grammar systems relations
;; ---------------------------------------------------------------------------
(defun draw-grammar (&optional (root *u-grammar*))
  (let ((ids (make-hash-table)))
    (labels ((fd-id (node)
		    (if (symbolp node)
		      node
		      (multiple-value-bind (id found) (gethash node ids)
			(if found
			  id
			  (setf (gethash node ids) (gensym)))))))
      (print-structure
       (list root)
       :max-name 8
       :node-name #'(lambda (x) (if (symbolp x)
				  (symbol-name x)
				  "grammar"))
       :node-children #'fd-children
       :node-id  #'fd-id))))


;; ---------------------------------------------------------------------------
;; DRAW-TYPES:  pretty printer of type definitions in grammar
;; ---------------------------------------------------------------------------
(defun draw-types (&optional root)
  (let ((roots (cond ((consp root) root)
		     ((null root) (type-roots))
		     ((member root *typed-features*) (list root))
		     (t (error "~s is not a type." root)))))
    (print-structure
     roots
     :max-name 8
     :node-children #'(lambda (n) (get n :feature-type))))
  (values))

(defun type-roots ()
  "Return types that do not have parents in type hierarchy"
  (let ((desc (loop for x in  *typed-features*
		    append (get x :feature-type))))
    (nreverse (set-difference *typed-features* desc))))


;; ---------------------------------------------------------------------------
;; TYPES-POSTSCRIPT:  postscript pretty printer of type definitions in grammar
;; ---------------------------------------------------------------------------
(defun types-postscript (root filename &key (shrink t) insert)
  (require "$fug5/psgraph" "$fug5/psgraph")
  (let* ((roots (cond ((consp root) root)
		      ((null root) (type-roots))
		      ((member root *typed-features*) (list root))
		      (t (error "~s is not a type." root))))
	 (lr (length roots)))
    (cond ((> lr 1)
	   (setf root :top)
	   (setf (get root :feature-type) roots))
	  ((= lr 1) (setf root (car roots)))
	  (t (return-from types-postscript (values))))
    (with-open-file (*standard-output* filename :direction :output
				       :if-does-not-exist :create
				       :if-exists :supersede)

      (psgraph root
	       #'(lambda (n) (get n :feature-type))
	       #'(lambda (n) (list n))
	       shrink           ;; fit on one page
	       insert)))        ;; insert in another PS file
  (values))

;; ---------------------------------------------------------------------------
;; FUF-POSTSCRIPT:  postscript pretty printer of tree using psgraph.
;; ---------------------------------------------------------------------------
;; The psgraph function is provided by Joseph Bates @ CMU CSD.
(defun fuf-postscript (root filename &key (shrink t) insert)
  (require "$fug5/psgraph" "$fug5/psgraph")
  (with-open-file (*standard-output* filename :direction :output
				     :if-does-not-exist :create
				     :if-exists :supersede)
    (psgraph root
	     #'fd-children    ;; children
	     #'(lambda (x)    ;; info (must be a list)
		 (if (consp x)
		   (list :top-level)
		   (list x)))
	     shrink           ;; fit on one page
	     insert))         ;; insert in another PS file
  (values))


;; ---------------------------------------------------------------------------
;; PRINT-STRUCTURE: Adapted from file $fug/tree.l
;;                  Generic tree printer.
;; lroots: list of root nodes.
;; tab: a string for original identation
;; out: a open stream for output
;; node-id: a function which returns a symbol id unique for each node
;; node-name: a function which returns a string name for each node
;; node-children: a function which returns the list of children nodes
;; ---------------------------------------------------------------------------
(defmacro while (test &rest body) `(do () ((not ,test)) ,@body))

(defun print-structure (lroots
			&key
			(tab "")
			(max-name *max-name-length*)
			(out t)
			(node-id       #'identity)
			(node-name     #'symbol-name)
			(node-children #'(lambda (x) (get x 'children))))
  "Graphically display a tree on terminal.
   out can be an open stream for the output."
  (while lroots
    (let* ((root (pop lroots))
	   (root-id (funcall node-id root))
	   (root-name (funcall node-name root))
	   (children  (funcall node-children root))
	   (tab tab)
	   (slen nil))
      (if (get root-id :in-loop)
	(format out "~A|- loop to ~A~%" tab root-name)
	(unwind-protect
	    (progn
	      (format out "~A|- ~A" tab root-name)
	      (setf (get root-id :in-loop) t)
	      (if (null children)
		(format out "~%")
		(progn
		  (setf slen (length root-name))
		  ;; if name is too long, go to next line
		  (when (> slen max-name)
		    (setf slen 1)
		    (format out "~%~A|   " tab))
		  (format out " +~%")
		  (setf tab
			(concatenate
			 'string tab
			 (if lroots "|" " ")
			 (make-string (+ slen 3)
				      :initial-element #\space)))
		  (print-structure children
				   :tab tab
				   :max-name max-name
				   :out out
				   :node-id node-id
				   :node-name node-name
				   :node-children node-children)
		  (format out "~A~%" tab))))
	  (remprop root-id :in-loop)))))
  (values))



;; ------------------------------------------------------------
(provide "$fug5/define")
;; ------------------------------------------------------------
