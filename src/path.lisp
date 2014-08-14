;;; -*- Mode:Lisp; Syntax:Common-Lisp; Package:FUG5 -*-
;;; -----------------------------------------------------------------------
;;; File:         PATH.L
;;; Description:  PATH unification with UNDO and SUCCESS and no STACK and 
;;;               unif. not at top level.
;;; Author:       Michael Elhadad
;;; Created:      02-Nov-88
;;; Modified:     29-Nov-88
;;;               30 Apr 90 - added *special-attributes*
;;;                         - moved exports to fug5
;;;               02 May 90 - added FSET to gdp and gdpp
;;;               03 May 90 - added FSET to build-path
;;;               04 May 90 - added specials and FSET to unify-pathij
;;;               10 May 90 - made path special type.
;;;               17 May 90 - moved macros to macros.l
;;;               04 Jun 90 - fixed bug in build-path when leaf={}
;;;               20 Jun 90 - made pathij ok with equations in pair2.
;;;               02 Jul 90 - removed pathij fctns.
;;;               20 Feb 91 - added path-equal and equality - fixed bug on 386
;;;               05 May 91 - made incr-gdp act correctly on a path 
;;;                           added safe-path-extend
;;;                           (to allow paths in patterns)
;;;               16 Aug 91 - fixed safe-path-extend.
;;;               18 Aug 91 - use attr-p instead of symbolp
;;;               04 Sep 91 - add ^n and ~n notations in paths
;;;               05 Dec 91 - moved make-absolute here from top.l
;;;               06 Dec 91 - fixed looping bug in gdp on ((e {e c})) {e}.
;;;               10 Dec 91 - used nifty format control string in print-path
;;;               09 Jun 93 - add ^~ notation in paths: go up to beginning
;;;                           of list.
;;;               27 Jan 94 - fix loop detection in gdp: ((a {a b}) (c {a d}))
;;;               31 May 94 - added initial arg val to build-fd-from-path.
;;;               06 Jun 94 - redid gdp and gdpp using gdc.
;;;                         - make build-id-from-path return 2 args and take an
;;;                           initial value.
;;; Package:      FUG5
;;; Status:       Experimental
;;; Inline:       path-null, path-car, path-cdr, path-last, path-reverse, 
;;;               path-push, path-pop, path-extend, path-append,
;;;               path-butlast, the-last-arc-of-path
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
(format t "Path unifier...~%")

;;; --------------------------------------------------
;;; PATH Utilities (inline)
;;; --------------------------------------------------

(proclaim '(inline path-null path-car path-cdr path-cons path-last
		   path-extend path-reverse path-push path-pop path-append
		   path-butlast path-nthcdr path-len path-prefix
		   path-relative-p the-last-arc-of-path))

(defun path-null (path)
  (null (path-l path)))

(defun path-car (path)
  (car (path-l path)))

(defun path-cdr (path)
  (make-path :l (cdr (path-l path))))

(defun path-cons (x path)
  (make-path :l (cons x (path-l path))))

(defun path-last (path)
  (last (path-l path)))

(defun path-extend (path arc)
  "Return a path with arc added at the end of path"
  (make-path :l (append (path-l path) (list arc))))

(defun path-reverse (path)
  "Return an arc with the reverse path {1 2 3} --> {3 2 1}"
  (make-path :l (reverse (path-l path))))

(defun path-push (item path)
  "Add an arc at the beginning of path."
  (progn (push item (path-l path)) path))

(defun path-pop (path)
  "Pop an arc from the top of path."
  (pop (path-l path)))

(defun path-append (path1 path2)
  "Return the append of two paths"
  (make-path :l (append (path-l path1) (path-l path2))))

(defun path-butlast (path &optional (n 1))
  "return a path with butlast steps"
  (make-path :l (butlast (path-l path) n)))

(defun path-nthcdr (n path)
  (make-path :l (nthcdr n (path-l path))))

(defun path-len (path)
  (length (path-l path)))

(defun path-prefix (tpath spath)
  (equal 0 (search (path-l spath) (path-l tpath))))

(defun path-relative-p (path)
  (eq '^ (car (path-l path))))

;; the dumb go-down-path-pair. Returns the last arc of path even if
;; it is a dummy arc (second is a path).
;; Can also return ANY, NONE or NIL.
;; FD MUST BE *INPUT*.
(defun the-last-arc-of-path (fd path)
  (let ((penultimate-node (gdp fd (path-butlast path))))
     (if (leaf-p penultimate-node)
	 penultimate-node
	 (safe-assoc (car (last (path-l path))) penultimate-node))))



;; ------------------------------------------------------------

(defun absolute-path (uppath path)
  "translates a relative path (uppath, containing ^) to an absolute path
  (beginning at the root of the fd)"
  (let ((uplist (path-l uppath)))
    (if (or (eq '^ (car uplist)) (eq '^~ (car uplist)))
	(absolute-path-from-relative uplist (path-l path))
      uppath)))

;; ^ goes up one level
;; ^~ goes up a sequence of the form (x cdr cdr cdr car) (x cdr* car).
;; If ^~ is used not under car, ^~ is equivalent to ^.
;; So (a {^~ b}) is equivalent to (a {^ b}).
(defun absolute-path-from-relative (uplist pathlist)
  (cond 
   ((eq '^ (car uplist))
    (absolute-path-from-relative (cdr uplist) (butlast pathlist)))
   ;; go up a sequence of the form (cdr cdr cdr car) plus one level.
   ((eq '^~ (car uplist))
    (let ((rp (reverse pathlist)))
      (when (eq (car rp) 'car) 
	(setf rp (cdr rp))
	(while (eq (car rp) 'cdr) (pop rp)))
      (pop rp)
      (absolute-path-from-relative (cdr uplist) (reverse rp))))
   (t (make-path :l (append pathlist uplist)))))

(defun safe-path-extend (path constituent)
  "A safe version of path-extend: add constituent at the end of path - does
   the right thing according to the type of constituent.
   Must be called from linearize on the value of patterns."
  (cond ((attr-p constituent) (path-extend path constituent))
	((path-p constituent) (absolute-path constituent 
					     (path-extend path 'pattern)))
	(t (error "safe-path-extend: constituent should be either a path or a symbol.~@
Probable cause of error: bad value in a pattern of the grammar."))))


(defun make-absolute (lpath path)
  "Make a list of relative paths absolute with convention that a path <^ a>
can be represented as simply a."
  (let ((butlast (path-butlast path)))
    (mapcar #'(lambda (p)
		(if (path-p p) 
		  (absolute-path p path) 
		  (path-extend butlast p)))
	    lpath)))



;; ------------------------------------------------------------
;; Functions to deal with PATHs.
;; 
;; General idea : path is a named pointer to any node in an fd.
;; An fd is a dag with a single root (path = nil).
;; NOTE : 1 - All these functions don't work if fd has alts or opts.
;;            (this is not a real limitation since alts can be factored out)
;;        2 - Since we can go from everywhere to anywhere, I use the
;;            global var *INPUT* as the total fd (beginning at the root).
;; ------------------------------------------------------------


#+ignore(defun gdp (fd path)
  "given an fd (representing a connected dag) and a path, gives
  the value of the node pointed by the path (the arc leaving the node).
  The function uses the global var *input* set to the total fd
  when it encounters a pointer up on its way down (GO-DOWN-PATH).
  NOTE : always returns a well-formed fd (possibly <none>).
         may cause an error if fd contains vicious cycles"

  ;; FD MUST BE *INPUT* INITIALLY
  ;; Check for cycles in the way. Mark our way in already-tried.
  ;; Returns none if encounters an atom on its way down, and path is not nil.
  ;; NOTE: if the path leads to a non-existent sub-fd, gdp returns:
  ;; NONE: if the fd cannot be extended to include such a sub-fd
  ;;       (that's when we meet an atom on the way down)
  ;; ANY : if the fd MUST be extended to include such a sub-fd (and exactly
  ;;       this sub-fd, that is only when the value is ANY)
  ;; NIL : otherwise (that is, an UNRESTRICTED fd).
  ;; FSET: check for FSET on the way.  If FSET is nil, nothing.
  ;;       otw, if we stay within FSET fine, otw it's like having a none.
  ;;       NOTE: FSET is always authorized (even if not mentioned in the
  ;;       FSET)
  ;; SPECIALs: three cases 
  ;; 1/ (att <a b S c d>)  <==> (att none)  [cannot go below a special]
  ;; 2/ (S <a b S>) is ok. [call special-unify]
  ;; 3/ (att <a b S>)    <==> (att none) [cannot map non-special to special]
  (do ((fd fd) 
       (already-tried (list (copy-path path)))
       (path path)
       (reverse-current-path (make-path))
       (current-fset (find-fset fd))
       (arc-tried nil))
      ((and (not (path-p fd)) (path-null path)) fd)
    (cond 
     ;; Do we stop at any or do we go below it: below is unspecified.
     ((eq fd 'any) (if (path-null path) (return 'any) (return nil)))
     ((eq fd 'given) (if (path-null path) (return 'given) (return nil)))
     ((eq fd nil) (return nil))

     ;; Below any other atom is impossible: none
     ((leaf-p fd) (return 'none))

     ;; Need to go up to the root and go down again to the indirection.
     ;; Keep track of each path visited to avoid loops.
     ;; When a loop is encountered, (ie, two paths pointing to each other),
     ;; break it by putting nil to one of the paths.
     ((path-p fd)
      ;; A non-special cannot point to a special: check validity here.
      (let ((from (car arc-tried))
	    (to   (car (last (path-l fd)))))
	(cond 
	 ((eq from to))  ;; fine - type equality
	 ((or (member from *special-attributes*)
	      (member to *special-attributes*))
	  (return 'none))))
      ;; ok proceed...
      (let* ((cpath (path-reverse reverse-current-path))
	     (path-prefix (if (eq '^ (path-car fd))
			      (absolute-path fd cpath)
			    fd)))
	(setf path (path-append path-prefix path))
	;; This is a loop: ({a b c} {a b c d e}) - cut it.
	(when (or (path-prefix path-prefix cpath)
		  (path-prefix cpath path-prefix)
		  (member path-prefix already-tried :test #'path-equal))
	  ;; **************************
	  ;; Does this line cause trouble? 
	  ;; How does it interact with backtraking?  Can we loose info?
	  ;; It is needed for (gdp '((s ((p ((l {p l}))))) (p {s p})) {p l r})
	  ;; or (gdp '((e {e c})) {e f}) -> ((e nil))
	  ;; **************************
	  (format t "~%~%HERE!!!!!!!!! arc-tried = ~s - path = ~s~%~%" arc-tried path)
	  (setf (second arc-tried) nil)
	  (return nil))
	(push path already-tried))
      (setf reverse-current-path (make-path))
      (setq fd *input*))

     ;; Special attributes are also atomic: cannot go below them.
     ;; but can have a path as value.
     ((member (path-car path) *special-attributes*)
      (cond ((cdr (path-l path)) (return 'none))
	    ((or (null current-fset) 
		 (member (path-car path) current-fset)
		 (eq (path-car path) 'fset))
	     (setf arc-tried (safe-assoc (path-car path) fd))
	     (path-push (path-car path) reverse-current-path)
	     (setf path (path-cdr path))
	     (setf fd (second arc-tried)))
	    (t (return 'none))))

     ;; Are we within the authorized FSET? If so, go down one level.
     ((or (null current-fset)
	  (member (path-car path) current-fset))
      (setf arc-tried (safe-assoc (path-car path) fd))
      (path-push (path-car path) reverse-current-path)
      (setf path (path-cdr path))
      (setf fd (second arc-tried))
      (setf current-fset (find-fset fd)))

     ;; Not within the authorized FSET: value is none.
     (t (return 'none)))))

(defun gdp (fd path)
  (multiple-value-bind (val phys missing node arc) (gdc fd path)
    (declare (ignore phys missing node arc))
    val))

;; the-last-arc-of-path: MACROS
;; the dumb go-down-path-pair. Returns the last arc of path even if
;; it is a dummy arc (second is a path).
;; Can also return ANY, NONE or NIL.
;; FD MUST BE *INPUT*.

#+ignore(defun gdpp (fd path &optional (frame dummy-frame))
  (multiple-value-bind (val phys missing node arc) (gdc fd path)
    (declare (ignore val))
    (if (or (path-null missing) (eq val 'none))
      arc
      (add-missing node arc phys missing frame))))

(defun gdpp (fd path &optional (frame dummy-frame))
  (multiple-value-bind (val phys missing node arc) (gdc fd path)
    (cond ((path-null missing) arc)
          ((eq val 'none) 'none)
          (t (add-missing node arc phys missing frame)))))

(defun add-missing (node arc phys missing frame)
  (let ((fset (find-fset node)))
    (cond ((member node '(nil any))
	   (if (eq node 'any)
	     (push (make-test :test '(any-p path) :path phys)
		   (frame-tests frame)))
	   (multiple-value-bind (ext pair) (build-fd-from-path missing)
	     (update-pair arc ext phys frame)
	     pair))
	  ((leaf-p node) 'none)
	  ((and fset (not (member (path-car missing) fset)))
	   (let ((new-pair (list (path-car missing) 'none)))
	     (enrich node new-pair frame)
	     new-pair))
	  (t 
	   (multiple-value-bind (ext pair) (build-fd-from-path missing)
	     (nconc node ext)
	     pair)))))

#+ignore(defun gdpp (fd path &optional (frame dummy-frame))
  "given an fd (connected dag) and a path, returns the last arc of the
  path. Always returns a real arc (that is a pair whose second is not
  a path but a real value). If necessary, builds such an arc in *INPUT*.
  Can also return NONE, but not ANY.
  Takes FSET into account.
  FD MUST BE *INPUT*.
  (GO-DOWN-PATH-PAIR)"
  ;; NOTE: gdpp can modify *input* to extend it or to break cycles.
  ;; NOTE: probably does not work if *input* contains GIVEN.

  (cond 
   ((path-null path) (list '*TOP* fd))
   (t 
    (do* ((fd fd) 
	  (path path)
	  (arc (the-last-arc-of-path fd path))
	  (arc-tried arc))

	 ;; Exit condition: we have a real pair - the value is not a path,
	 ;; if it is an leaf-p other than nil, the pair is NONE (cannot find
	 ;; such a pair).  Should never return NIL, since in these cases it
	 ;; will extend the graph and return a pair (x nil).
	 ;; If arc is ANY, we can also extend the graph below it.
	 ((or (and arc (leaf-p arc) (not (eq arc 'any)))
	      (and arc (consp arc) (not (path-p (second arc)))))
	  (cond ((and arc-tried (consp arc-tried) (leaf-p arc))
		 (setf (second arc-tried) 'none)
		 arc-tried)
		((leaf-p arc) 'none)
		(t arc)))

	 ;; we come here  if: 1/ arc is ANY
	 ;;                   2/ (second arc) is a path 
	 ;;                   3/ arc is NIL.
	 ;; NOTE: it does not make sense to extend below a GIVEN (so we
	 ;; return NONE when GIVEN is the last arc).
	 (cond ((eq arc 'any)
		;; Extend by just one level: this could be done by
		;; BUILD-PATH but there is no reason to for simple case.
		;; KEEP TRACK of the fact that there used to be an ANY here
		;; by pushing the any-p constraint in the test list.
		;; NOTE: there cannot be an FSET in (att any).
		(setq arc (the-last-arc-of-path fd (path-butlast path)))
		(setf (second arc) (list (append (path-last path) (list nil))))
		(push (make-test :test '(any-p path) :path (path-butlast path))
		      (frame-tests frame))
		(setq arc (car (second arc))))

	       ;; (SECOND ARC) is a path: need to be careful with cycles. 
	       ;; Easiest thing to do is to call GDP completely to the end
	       ;; of path to break any cycle (just for the side effect).
	       (arc 
		(setq arc-tried arc)
		(setq path (absolute-path (second arc) path))
		(let ((pointed-to (gdp *input* path)))
		  ;; If pointed-to is a bound leaf-p, the conflation
		  ;; information is not necessary anymore.
		  (cond ((fd-boundp pointed-to)
			 (setf (second arc-tried) pointed-to)
			 (return arc-tried))
			(t
			 (setq arc (the-last-arc-of-path *input* path))
			 (setq fd *input*)))))

	       ;; ARC is NIL: extend the graph below it and return (x nil)
	       ;; Except if contradicts FSET on the way down.
	       (t   
		(build-path *input* path frame)
		(setq arc (the-last-arc-of-path *input* path))
		(setq fd *input*)))))))
  

;; find-path-to-leaf
(defun fptl (fd path)
  "given an fd (connected dag) and a path, returns the path to the last
  node in path which exists in the dag and the rest of the path.
  (2 values).
  FD is actually meant to be *input*.
  Refers to global var *INPUT* containing the complete dag sometimes"
  (fptl-aux fd (make-path) path))

(defun fptl-aux (fd where-in-top path)
  "Cf fptl"
  (if (or (path-null path) (leaf-p fd))
      (values (path-reverse where-in-top) path)
    (let ((node (safe-assoc (path-car path) fd)))
      (cond 
       ((null node) (values (path-reverse where-in-top) path))
       ((path-p (second node))
	(fptl-aux *input* (make-path)
		  (path-append 
		   (absolute-path 
		    (second node) 
		    (path-reverse (path-push (path-car path) where-in-top)))
		   (path-cdr path))))
       (t 
	(fptl-aux (second node) 
		  (path-push (path-car path)  where-in-top)
		  (path-cdr path)))))))


(defun build-fd-from-path (path &optional val pair)
  "given a path, builds an fd from scratch, which is a linear string, with
   val at the leaf."
  (cond ((path-null path) (values val pair))
	((path-null (path-cdr path)) 
	 (let ((pair (list (path-car path) val :e)))
	   (values (list pair) pair)))
	(t 
	 (multiple-value-bind (fd pair) (build-fd-from-path (path-cdr path) val)
	   (values (list (list (path-car path) fd)) pair)))))

(defun build-path (fd path frame)
  "given an fd and a path, physically modifies fd so that path points
  to a real node by adding all the missing nodes. 
  Returns none if addition of nodes is not possible"
  (declare (special *input*))
  (multiple-value-bind (leaf below) (fptl fd path)
    (let* ((node  (gdpp fd leaf frame)) ;; must be a real node given fptl
	   (fset  (find-fset (second node))))
      (when below
	(cond ((member (second node) '(nil any))
	       (if (eq (second node) 'any)
		   (push (make-test :test '(any-p path) :path leaf)
			 (frame-tests frame)))
	       (update-pair node (build-fd-from-path below) leaf frame))
	      ((leaf-p (second node)) 
	       'none)
	      ((and fset (not (member (path-car below) fset)))
	       (let ((new-pair (list (path-car below) 'none)))
		 (enrich (second node) new-pair frame)
		 new-pair))
	      ;; (second node) is never a path given gdpp
	      (t (nconc (second node) (build-fd-from-path below))))))))



;; --------------------------------------------------
;; TOP level traversal functions
;; To call at the top level on any FD.
;; --------------------------------------------------

(defun incr-gdp (constituent fd path)
  "Incremental gdp: if value of constituent can be found easily in
       fd, return it, otw, call full gdp from root."
  (declare (special *input*))
  (cond ((leaf-p fd) 'none)
	((attr-p constituent)
	 (let ((value (safe-assoc constituent fd)))
	   (cond ((null value) nil)
		 ((path-p (second value))
		  (gdp *input* (path-extend path constituent)))
		 (t (second value)))))
	((path-p constituent)
	 (gdp *input* (absolute-path constituent path)))
	(t (error "Argument to incr-gdp should be an atom or a path.~@
                   Probable cause of error: bad value in a pattern."))))

(defun top-gdp (fd path)
  (let ((*input* (ban-path-as-total fd)))
    (declare (special *input*))
    (gdp *input* path)))

(defun top-gdpp (fd path)
  (let ((*input* (ban-path-as-total fd)))
    (declare (special *input*))
    (gdpp *input* path (make-frame))))

;; To be called in debugger when %break% is used.
;; Like gdp but cleans up its value.
;; Also accepts path as a list instead of a path if {} is not recognized.
(defun path-value (path)
  (declare (special *input*))
  (let ((path (if (path-p path) path (make-path :l path))))
    (filter-flags (gdp *input* path))))

(defun set-path-value (path fd &optional (cset-attribute *cset-attribute*))
  (declare (special *input*))
  (let* ((path (if (path-p path) path (make-path :l path)))
	 (fd (prep-input fd cset-attribute path))
	 (pair (gdpp *input* path)))
    (setf (second pair) fd)))
  

;; -----------------------------------------------------------------------
(provide "$fug5/path")
;; -----------------------------------------------------------------------

