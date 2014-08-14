;;; -*- Mode:Lisp; Syntax:Common-Lisp; Package: FUG5 -*-
;;; -----------------------------------------------------------------------
;;; File:         fd-to-graph2.l
;;; Description:  A buggy and fast version of relocate/insert-fd
;;; Author:       Michael Elhadad
;;; Created:      25 Jan 1996
;;; Modified:     
;;; Package:      FUG5
;;; -----------------------------------------------------------------------

(in-package "FUG5")

;; ------------------------------------------------------------
;; RELOCATE: grab a sub-fd from a total fd and make it a 
;; stand-alone total fd (resolve paths outside the sub-fd).
;; NOTE: Relocate is NOT smart about csets and patterns (it does not follow
;; the paths to resolve them and update them).
;; ------------------------------------------------------------
;; Example:
;; (relocate '((a ((a1 {^ a2})
;;                 (a2 2)
;;                 (a3 {a a1})
;;                 (a4 {c})
;;                 (a5 {b})))
;;             (b {a a1})
;;             (c ((c1 1))))
;;           {a})
;; =>
;; ((a1 {^ a2})     <--- NOTE keep relative path
;;  (a2 2)
;;  (a3 {a1})       <--- NOTE updated path
;;  (a4 ((c 1)))    <--- NOTE resolved path
;;  (a5 2))         <--- NOTE loose conflation a5/a1 because went out of a
;;                            scope.

(defun relocate (total rpath)
  (let ((const (top-gdp total rpath)))
    (relocpairs total rpath (make-path) const (copy-tree const))))

(defun relocpair (total rpath cpath pair-ind pairs result)
  (let* ((pair (nth pair-ind pairs))
	 (feature (first pair))
	 (value (second pair)) 
	 (new-path (path-extend cpath feature))
	 (point-to (if (path-p value)
		     (absolute-path value (path-append rpath new-path))
		     nil)))
    (cond ((leaf-p value) 
	   result)
	  ;; Problem with pattern and cset having paths in their values.
	  ;; Copy special deals with it.
	  ((member feature *special-attributes*)
	   (setf (second (nth pair-ind (top-gdp result cpath)))
		 (copy-special value feature cpath)))
	  ((not (path-p value))
	   (relocpairs total rpath new-path value result))
	  ;; Preserve relative paths
	  ((and (path-relative-p value) (path-prefix point-to rpath))
	   result)
	  ;; Truncate in-scope paths
	  ((path-prefix point-to rpath)
	   (setf (second (nth pair-ind (top-gdp result cpath)))
		 (path-nthcdr (path-len rpath) value))
	   result)
	  ;; Resolve out-of-scope paths
	  (T 
	   (let ((pointed-value (top-gdp total point-to)))
	     (setf (second (nth pair-ind (top-gdp result cpath)))
		   (copy-tree pointed-value))
	     (relocpairs total rpath new-path pointed-value result))))))

(defun relocpairs (total rpath cpath pairs result)
  (if (leaf-p pairs)
    pairs
    (loop for pair in pairs
          for pair-ind = 0 then (+ pair-ind 1)
	  do (relocpair total rpath cpath pair-ind pairs result)
	  finally (return result))))


;; ------------------------------------------------------------
;; INSERT-FD: reverse of relocate, insert a total fd within a larger total
;; fd under path subfd-path.
;; ------------------------------------------------------------
;; Example: 
;; (insert-fd '((a {b}) (b 1) (c {^ b}))
;;            '((b 2))
;;            {c})
;; =>
;; ((b 2)
;;  (c ((a {c b})  <------ NOTE updated path.
;;      (b 1)
;;      (c {c b}))))  <--- NOTE relative path is resolved
;;

(defun insert-fd (fd total subfd-path)
  (filter-flags (u total (insert-empty-fd fd subfd-path))))

;; Just put an fd under a path 
;; Example: (insert-empty-fd '((a {b}) (c {^ a})) {x y})
;; =>
;; ((x ((y ((a {x y b})     <--- NOTE updated path
;;          (c {^ a}))))))  <--- NOTE preserve relative path
;;
(defun insert-empty-fd (fd path)
  (if (path-null path)
    fd
    (let* ((total (build-fd-from-path path))
	   (pair (the-last-arc-of-path total path)))
      ;; Get rid of all flags after second
      (if (or (leaf-p fd) (path-p fd))
	(setf (cdr pair) (list fd))
	(setf (cdr pair) (list (insert-patch fd path path))))
      total)))

;; Copy the fd appearing at level path and patches the paths according to
;; relocation under path rpath in total fd.
(defun insert-patch (fd path rpath)
  (cond ((null fd) fd)
	(t (cons (insert-patch-pair (car fd) path rpath)
		 (insert-patch (cdr fd) path rpath)))))

(defun insert-patch-pair (pair path rpath)
  (let* ((attr (car pair))
	 (value (second pair)))
    (cond ((leaf-p value) (list attr value))
	  ((and (path-p value) (path-relative-p value)) (list attr value))
	  ((path-p value) ;; an absolute path
	   (list attr (path-append rpath value)))
	  ((member attr *special-attributes*)
	   (copy-special-pair pair path))
	  (t (list attr
		   (insert-patch value (path-extend path attr) rpath))))))



(defun longest-common-prefix (l1 l2 &optional accu)
  "Return the lcp of 2 lists.
  Ex: (longest-common-prefix '(1 2 3 4) '(1 2 a)) --> (1 2)"
  (cond ((or (null l1) (null l2)) (nreverse accu))
	((equalp (car l1) (car l2))
	 (longest-common-prefix (cdr l1) (cdr l2) (cons (car l1) accu)))
	(t (nreverse accu))))

(defun strip-prefix (path prefix)
  "Return path without the prefix."
  (make-path :l (nthcdr (length (path-l prefix)) (path-l path))))

(defun make-relative-path (p1 p2)
  "Transform p2 into a path relative to p1.
  Return 3 vals: the relative path, how many ^ are necessary, and the
  length of the shared prefix between p1 and p2."
  (let* ((l1 (path-l p1))
	 (l2 (path-l p2))
	 (lcp (longest-common-prefix l1 l2))
	 (llcp (length lcp))
	 (uplevel (- (length l1) llcp)))
    (values 
     (make-path :l (append 
		    (make-sequence 'list uplevel :initial-element '^)
		    (subseq l2 llcp)))
     uplevel
     llcp)))


;; ======================================================================
;; GDC: New version of gdp that returns value AND physical path.
;; ======================================================================

(defun ban-path-as-total (fd)
  (cond ((and (path-p fd) (path-relative-p fd))
	 (error "A total fd cannot be a relative path"))
	((path-p fd) (setf fd (filter-flags (build-fd-from-path fd (make-path)))))
	(T fd)))

(defun top-gdc (fd path) (let ((*input* (ban-path-as-total fd))) (gdc fd path)))

(defun gdc (fd path)
  "given an fd (representing a connected graph) and a path, 
  return 5 values: 
  1. value pointed by path
  2. physical path where this value occurs in fd.
  3. what's missing from path in the physical rep.
  4. the last node of the path traversed in fd.
  5. the last arc of the path traversed in fd."
  ;; Check for cycles in the way. Mark our way in already-tried.
  ;; For fds in canonical form, all paths point to the physical
  ;; representant directly, and already-tried is not necessary.  For other
  ;; fds, you need it as shown by example: ((a {o}) (o ((m {a m}))))

  (do* ((fd fd) 
	(path path)
	(already-tried (list path))
	(cpath (make-path))
	(current-fset (find-fset fd))
	(arc-tried (list :top fd))
	(node-tried (list arc-tried)))
      ((and (not (path-p fd)) (path-null path)) 
       (values fd cpath path node-tried arc-tried))
    ;; (format t "~&DO path = ~s - cpath = ~s - fd = ~s" path cpath fd)
    (cond 
     ;; Below any/given/nil is unspecified.
     ((or (eq fd 'any) (eq fd 'given) (eq fd nil))
      (return (values nil cpath path fd arc-tried)))

     ;; Below any other atom is impossible: none
     ((leaf-p fd) (return (values 'none cpath path node-tried arc-tried)))

     ;; Need to go up to the root and go down again to the indirection.
     ((path-p fd)
      ;; A non-special cannot point to a special: check validity here.
      #+check-types
      (let ((from (car arc-tried))
	    (to   (car (last (path-l fd)))))
	(cond 
	 ((eq from to))  ;; fine - type equality
	 ((or (special-p from) (special-p to)) 
	  (return (values 'none cpath path node-tried arc-tried)))))
      ;; ok proceed...
      (let* ((path-prefix (absolute-path fd cpath))
	     (npath (path-append path-prefix path)))
	;; This is a loop: ({a b c} {a b c d e})
	;; Note: the problem is only when a short points to a long:
	;; ((a {a b})) loops, ((a ((b {a})))) is fine.
	;; (format t "~&already tried = ~s - pp = ~s" already-tried path-prefix)
	(cond 
	 ;; Find a short path pointing to an extension of itself:
	 ;; Replace (a {a b}) by (a ((b {a}))) and continue.
	 ;; Ignore cases (a {a}) which are tetard cases dealt with below.
	 ((and (path-prefix path-prefix cpath) 
	       (not (path-equal path-prefix cpath)))
	  ;; (format t "~&Found a short to long pointer: ~s ~s" path-prefix cpath)
	  (let ((extension (strip-prefix path-prefix cpath)))
;;	    (format T "~& short->long")
;;	    (format T "~& arc-tried1 = ~s" arc-tried)
;;	    (format T "~& already-tried1 = ~s" already-tried)	    
	    (setf (second arc-tried) 
		  (build-fd-from-path extension cpath)))
	  ;; Reinit already-tried for the new structure
	  (setf already-tried nil))
;;	  (format T "~& arc-tried2 = ~s" arc-tried)
;;	  (format T "~& already-tried2 = ~s" already-tried)

	 ;; Find a long path pointing to a prefix of itself.  This is a loop, eat
	 ;; the elements of the path and do not check for tetard cases here, because
	 ;; you want to cut the tetard head where it is redundant, not here.
	 ;; Example: ((a ((b ((c {a d d d b c}))) (d {a}))))
	 ;; You do NOT want to cut (d nil) but instead (c nil).
	 ((and (path-prefix cpath path-prefix)
	       (not (path-equal cpath path-prefix))))
;;	  (format T "~& long->short")
	 
	 ;; Tetard cases: cut the redundant path eg ((a {a})) is ((a nil))
	 ;; or  ((a {b}) (b ((m {a m})))) is ((a {b}) (b ((m nil))))
	 ((member npath already-tried :test #'path-equal)
;;	  (format T "~& tetard")
	  (setf (second arc-tried) nil)
	  (return (values nil cpath path node-tried arc-tried))))

;;	(format T "~& arc-tried3 = ~s" arc-tried)
;;	(format T "~& already-tried3 = ~s" already-tried)	    
	(setf path npath)
	(push path already-tried)
	(setf cpath (make-path))
	(setf fd *input*)
	(setf arc-tried (list :top fd))
	(setf node-tried (list arc-tried))))
;;	(format T "~& arc-tried4 = ~s" arc-tried)
;;	(format T "~& already-tried4 = ~s" already-tried)

     ;; Special attributes are also atomic: cannot go below them.
     ;; but can have a path as value.
     ((member (path-car path) *special-attributes*)
      (cond ((cdr (path-l path)) (return (values 'none cpath path node-tried arc-tried)))
	    ((or (null current-fset) 
		 (member (path-car path) current-fset)
		 (eq (path-car path) 'fset))
	     (setf arc-tried (safe-assoc (path-car path) fd))
	     (setf node-tried fd)
	     (if (consp arc-tried)
		 (setf fd (second arc-tried))
	       (return (values nil cpath path node-tried arc-tried)))
	     (setf cpath (path-extend cpath (path-car path)))
	     (setf path (path-cdr path)))
	    (t (return (values 'none cpath path node-tried arc-tried)))))

     ;; Are we within the authorized FSET? If so, go down one level.
     ((or (null current-fset)
	  (member (path-car path) current-fset))
      (setf arc-tried (safe-assoc (path-car path) fd))
      (setf node-tried fd)
      (if (consp arc-tried)
	  (setf fd (second arc-tried))
	  (return (values nil cpath path node-tried arc-tried)))
      (setf cpath (path-extend cpath (path-car path)))
      (setf path (path-cdr path))
      (setf current-fset (find-fset fd)))

     ;; Not within the authorized FSET: value is none.
     (t (return (values 'none cpath path node-tried arc-tried))))))

(defun phys-rep (path)
  (multiple-value-bind (val phys missing) (gdc *input* path)
    (declare (ignore val))
    (path-append phys missing)))

;; -----------------------------------------------------------------------
(provide "fd-to-graph2")
;; -----------------------------------------------------------------------

