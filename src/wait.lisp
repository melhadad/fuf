;;; -*- Mode:Lisp; Syntax:Common-Lisp; Package: FUG5 -*-
;;; -----------------------------------------------------------------------
;;; File:         wait.l
;;; Description:  Deal with WAIT annotations of ALT constructs (the freeze).
;;; Author:       Michael Elhadad
;;; Created:      28 Jul 1991
;;; Modified:     29 Oct 1991: add *trace-wait*
;;;               04 Dec 1991: changed check-wait to use path2+wait to
;;;               resolve paths in wait instead of path2.
;;;               09 Dec 1991: added :from-end t to find-candidate so that
;;;               oldest frozen alt is thawed first.
;;;               09 Dec 1991: added after-wait handling
;;;               23 Dec 1991: added switch to address of failure in
;;;                            force-agenda. 
;;;               20 Oct 1992: Added level to trace-format
;;;                5 Sep 1995: Changed get-active-agenda so that 
;;;                            If there is a thaw-candidate, retrieve it
;;;                            first, else retrieve the first item from the
;;;                            agenda. 
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

;; Wait annotation modifies the control on alt constructs.
;; Syntax is: (alt (:wait attr) <branches>)
;;        or: (alt (:wait path) <branches>)
;;        or: (alt (:wait (path1 ... pathn)) <branches>)
;;        or: (alt (:wait ( (path1 given) (pathi #(under x)) ...) <branches>))
;; (:wait attr) is equivalent to (:wait ({^ attr} given))
;; (:wait path) is equivalent to (:wait ((path given)))
;; (:wait (path)) is equivalent to (:wait ((path given)))
;; 
;; Effect is:
;; If ALL pathi in the list are sufficiently instantiated (non NIL value
;; more specific than the lower bound specified in the annotation), the alt is
;; evaluated as usual (note the CONJUNCTION).
;; If ANY pathi is uninstantiated (NIL or more general than the under
;; bound), the alt is frozen until all pathi's have a value instantiated
;; enough. 
;; 
;; Frozen alts are re-checked before each new alt is processed.
;; When an alt is thawed (awakened), it is evaluated in the same context
;; where it was frozen and control continues to the current alt as usual.
;;
;; NOTES:
;; 1. Relative paths in the annotation refer to path2+wait (where the grammar
;;    writer think his alt is evaluated + wait so that ^ works for children).
;; 2. If an alt waited for (path given) or (path under) and path has a
;;    value of NONE, we don't wait (thaw). 
;; 3. If an alt waited for (path under) or (path leaf) and path has a value
;;    non leaf (a list), we don't wait.
;; 4. After-wait is a piece of code that can be added when calling
;;    check-agenda and is executed after an alt is successfully unified
;;    even if the alt had to be frozen.  After-wait is executed in any
;;    case. 

;; *AGENDA-POLICY* determines what to do with frozen alts at the end of
;; unif: keep them undeveloped or force them (:keep or :force).
;; It is tested in determine.l


;; ----------------------------------------------------------------------
;; AGENDA DEFINITION
;; ----------------------------------------------------------------------

;; Agenda only contains agenda-item structs.
;; The agenda is organized to collect items that can be added or removed
;; often.  The mechanism works as follows:
;; Each item has a unique numeric id.
;; When the item is temporarily removed, the id becomes negative.
;; When the item is returned to the pool, the id becomes positive again.

(defstruct agenda-item 
  id                    ;; unique identifier for quick test
  alt                   ;; the alt pair (complete as in grammar)
  wait                  ;; the list of guard paths
  path1
  path2                 ;; the one to use to check awakening
  after-wait            ;; piece of code to evaluate after waking up
                        ;; in addition to calling normal continuation.
  index
  order
  ignore-unless
  ignore-when
  pair
  )


(defun gen-agenda-id ()
  (incf *agenda-id*))

(defun add-agenda (frame alt-pair wait path1 path2 after-wait
			 index order ignore-unless ignore-when pair)
  (let ((id (gen-agenda-id)))
    (push (make-agenda-item 
	   :id id
	   :alt alt-pair 
	   :wait wait
	   :after-wait after-wait
	   :path1 path1
	   :path2 path2
	   :index index
	   :order order
	   :ignore-unless ignore-unless
	   :ignore-when ignore-when
	   :pair pair)
	  *agenda*)
    ;; Log the action on the undo list
    (nconc (frame-undo frame) (list (cons 'aa id)))
    id))

(defun find-agenda (id)
  (find-if #'(lambda (ag-item) (= (agenda-item-id ag-item) id)) *agenda*))

(defun empty-agenda ()
  "Check whether agenda contains active items"
  (every #'(lambda (item)
	     (< (agenda-item-id item) 0))
	 *agenda*))

(defun remove-agenda (frame ag-item)
  ;; Log the action on the undo list (positive id)
  (nconc (frame-undo frame) (list (cons 'ar (agenda-item-id ag-item))))
  ;; Just switch the sign of the id
  (setf (agenda-item-id ag-item) (- (agenda-item-id ag-item))))


;; ----------------------------------------------------------------------
;; Syntax for wait annotations
;; ----------------------------------------------------------------------

;; Put a wait clause in canonical form:
;; list of pairs (path given) or (path #(under x)) or (path symbol)
;; Acceptable input forms are:
;; attr => ( ({^ attr} given) )
;; path => ( (path given) )
;; list of the above mixed with acceptable values.

(defun normalize-wait (wait)
  (cond ((listp wait) (mapcar #'canonical-wait-form wait))
	(t (list (canonical-wait-form wait)))))

(defun canonical-wait-form (wait)
  (cond 
   ((symbolp wait) `( ,(make-path :l (list '^ wait)) given))
   ((path-p wait) `( ,wait given))
   ((wait-pair-p wait) wait)
   (t (error ":Wait annotation is not well formed: ~s." wait))))

(defun wait-pair-p (expr)
  "Check whether expr is a pair of the form (path given) or (path under)"
  (and 
   (consp expr)
   (= (length expr) 2)
   (path-p (car expr))
   (or (leaf-p (second expr))
       (underp (second expr)))))


;; ----------------------------------------------------------------------
;; Check whether a wait clause can be thawed
;; Check-wait is true -> don't wait.
;; ----------------------------------------------------------------------

;; The rule is as follows to decide whether to thaw:
;;               test| given   under   symbol
;; value in *input* -|------------------------
;;           empty   | freeze  freeze  freeze
;;            leaf   | thaw    subsume subsume
;;        non-leaf   | thaw    thaw    thaw
;; 
;; NOTE: NONE plays no special role... (think about it).
(defun check-wait (wait path)
  (every
   #'(lambda (pair)
       (let* ((p (car pair))
	      (v (second pair))
	      (val (gdp *input* (absolute-path p (path-extend path :wait)))))
	 (cond 
	  ((empty-fd val) nil)
	  ((not (leaf-p val)) t)
	  ((eq v 'given) t)
	  ((subsume v val)))))
   wait))



;; ----------------------------------------------------------------------
;; Find a candidate to thaw and if found remove it from agenda
;; ----------------------------------------------------------------------

(defun get-thaw-candidate (frame)
  (let ((cand (find-if #'(lambda (ag-item)
			   (and (> (agenda-item-id ag-item) 0)
				(check-wait (agenda-item-wait ag-item)
					    (agenda-item-path2 ag-item))))
		       *agenda* :from-end t)))
    (when cand 
      (remove-agenda frame cand)
      cand)))


;; Get any active entry from the agenda.  Nil if empty agenda.
;; CHANGE: 5/9/95
;; If there is a thaw-candidate, retrieve it first, else retrieve the first
;; one from the agenda.
(defun get-active-agenda (frame)
  (let ((cand (get-thaw-candidate frame)))
    (if cand
	cand
      (let ((active (find-if #'(lambda (item)
				 (> (agenda-item-id item) 0)) 
			     *agenda* :from-end t)))
	(when active
	      (remove-agenda frame active)
	      active)))))



;; ----------------------------------------------------------------------
;; Check the agenda and thaw if possible, otw proceed to alt-unify
;; ----------------------------------------------------------------------

(defun check-agenda (fd1 fd2 path1 path2 frame fail success 
			 &key indexed-given order-given force-wait after-wait
			      (pair :unknown))
  (let ((ag-item (get-thaw-candidate frame)))
    (if ag-item
      ;; wake him up and if ok, keep checking the agenda
      (let ((npath1 (agenda-item-path1 ag-item))
	    (npath2 (agenda-item-path2 ag-item))
	    (after  (agenda-item-after-wait ag-item)))
	(trace-format 
	 *trace-wait* frame 15
	 "Thawing [agenda ~s]: Restarting at level ~s" 
	 (- (agenda-item-id ag-item)) npath1)
	(alt-unify (gdp *input* npath1)
	       (list (agenda-item-alt ag-item))
	       npath1 npath2 frame 
	       fail
	       #'(lambda (fd fail frame)
		   (declare (ignore fd))
		   (when after (funcall after))
		   (check-agenda fd1 fd2 path1 path2 frame fail success
				 :indexed-given indexed-given
				 :order-given order-given
				 :force-wait force-wait
				 :after-wait after-wait
				 :pair pair))
	       :force-wait t
	       :pair (agenda-item-pair ag-item)
	       :indexed-given (agenda-item-index ag-item)
	       :order-given (agenda-item-order ag-item)))
	     
      ;; Otherwise, proceed to current alt
      (alt-unify fd1 fd2 path1 path2 frame fail success
		 :indexed-given indexed-given
		 :order-given order-given
		 :force-wait force-wait
		 :after-wait after-wait
		 :pair pair))))
       


;; ----------------------------------------------------------------------
;; filter-agenda: remove entries from agenda whose ignore clauses now match
;; ----------------------------------------------------------------------

(defun filter-agenda (frame)
  (mapc #'(lambda (ag-item)
	    (let ((ignore-when (agenda-item-ignore-when ag-item))
		  (ignore-unless (agenda-item-ignore-unless ag-item))
		  (path1 (agenda-item-path1 ag-item))
		  (path2 (agenda-item-path2 ag-item))
		  (id (agenda-item-id ag-item)))
	      (cond
	       ((< id 0))
	       ((check-ignore-when ignore-when path1 path2)
		(trace-format 
		 *trace-wait* frame 15
		 "Ignoring [agenda ~s]" (agenda-item-id ag-item)) 
		(remove-agenda frame ag-item))
	       ((check-ignore-unless ignore-unless path1 path2)
		(trace-format 
		 *trace-wait* frame 15
		 "Ignoring [agenda ~s]" (agenda-item-id ag-item)) 
		(remove-agenda frame ag-item)))))
	*agenda*))




;; ----------------------------------------------------------------------
;; fd-adjoin:  add an alt into fd at level path.
;;             the old value becomes the first branch of the alt.
;; ----------------------------------------------------------------------

;; NOTE: since we do not know how to GDP through an fd with ALTs, we cannot
;; really go at the level we want.
;; Instead, we just add an equation ({path} alt)...
;; For all usages, this is equivalent.
;; Also undoably remove agenda-item from agenda.
(defun fd-adjoin (fd ag-item frame)
  (let ((alt (agenda-item-alt ag-item))
	(path (agenda-item-path2 ag-item)))
    (enrich fd (list path (list alt)) frame)
    (remove-agenda frame ag-item)))



;; ----------------------------------------------------------------------
;; force-agenda: 
;; ----------------------------------------------------------------------
;; After each force, check agenda before forcing again.

(defun force-agenda (fd fail frame success)
  (declare (special *input* *failure-address* *changes-made*))
  (let ((item (get-active-agenda frame)))
    (if (null item)
      ;; The agenda is empty
      (funcall success fd fail frame)
      ;; Force this entry, and continue forcing if succeed
      (let ((npath1 (agenda-item-path1 item))
	    (npath2 (agenda-item-path2 item))
	    (after  (agenda-item-after-wait item)))
	(trace-format 
	 *trace-wait* frame 15
	 "Forcing [agenda ~s]: Restarting at level ~s" 
	 (- (agenda-item-id item)) npath1)
	;; Change failure address so that bk-class is not confused.
	(trace-format 
	 *trace-bk-class* frame 5
	 "BKw: Switch from ~s to ~s" *failure-address* npath1)
	(setf *failure-address* npath1)
	(setf *changes-made* t)
	(alt-unify 
	 (gdp *input* npath1)
	 (list (agenda-item-alt item))
	 npath1 npath2 frame 
	 fail
	 #'(lambda (fd fail frame)
	     (when after (funcall after))
	     (let ((ag-item (get-thaw-candidate frame)))
	       (if ag-item
		 ;; wake him up and if ok, keep checking the agenda
		 (let ((npath1 (agenda-item-path1 ag-item))
		       (npath2 (agenda-item-path2 ag-item))
		       (after  (agenda-item-after-wait ag-item)))
		   (trace-format 
		    *trace-wait* frame 15
		    "Thawing [agenda ~s]: Restarting at level ~s" 
		    (- (agenda-item-id ag-item)) npath1)
		   (alt-unify (gdp *input* npath1)
			      (list (agenda-item-alt ag-item))
			      npath1 npath2 frame 
			      fail
			      #'(lambda (fd fail frame)
				  (when after (funcall after))
				  (force-agenda fd fail frame success))
			      :force-wait t
			      :pair (agenda-item-pair ag-item)
			      :indexed-given (agenda-item-index ag-item)
			      :order-given (agenda-item-order ag-item)))
		 (force-agenda fd fail frame success))))
	 :force-wait t
	 :indexed-given (agenda-item-index item)
	 :order-given (agenda-item-order item)
	 :pair (agenda-item-pair item))))))
	     

#|
;; Dec 15 91: THE FOLLOWING IS NOT USED ANYMORE - IT IS REPLACED BY
;; DET-CONSTITUENTS. 
;; ----------------------------------------------------------------------
;; Constituent-Agenda definition
;; ----------------------------------------------------------------------
;; To deal with the interaction cset/wait (simplistic...)
;; Only deals with delayed constituents in breadth-first
;; Not with modifications to cset that may derive from a delayed alt after
;; cset has been computed by breadth-first like in
;; (alt (:wait {never}) (((x ((cat clause))))))
;; the x constituent will not be added to the breadth-first search...
;; Well now you know it at least...

;; Exactly same mechanism as for *agenda* without the ignore complications.

(defstruct constituent-agenda-item 
  id 
  path
  grammar
  cat-attribute
  cset-attribute)

(defun gen-constituent-agenda-id ()
  (incf *agenda-id*))

(defun add-constituent-agenda (path frame grammar cat cset)
  (let ((id (gen-constituent-agenda-id)))
    (push (make-constituent-agenda-item 
	   :id id :path path :grammar grammar 
	   :cat-attribute cat :cset-attribute cset)
	  *constituent-agenda*)
    ;; Log the action on the undo list
    (nconc (frame-undo frame) (list (cons 'ac id)))
    id))


(defun find-constituent-agenda (id)
  (find-if #'(lambda (item) (= (constituent-agenda-item-id item) id)) 
	   *constituent-agenda*))

(defun empty-constituent-agenda ()
  "Check whether constituent agenda contains active items"
  (every #'(lambda (item)
	     (< (constituent-agenda-item-id item) 0))
	 *constituent-agenda*))

(defun remove-constituent-agenda (frame item)
  ;; Log the action on the undo list (positive id)
  (nconc (frame-undo frame) (list (cons 'acr (constituent-agenda-item-id item))))
  ;; Just switch the sign of the id
  (setf (constituent-agenda-item-id item) 
	(- (constituent-agenda-item-id item))))


;; Get any active entry from the constituent agenda.  Nil if empty agenda.
(defun get-active-constituent-agenda (frame)
  (let ((active (find-if #'(lambda (item)
			     (> (constituent-agenda-item-id item) 0)) 
			 *constituent-agenda* :from-end t)))
    (when active
      (remove-constituent-agenda frame active)
      active)))


(defun force-constituent-agenda (fd fail frame success)
  (declare (special *input*))
  (let ((item (get-active-constituent-agenda frame)))
    (if (null item)
      ;; The agenda is empty
      (funcall success fd fail frame)
      ;; Force this entry and continue forcing if succeed
      (let ((path (constituent-agenda-item-path item))
	    (grammar (constituent-agenda-item-grammar item))
	    (cat (constituent-agenda-item-cat-attribute item))
	    (cset (constituent-agenda-item-cset-attribute item)))
	(trace-format 
	 *trace-wait* frame 15
	 "Forcing constituent-agenda at level ~s" path)
	(unify-breadth-first 
	 (list path) *input* grammar frame fail 
	 #'(lambda (fd fail frame)
	     (force-constituent-agenda fd fail frame success))
	 cat cset :force)))))
			   
|#

;; ----------------------------------------------------------------------
(provide "$fug5/wait")
;; ----------------------------------------------------------------------
