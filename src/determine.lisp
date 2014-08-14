;;; -*- Mode:Lisp; Syntax:Common-Lisp; Package:FUG5 -*-
;;; -----------------------------------------------------------------------
;;; File:         DETERMINE.L 
;;; Description:  "Determination" stage of FUGs with SUCCESS.
;;; Author:       Michael Elhadad
;;; Created:      16-Oct-88
;;; Modified:     27-Oct-88
;;;               29 Apr 90 - added *special-attributes*
;;;               16 May 90 - path-p and leaf-p.
;;;               11 Jun 90 - added success arg to determine.
;;;               20 Jun 90 - cleaned up det1 and filters.
;;;               25 Feb 91 - do not depend on throw anymore!
;;;                           Changed determine, det-tests and det1.
;;;               28 Jul 91 - added FREEZE and IGNORE and AGENDA-POLICY
;;;               19 Sep 91 - added *use-any*
;;;               13 Nov 91 - updated calls to *fail*
;;;               26 Nov 91 - added filter-flags and relocate
;;;               11 Dec 91 - added force-constituent-agenda call
;;;               13 Dec 91 - added det-constituents to deal with
;;;               constituents appearing as a result of a delayed alt. 
;;;               18 Dec 91 - added *use-wait*
;;;               23 Dec 91 - added change of failure address in det-const.
;;;               24 Dec 91 - remove call to add-constituent-agenda
;;;               06 Feb 92 - added :no-cset arg to determine to avoid
;;;                           going thru the det-const for non-structural fct.
;;;               11 Feb 92 - added *added-cset* in determine to ensure
;;;                           complete traversal of cset structure.
;;;               18 Feb 92 - fixed relocate - added insert-fd.
;;;               24 Feb 92 - added remove-duplicates in det-constituents.
;;;               02 Aug 92 - fixed insert-empty-fd to accept leafs/paths JR
;;;               20 Oct 92: Added level to trace-format
;;;               26 Oct 93: Update call to call-linearizer in det-any
;;;                          (add parameter cat-att).
;;;               04 Jan 94: Some anti-looping code in relocate.
;;;               09 Jan 94: Major bug fix in relocate: added tpath arg.
;;;               11 Apr 95: Replaced {^} with (make-path :l '(^)) (ME)
;;;               03 Sep 95: Removed relocate and insert-fd (defined in
;;;               fd-graph). 
;;; Package:      FUG5
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
(format t "Determiner...~%")

;; ------------------------------------------------------------
;; DETERMINE
;; ------------------------------------------------------------


;; Here is a version without the det-constituent fix
#+ignore(defun determine (fd fail frame success grammar cat-att cset-att)
  "Check AGENDAS, TESTs and ANYs at the end of unification"
  ;; Agenda-policy determines what to do with frozen alts at the end of
  ;; unif: keep them undeveloped or force them (:keep or :force).
  (det-agenda 
   fd fail frame
   #'(lambda (fd fail frame)
       (if (empty-agenda)
	 (det-tests 
	  fd (reverse (frame-tests frame)) fail frame
	  #'(lambda (fd fail frame)
	      (if *use-any*
		(det-any fd fd (make-path) frame fail success)
		(funcall success fd fail frame))))
	 ;; The delayed stuff has added more delays... deal with them.
	 (determine fd fail frame success
		    grammar cat-att cset-att)))))

;; Finally, it looks like det-constituents makes constituent-agenda useless...
#+ignore(defun determine (fd fail frame success grammar cat-att cset-att)
  "Check AGENDAS, TESTs and ANYs at the end of unification"
  ;; Agenda-policy determines what to do with frozen alts at the end of
  ;; unif: keep them undeveloped or force them (:keep or :force).
  (det-agenda 
   fd fail frame
   #'(lambda (fd fail frame)
       (force-constituent-agenda 
        fd fail frame
	#'(lambda (fd fail frame)
	    (det-constituents 
	     (list (make-path)) grammar cat-att cset-att
	     fd fail frame 
	     #'(lambda (fd fail frame)
		 (if (and (empty-agenda) (empty-constituent-agenda))
		   (det-tests 
		    fd (reverse (frame-tests frame)) fail frame
		    #'(lambda (fd fail frame)
			(if *use-any*
			  (det-any fd fd (make-path) frame fail success cat-att)
			  (funcall success fd fail frame))))
		   ;; The delayed stuff has added more delays... deal with them.
		   (determine fd fail frame success
			      grammar cat-att cset-att)))))))))


;; This is the good version: with just what we need!
(defun determine (fd fail frame success grammar cat-att cset-att
		     &optional use-cset)
  "Check AGENDAS, TESTs and ANYs at the end of unification"
  ;; Agenda-policy determines what to do with frozen alts at the end of
  ;; unif: keep them undeveloped or force them (:keep or :force).
  (setf *added-cset* nil)
  (if (and *use-wait* (not use-cset))
    (det-agenda 
     fd fail frame
     #'(lambda (fd fail frame)
	 (det-constituents 
	  (list (make-path)) grammar cat-att cset-att
	  fd fail frame 
	  #'(lambda (fd fail frame)
	      (filter-agenda frame)
	      (if (and (empty-agenda) (not *added-cset*))
		(det-tests 
		 fd (reverse (frame-tests frame)) fail frame
		 #'(lambda (fd fail frame)
		     (if *use-any*
		       (det-any fd fd (make-path) frame fail success cat-att)
		       (funcall success fd fail frame))))
		;; The delayed stuff has added more delays... deal with them.
		;; Or there has been an added cset at some level.
		(determine fd fail frame success
			   grammar cat-att cset-att))))))

    ;; Don't bother about wait: just checks tests and any
    (det-tests 
     fd (reverse (frame-tests frame)) fail frame
     #'(lambda (fd fail frame)
	 (if *use-any*
	   (det-any fd fd (make-path) frame fail success cat-att)
	   (funcall success fd fail frame))))))


;; ------------------------------------------------------------
;; DET-ANY: is there an any somewhere in fd
;; ------------------------------------------------------------
(defun det-any (fd total-fd path frame fail success cat-att)
  "Check for any ANYs left."
  (declare (special *from-top*))
  (cond ((null fd) (funcall success total-fd fail frame))
	((eq fd 'any) 
	 (trace-format *trace-determine* frame 30
		       "Fail in Determine: found an ANY at level ~s" path)
	 (when (and *global-tracing* *local-tracing* 
		    *trace-determine* *from-top*)
	   (trace-indent ">" frame)
	   (format t "CURRENT SENTENCE:~%")
	   (trace-indent ">" frame)
	   (print-sentence (call-linearizer *input* :cat-attribute cat-att)))
	 (*fail* fail frame path path :e))
	((leaf-p fd) (funcall success total-fd fail frame))
	((path-p fd) (funcall success total-fd fail frame))
	((leaf-p (car fd)) (error "Ill-formed fd in det-any"))
	((or (member (caar fd) *special-attributes*)
	     (path-p (cadar fd)))
	 (det-any (cdr fd) total-fd path frame fail success cat-att))
	(t 
	  (let ((new-path (path-extend path (caar fd)))
		(sub-fd (cadar fd)))
	    (det-any 
	     sub-fd total-fd new-path frame fail
	     #'(lambda (another-fd fail frame)
		 (declare (ignore another-fd))
		 (det-any (cdr fd) total-fd path frame fail success cat-att))
	     cat-att)))))


;; ------------------------------------------------------------
;; DET-TESTS: evaluation of all delayed tests.
;; ------------------------------------------------------------
(defun det-tests (fd tests fail frame success)
  (if (null tests)
    (funcall success fd fail frame)
    (let ((test (test-test (car tests)))
	  (path (test-path (car tests))))
      (if (eval `(let ((path ',path) (%frame% ,frame)) ,test))
	(progn
	  (trace-format *trace-determine* frame 5
			"TEST succeeds: ~s at level ~s"
			(filter-macro-char test) path)
	  (det-tests fd (cdr tests) fail frame success))
	(*fail* fail frame path path :e
		"Fail in testing ~s at level ~s"
		(filter-macro-char test) path)))))
    

;; ------------------------------------------------------------
;; DET-AGENDA: forces evaluation of all delayed stuff.
;; ------------------------------------------------------------

;; Agenda-policy determines what to do with frozen alts at the end of
;; unif: keep them undeveloped or force them (:keep or :force).

(defun det-agenda (fd fail frame success)
  (filter-agenda frame)   ;; remove the entries whose ignore clauses now match
  (cond 
   ((empty-agenda) (funcall success fd fail frame))
   ((eq *agenda-policy* :keep)
    ;; put all the alts into the result fd at the level they come from
    ;; (path2). 
    (mapc #'(lambda (ag-item)
	      ;; if path points to a leaf, force it to become part of the
	      ;; alt. fd-adjoin is defined in wait.l
	      ;; Note that fd-adjoin removes undoably the ag-item
	      (fd-adjoin fd ag-item frame))
	  *agenda*)
    (funcall success fd fail frame))
   ((eq *agenda-policy* :force)
    ;; Need to force the alt-unify of all entries successively.
    ;; force-agenda is defined in wait.l
    (force-agenda fd fail frame success))))


;; ------------------------------------------------------------
;; DET-CONSTITUENTS : check that all constituents have been visited
;;                    after all delayed stuff has been evaluated.
;;                    So if a delayed part creates a constituent,
;;                    it will be visited now.
;; Works as unify-breadth-first except that children of an already visited
;; constituent are still visited.  This forces a re-evaluation of all the
;; csets at all levels of *input*.
;; ------------------------------------------------------------

(defun det-constituents (lpath grammar cat-att cset-att fd fail frame success)
  (declare (special *failure-address* *changes-made*))
  (cond 
   ((null lpath) (funcall success *input* fail frame))
   (t (let* ((arc (gdpp *input* (car lpath) frame))
	     (cset (find-cset (safe-second arc) (car lpath) 
			      cat-att cset-att))) 
	(cond 
	 ((or (arc-is-marked arc) (arc-is-marked-after-wait arc)
	      (path-null (car lpath)))
	  (det-constituents 
	   (remove-duplicates (append (cdr lpath) cset)
			      :test #'path-equal :from-end t)
	   grammar cat-att cset-att
	   fd fail frame success))
	 (t
	  ;; Change failure address so that bk-class is not confused.
	  (trace-format 
	   *trace-bk-class* frame 5
	   "BKd: Switch from ~s to ~s" *failure-address* (car lpath))
	  (setf *failure-address* (car lpath))
	  (setf *changes-made* t)
	  (unify-cat 
	   (safe-second arc) grammar (car lpath) frame fail
	   #'(lambda (fd fail frame)
	       (cond 
		((eq fd :frozen)  ;; delay traversal of this constituent
		 #+ignore(add-constituent-agenda 
		  (car lpath) frame grammar cat-att cset-att)
		 (det-constituents
		  (cdr lpath) grammar cat-att cset-att
		  fd fail frame success ))
		(t
		 (let ((cset (find-cset fd (car lpath) cat-att cset-att)))
		   (when *trace-cset*
		     (if cset
		       (trace-format 
			*trace-cset* frame 20
			"Expanding constituent ~s into cset ~s." 
			(car lpath) cset)
		       (trace-format
			*trace-cset* frame 20
			"Constituent ~s is a leaf." (car lpath))))
		   (det-constituents
		    (append (cdr lpath) cset) grammar cat-att cset-att
		    fd fail frame success)))))
	   arc
	   cat-att
	   cset-att
	   )))))))


;; ------------------------------------------------------------
;; Some utilities to clean-up fds
;; ------------------------------------------------------------

(defun filter-nones (fd)
  "Remove all pairs (att none) from an fd at all levels."
  (cond ((null fd) fd)
	((leaf-p fd) fd)
	((path-p fd) fd)
	((leaf-p (car fd)) (error "Ill-formed fd in filter-nones"))
	((eq 'none (cadar fd)) (filter-nones (cdr fd)))
	((or (leaf-p (cadar fd)) 
	     (member (caar fd) *special-attributes*)
	     (path-p (cadar fd)))
	 (cons (car fd) (filter-nones (cdr fd))))
	(t 
	 (cons (list (caar fd)
		     (filter-nones (cadar fd)))
	       (filter-nones (cdr fd))))))

(defun filter-nils (fd)
  "Remove all pairs (att nil) from an fd at all levels."
  (cond ((null fd) fd)
	((leaf-p fd) fd)
	((path-p fd) fd)
	((leaf-p (car fd)) (error "Ill-formed fd in filter-nils"))
	((eq nil (cadar fd)) (filter-nils (cdr fd)))
	((or (leaf-p (cadar fd)) 
	     (member (caar fd) *special-attributes*)
	     (path-p (cadar fd)))
	 (cons (car fd) (filter-nils (cdr fd))))
	(t 
	 (let ((sub-fd (filter-nils (cadar fd)))
	       (rest-fd (filter-nils (cdr fd))))
	   (if sub-fd
	       (cons (list (caar fd) sub-fd) rest-fd)
	     rest-fd)))))

(defun filter-flags (fd &optional keep-nil)
  "Remove all :i and :e from an fd at all levels."
  (cond ((null fd) fd)
	((leaf-p fd) fd)
	((path-p fd) fd)
	((leaf-p (car fd)) (error "Ill-formed fd in filter-nils"))
	((and (null keep-nil) (eq nil (cadar fd))) (filter-flags (cdr fd)))
	((or (leaf-p (cadar fd)) 
	     (member (caar fd) *special-attributes*)
	     (path-p (cadar fd)))
	 (cons (list (caar fd) (cadar fd)) (filter-flags (cdr fd))))
	(t 
	 (let ((sub-fd (filter-flags (cadar fd)))
	       (rest-fd (filter-flags (cdr fd))))
	   (if sub-fd
	       (cons (list (caar fd) sub-fd) rest-fd)
	     rest-fd)))))

;; ------------------------------------------------------------
;; ANY-P : is there something at this location
;; ------------------------------------------------------------

(defun any-p (path)
  (if *use-any*
    (not (member (gdp *input* path) '(none nil)))
    t))


;; -----------------------------------------------------------------------
(provide "$fug5/determine")
;; -----------------------------------------------------------------------

