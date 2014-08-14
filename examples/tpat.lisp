;;; -*- Mode:Lisp; Syntax:Common-Lisp;  -*-
;;; -----------------------------------------------------------------------
;;; File:         tpat.lisp
;;; Description:  The tpattern routines (for tense determination)
;;; Author:       David Robinowitz
;;; Created:       1 Jun 1990
;;; Modified:     24 July 1990
;;;               07 Apr 1991 (Michael Elhadad: added comments, fixed few
;;;               bugs.  Not tested properly yet)
;;; -----------------------------------------------------------------------
;;; WARNING: THIS TPATTERN UNIFIER DOES NOT WORK PROPERLY
;;; IT WORKS ENOUGH TO RUN EXAMPLES BUT NEEDS TO BE FIXED.

;;; This files implements a "tpattern unifier" for use with FUF.
;;; tpattern stands for "temporal pattern."  tpatterns are used in grammars to
;;; encode the semantics of tense.  
;;; tpattern express temporal relations between the speech time, the event
;;; time (the time when the event we are describing happens) and several
;;; reference times (up to five in this implementation).  
;;; Each one of these actually refers to an interval of time.
;;; Relations between these temporal intervals can be: :precedes,
;;; :includes, :equal, :overlaps, :meets, :before, :during, :in or some
;;; more generic relations :any, :given, or :none.
;;; For example, to describe an event in the past, one might do:
;;; (tpattern (:et :before :st))
;;; meaning that the event time occured before the speech time.
;;; Reference times are necessary to account for complex temporal relations
;;; like the one expressed by the following:
;;; I am going to be taking the bus.
;;; The grammar in GR4T takes  a tpattern in input and maps it to one of
;;; the 36 tenses of the English grammar identified by Halliday that is
;;; compatible with the tpattern specification.  The grammar tries to
;;; select the simplest tense compatible with the input specification.
;;; (Note that GR4T also functions without tpattern if the tense is
;;; specified by name).
;;;
;;; The tpattern unifier takes two tpattern descriptions and "unifies" them
;;; together to return a tpattern that is more specific than both of its
;;; inputs arguments.
;;;
;;; The tpattern unifier is registered with FUF as the procedural
;;; unification method for the tpattern attribute (cf FUF manual for
;;; procedural unification methods).  This way, when FUF encounters a
;;; tpattern attribute, it uses the tpattern unifier instead of the generic
;;; FUF unification method.
;;;
;;; The tpattern unifier also includes a syntax checker that is used by
;;; FD-P and GRAMMAR-P to check if tpatterns are used appropriately.
;;;

(in-package "FUG5")

(export 'tpattern)
 
(defvar *tpattern-max-refs* 5 "the maximum number of references allowed in a
tpattern element.  This should be equal to (length *tpattern-ref-keywords*)")
(defvar *tpattern-ref-keywords* '((:rt0) (:rt1) (:rt2) (:rt3) (:r4)))
(defvar *tpattern-debug* nil)

(defun tpattern-reset-tpattern-ref-keywords ()
  (setq *tpattern-ref-keywords* '((:rt0) (:rt1) (:rt2) (:rt3) (:r4))))
  
(defvar *tpattern-speech-time-keyword* :st)
(defvar *tpattern-event-time-keyword* :et)
(defvar *tpattern-reversable-operands* '(:includes :equals))

(defun reset-tpattern-types ()
  (define-feature-type :any      (:precedes :includes))
  (define-feature-type :given    (:precedes :includes))
  (define-feature-type :precedes (:overlaps :meets :before))
  (define-feature-type :includes (:during :in :equals)))

(defun tpattern-check-operand (potential-op)
  "This function checks a tpattern operand for legality.  A legal t-pattern
operand is either the speech time  symbol, the event-time symbol, or one of
the reference time symbols."  
  (tpattern-reset-tpattern-ref-keywords)  
  (or
   (eq potential-op *tpattern-speech-time-keyword*)
   (eq potential-op *tpattern-event-time-keyword*)
   (assoc potential-op *tpattern-ref-keywords*))
  )


(defun tpattern-check-operator (potential-operator)
  "This function checks a tpattern operator for legality.  A legal tpattern
operator is a member of *tpattern-operators*."
  (tpattern-reset-tpattern-ref-keywords)
  (or
   (eq   :none  potential-operator)
   (eq   :given potential-operator)
   (subsume :any   potential-operator))
  )


(defun tpattern-reset-graph ()
  (tpattern-reset-tpattern-ref-keywords)
  `((,(copy-tree *tpattern-event-time-keyword*))
    (,(copy-tree *tpattern-speech-time-keyword*))
    ,@(copy-tree *tpattern-ref-keywords*))
  )

(defun tpattern-get-relation (graph key-time-element related-time-element)
  "Given a graph of tpattern relations, it locates the operand which
relates key-time-element to related-time-element.  Remeber, the graphs
are ordered, so it may be necessary to check for a relationship 
between two different time elements both in row order and in col.
order -- so to speak.  Just swap the order of the time-element
params."
  (let ((value (assoc related-time-element 
		      (cdr (assoc key-time-element graph)))))
    (when value
      (second value)))
  )

(defun tpattern-set-relation  (graph key-time-element related-time-element 
				     operand)
  (let* ((relations (assoc key-time-element graph))
	 (value (assoc related-time-element
		       (cdr relations))))
    (unless relations
      (error "something's corrupt in tpattern-set-relation"))
    (if value
	(rplacd value (list operand)) ; replace the existing operand
      ;; there's no value for this time-element, create a list
      (rplacd relations
	    (append (list (list related-time-element operand))
		    (cdr relations)))))
  )

(defun tpattern-reversable-operand-p (operand)
  (member operand *tpattern-reversable-operands*)
  )

(defun tpattern-unify-operands (op1 op2)
  (cond ((equal :given op1)
	 (if (and op2
		  (not (equal op2 :any))) op2 :fail))
	((equal :given op2)
	 (if (and op2
		  (not (equal op1 :any))) op1 :fail))
	((null op1) op2)         ; op1 is NIL
	((null op2) op1)         ; op2 is NIL
	((equal op1 op2) op1)    ; they're the same
	((subsume op1 op2) op2)  ; op2 is more specific than op1
	((subsume op2 op1) op1)  ; op1 "     "   "        "  op2
	(t :fail))               ; unifcation fails
  )
	
(defun tpattern-unify-relation (graph key-time-element related-time-element
				      operand)
  (let* ((old-operand (tpattern-get-relation 
		       graph
		       key-time-element
		       related-time-element))
	 (unified-operand (tpattern-unify-operands operand old-operand)))
    ;;  (format t "~%; graph: ~a~%; key-time-element: ~A~%;~
    ;;             related: ~A~%; operand: ~A"
    ;;         graph key-time-element related-time-element operand)
    ;;  (format t "~%; old-operand ~A old-inverse-operand ~A~
    ;;             unified-operand: ~A"
    ;;         old-operand old-inverse-operand unified-operand)
    (unless (equal :fail unified-operand)
      (tpattern-set-relation  ;set the relation
       graph
       key-time-element
       related-time-element
       unified-operand)
      (when (and (tpattern-reversable-operand-p old-operand)
		 (not (tpattern-reversable-operand-p unified-operand)))
	;; if the old operand was reversable and the new one isn't
	;; [e.g. old: :includes; new:  :meets], then we want to zap
	;; the old operand in its inverse incarnation (so to speek).
	(tpattern-set-relation 
	 graph
	 related-time-element
	 key-time-element
	 NIL))
      t))
  )


(defun tpattern-graph (tp)
  "This function takes a list of tpattern relations and encodes
them into a graph structure, performing certain tests.
Basically, they are as follows, the number of reference
times cannot be greater than *tpattern-max-refs*.  Then, each operand
must be either :st -- speech-time, :et -- event-time, or :rti where
i is 0 .. *tpattern-max-refs* -1.  Future versions may want to return
better error messages.   Right now all failed parses return the
same (NIL)."
  (do ((cur-statement (car tp) (car tp))
       (graph (tpattern-reset-graph))
       (ok t))
      ((or (null tp)
	   (not  ok)) (if ok graph :fail))
      (setf ok (and (listp cur-statement)
		    (= (length cur-statement) 3)
		    (tpattern-check-operand (first cur-statement))
		    (tpattern-check-operand (third cur-statement))
		    (tpattern-check-operator (second cur-statement))
		    (tpattern-unify-relation graph
					     (first cur-statement)
					     (third cur-statement)
					     (second cur-statement))
                    ;; now, if the operand is reversable -- install
                    ;; the inverse relation too
		    (or (not 
			 (tpattern-reversable-operand-p 
			  (second cur-statement)))
			(tpattern-unify-relation graph
						 (third cur-statement)
						 (first cur-statement)
						 (second cur-statement)))))
    (setf tp (cdr tp))))


(defun tpattern-generate-new-mapping (mapping)
  )


;; Not used yet - should be used to clean up graphs into canonical form
(defun tpattern-find-unused-refs (tp-graph)
  (do ((not-found-refs (copy-tree *tpattern-ref-keywords*))
       (cur-item (car tp-graph) (car tp-graph)))
      ((null cur-item) not-found-refs)
    (when (cdr cur-item)
      (setf not-found-refs 
            (remove-if #'(lambda (x)
                           (equal (car x) (car cur-item))) not-found-refs))
      (mapcar #'(lambda (relation)
                  (setf not-found-refs
                        (remove-if #'(lambda (item)
                                       (equal item (list (car relation))))
                                not-found-refs)))
              (cdr cur-item)))
    (setf tp-graph (cdr tp-graph))))


;; This is not done - nor used yet
(defun tpattern-fix-rts (tp-graph)
  (if (equal tp-graph :fail)
    :fail
    (let ((unused-refs (reverse (tp-find-gaps tp-graph))))
      
      ))
  )


;; Merge (unify) two graphs containing temporal relations as arcs.  
;; Return :fail if graphs are not compatible.
(defun tpattern-merge (tp-graph1 tp-graph2 mapping)
  (if (or (eq tp-graph1 :fail)
          (eq tp-graph2 :fail))
    :fail
    (do ((cur-item (car tp-graph2) (car tp-graph2))
         (ok t))
        ((or (null tp-graph2)
             (not ok)) (if ok tp-graph1
                           :fail))
      ;; we don't need to worry about reversable operators
      ;; cause they're taking care of by tpattern-graph.
      (setf ok (do* ((first-operand (car cur-item))
                     (relations (cdr cur-item) (cdr relations))
                     (cur-relation (car relations) (car relations)))
                    ((or (null cur-relation)
                         (not ok)) ok)
                 ;; if there is an operator:
                 (when (second cur-relation)
                   (setf ok (tpattern-unify-relation tp-graph1
                                                     first-operand
                                                     (first cur-relation)
                                                     (second cur-relation))))))
      (setf tp-graph2 (cdr tp-graph2)))))


;; Linearize a graph back into a canonical tpattern notation
(defun tpattern-linearize (tp-graph)
  (cond ((equal tp-graph :fail) :fail)
        ((NULL tp-graph) NIL)
        (t (do ((cur-item (car tp-graph) (car tp-graph))
                (tp nil))
               ((null tp-graph) tp)
             (do* ((first-operand (car cur-item))
                   (relations (cdr cur-item) (cdr relations))
                   (cur-relation (car relations) (car relations)))
                  ((null cur-relation) nil)
               (when (second cur-relation)
                 (setf tp 
                       (append tp 
                               `((,first-operand 
                                  ,(second cur-relation)
                                  ,(car cur-relation)))))))
             (setf tp-graph (cdr tp-graph))))))


;; ------------------------------------------------------------
;; SYNTAX CHECKING
;; ------------------------------------------------------------
(defun tpattern-check-each-member (tp)
  (do ((cur-item (car tp) (car tp))
       (ok t)) ; innocent until proven guilty
      ((or (null tp)
           (not ok)) ok)
      (setf ok (and (listp cur-item)
		    (= (length cur-item) 3)
		    (tpattern-check-operand (first cur-item))
		    (tpattern-check-operator (second cur-item))
		    (tpattern-check-operand (third cur-item))))
    (when *tpattern-debug*
      (format t "~%; cur-item: ~A, ok: ~A" cur-item ok))
    (setf tp (cdr tp))))


;; The syntax checker for tpatterns.  Is used by fd-p when registered with
;; FUF.
(defun tpattern-syntax (tp)
  (reset-tpattern-types)
  (when *tpattern-debug*
    (format t"~%; checking syntax of ~A" tp))
  (if (and (listp tp)
           (or (null tp)
               (tpattern-check-each-member tp)))
    t
    (values 
     nil 
     "tpattern ({(<tpattern-operand> <tpattern-operator> <tpattern-operand>)}*)")))


;; Top level function: unify two tpattern values.
(defun tpattern-unify (tp1 tp2 &optional path)
  (reset-tpattern-types)
  (tpattern-linearize (tpattern-merge (tpattern-graph tp1)
				      (tpattern-graph tp2) NIL)))



;; -----------------------------------------------------------------------
;; Register tpattern-unify as the procedural unification method for the
;; attribute tpattern with FUF.
;; -----------------------------------------------------------------------
(define-procedural-type 'tpattern #'tpattern-unify :syntax #'tpattern-syntax)


(provide "tpat")
