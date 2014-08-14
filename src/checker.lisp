;;; -*- Mode:Lisp; Syntax:Common-Lisp; Package:FUG5 -*-
;;; -----------------------------------------------------------------------
;;; File:         CHECKER.L
;;; Description:  Syntax and semantics verifier of FD forms
;;; Author:       Jong Lim, Jacques Robin and Michael Elhadad
;;; Created:      17-Feb-88
;;; Modified:     26-Feb-89 (Michael Elhadad)
;;;               01 May 90 - Moved exports to fug5
;;;               20 Jun 90 - Rewritten. 
;;;                         - fd-sem is now (u nil fd)
;;;                         - added (normalize fd) = (u nil fd)
;;;               11 Aug 91 - support for :wait :order and new annotations
;;;               19 Sep 91 - added *use-any*
;;;               25 Nov 91 - made cset a parameter
;;;               26 Nov 91 - commented out prep-input call in fd-p (JR)
;;;               27 Nov 91 - added check-cset
;;;               05 Nov 91 - moved check-cset to type.l
;;;               18 Dec 91 - added name of alt in path (alt pos br name).
;;;               20 Dec 91 - added checker for def-alt and def-conj.
;;;               23 Dec 91 - use get-grammar in grammar-p.
;;;               10 Nov 92 - avoid %break% during check.
;;;               04 Oct 93 - added prep-input and filter-flags to normalize-fd
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

(format t "Syntax and semantic checker...~%")

;; ----------------------------------------------------------------------
;;    FD-P-TEST:  returns T if the predicate "test" is true, otherwise,
;;                prints the given error message with "path" and
;;                returns NIL.
;; ----------------------------------------------------------------------
(defmacro fd-p-test (test path err-msg &rest args)
  `(cond (,test) 
	 (t (fd-p-err-msg ,path ,err-msg ,@args) nil)))

(defmacro fd-p-err-msg (path err-msg &rest args)
  `(progn
     (when print-messages
       (format t ,err-msg ,@args)
       (format t "~%PATH ==> ~s~%" (path-reverse ,path)))
     (throw 'fd-p (values nil ,path))))

(defun how-many (&rest bools)
  "Counts how many of its arguments are T"
  (count-if #'identity bools))

;; ----------------------------------------------------------------------
;;    FD-SYNTAX:
;;          returns T if the argument passed is in FD form or
;;          returns NIL with error messages, otherwise.
;; 
;;          A functional description (FD) is in the following format:
;; 
;; 	   FD   <=  (PAR1 {F} PAR2 {F} ... {F} PARn)
;;         PAR  <=  ("alt" {F} {I} (FD1 FD2 ... FDn))
;;         PAR  <=  ("ralt" {F} {I} (FD1 FD2 ... FDn))
;; 	   PAR  <=  ("opt" {F} FD)
;;         PAR  <=  (SPC list)
;; 	   PAR  <=  (sym RHT)
;; 	   PAR  <=  (sym PATH)
;; 	   PAR  <=  (PATH RHT)
;; 	   PAR  <=  (PATH PATH)
;;         PAR  <=  (SPC depends-on-SPC)
;;         SPC  <=  "pattern "| "cset" | "test" | "control" | "fset"
;;         SPC  <=  a member of *special-attributes*
;; 	   RHT  <=  sym
;; 	   RHT  <=  FD
;;         PATH <=  an-existing-path-in-fd
;; 	   F    <=  an-atomic-tracing-flag | (trace {...} F)
;;         I    <=  (index {...} PATH) | (index {...} an-existing-atom)
;; ----------------------------------------------------------------------
(defun fd-syntax (&optional (fd *u-grammar*) 
		  &key (print-warnings t) (print-messages t)
		  (cset-attribute *cset-attribute*))
  (declare (special print-messages print-warnings))
  (catch 'fd-p (fd-p-p fd (make-path) 0 cset-attribute)))


(defun fd-p-p (fd path position cset-attribute)
  ;; position: how many pairs processed so far in this fd.
  ;; used to identify alt pairs.
  (declare (special print-messages print-warnings))
  (cond ((leaf-p fd))
	((path-p fd))
	((consp fd)
	 (and (fd-p-pair (car fd) path position cset-attribute)
	      (fd-p-p (cdr fd) path (1+ position) cset-attribute)))
	(t (fd-p-err-msg path "Unknown type: ~s" fd))))




;; ----------------------------------------------------------------------
;;    FD-P-PAIR:  determines whether or not the given argument "pair"
;;                is a legal attribute-value pair.
;;                In general, attribute can be any symbol and value
;;                can be either a symbol or an FD.
;;                But there are exceptions for the following attributes:
;;                ALT: the value that follows is a list of FD's
;;                OPT:       ,,    ,,   ,,    must be an FD
;;                PATTERN:   ,,    ,,   ,,    is a list.
;;                CSET:      ,,    ,,   ,,    is a list.
;;                CONTROL|TEST                any expr.
;; ----------------------------------------------------------------------
(defun fd-p-pair (pair path position cset-attribute &aux att)
  (declare (special print-messages print-warnings))
  ;; A Flag is always legal between pairs.
  (if (fd-p-flag pair)
      (return-from fd-p-pair t))
  (fd-p-test (consp pair) path
	     "Unknown type: ~s.  Should be a pair or a tracing flag"
	     pair)

  ;; Check validity of the attribute:
  (fd-p-test (not (fd-p-flag (car pair))) path
	     "~A is a tracing flag.  It cannot be used as an attribute in ~a"
	     (car pair) pair)

  (fd-p-test (or (leaf-p (car pair))
		 (path-p (car pair)))
	     path
	     "The attribute of a pair must be a symbol or a path: ~s"
	     pair)
  
  (setf path (if (leaf-p (car pair))
		 (path-cons (car pair) path)
	       (path-reverse (absolute-path (car pair) (path-reverse path)))))
  (setf att (if (path-p (car pair))
		(car (path-last (car pair)))
	      (car pair)))

  ;; Check now that we have just the right number of elts in the pair:
  (cond 
   ((member att '(alt ralt))
    (multiple-value-bind
	(branches traced indexed demo bk-class 
		  order wait ignore-unless ignore-when) 
	(branches pair)
      (cond 
       ((> (- (length pair)
	      (how-many traced indexed demo bk-class order wait 
			ignore-unless ignore-when))
	   2)
	(fd-p-test nil path
		   "This ~s pair has some unrecognized annotations:~%~
                    The valid annotations are:
                    <symbol> or (:trace <symbol>)~%~
                    (:demo <string>)~%~
                    (:index <path>)~%~%
                    (:bk-class {<symbol>}+)~%~
                    (:order {:random | :sequential})~%~
                    (:wait {<wait-form>|( <wait-form>+ )})~%~
                    (:ignore-when <fd>)~%~
                    (:ignore-unless <fd>)~%~
                    ~s"
		   att pair))
       ;; Check that annotations are of correct type
       ;; normalize wait can raise an error
       ((normalize-wait wait))
       ;; Ignore values need to be valid fds
       (ignore-when 
	(fd-p-test 
	 (fd-p ignore-when :print-warnings nil :print-messages nil) path
	 "The value of the :ignore-when annotation must be a valid fd."))
       (ignore-unless
	(fd-p-test 
	 (fd-p ignore-unless :print-warnings nil :print-messages nil) path
	 "The value of the :ignore-unless annotation must be a valid fd."))
       ;; :order must be either random or sequential
       (order
	(fd-p-test 
	 (or (eq order :sequential) (eq order :random))	 path
	 "The value of the :order annotation must be either :random or :sequential"))
       )

      ;; Now check validity of the branches:
      (fd-p-test (consp branches) path
		 "Value of ~s must be a list of at least one branch: ~s"
		 att branches)
      
      ;; Check that each branch is a valid fd:
      (fd-p-test
       (do ((list-of-fd branches (cdr list-of-fd))
	    (correct t)
	    (count 0 (+ 1 count)))
	   ((or (atom list-of-fd) (not correct)) correct)
	   (if (not (fd-p-p (car list-of-fd)
			    (path-cons (list att position count traced) 
				       (path-cdr path))
			    0
			    cset-attribute))
	       (setf correct nil)))
       path
       "Value of special attribute ALT must be a list of valid FDs.")))
	

   ((eq 'opt att)
    (multiple-value-bind (option flag) (option pair)
      (cond 
       (flag
	(fd-p-test (<= (length pair) 3) path
		   "An OPT pair must have at most 2 args: (OPT trace fd).~%~
                   ~s"
		   pair))
       (t
	(fd-p-test (<= (length pair) 2) path
		   "This ~s pair must have at most 1 value: (OPT fd).~%~
                   (There is no valid tracing flag in this pair.)~%~
                   ~s"
		   att pair)))
      ;; Now check that the optional fd is a valid fd:
      (fd-p-test (fd-p-p option path 0 cset-attribute)
		 (path-cons (list att position 0 flag)
			    (path-cdr path))
		 "Value of OPT must be a valid FD: ~s" pair)))

   ;; (FSET (s1 ... sn))
   ((eq att 'fset)
    (fd-p-test (= (length pair) 2) path
	       "An FSET pair must be of the form (ATT VALUE): ~s" pair)
    (fd-p-test
     (and (second pair) (consp (second pair))
	  (every #'(lambda (x) (leaf-p x)) (second pair)))
     path
     "Value of special attribute FSET must be a list of atoms: ~s" pair))

   ;; (CSET (path1 ... pathn))
   ;; PATHi <= path | symbol
   #+ignore((eq att cset-attribute)
    (fd-p-test (= (length pair) 2) path
	       "A CSET pair must be of the form (ATT VALUE): ~s" pair)
    (fd-p-test
     (and (listp (cadr pair))
	  (every #'(lambda (x) (or (leaf-p x) (path-p x))) (cadr pair)))
     path
     "Value of special attribute CSET must be a list of atoms or valid ~
      paths: ~s"
     pair))

   ;; (PATTERN (p1 ... pn))
   ;; pi <= symbol | path | (* symbol) | (* path)
   ((eq att 'pattern)
    (fd-p-test (= (length pair) 2) path
	       "A PATTERN pair must be of the form (ATT VALUE): ~s" pair)
    (fd-p-test
     (and (listp (cadr pair))
	  (every #'(lambda (l) 
		     (or (leaf-p l) 
			 (path-p l)
			 (and (consp l) 
			      (eq (car l) '*) 
			      (or (leaf-p (cadr l)) (path-p (cadr l)))
			      (null (cddr l)))))
		 (cadr pair)))
     path
     "Value of special attribute PATTERN should be a list ~
     of atoms, valid paths or mergeable atoms or paths: ~s" 
     pair))
    
   ;; (TEST expr) | (CONTROL expr)
   ((member att '(test control))
    (fd-p-test (= (length pair) 2) path
	       "A ~s pair must be of the form (ATT VALUE): ~s" 
	       att pair))

   ;; (SPECIAL special-value)
   ((member att *special-attributes*)
    (fd-p-test (= (length pair) 2) path
	       "A SPECIAL pair must be of the form (ATT VALUE): ~s" pair)
    (if (special-syntax-function att)
	(multiple-value-bind (test msg)
	    (funcall (special-syntax-function att) 
		     (second pair))
	  (fd-p-test test path msg))
      t))

   ;; (ATT #(EXTERNAL function)) | (ATT EXTERNAL)
   ((or (eq (second pair) 'external)
	(and (vectorp (second pair))
	     (> (array-dimension (second pair) 0) 0)
	     (eq (aref (second pair) 0) 'external)))
    (fd-p-test 
     (= (length pair) 2) path
     "An EXTERNAL pair must be of the form (ATT external-spec): ~s"
     pair)
    (when (vectorp (second pair))  ;; when function specified, check it
      (fd-p-test 
       (equal '(2) (array-dimensions (second pair))) path
       "An external specification must be either EXTERNAL or an array ~
       of the form #(EXTERNAL function): ~s" pair)
      (when (and print-messages print-warnings)
	(unless (functionp (aref (second pair) 1))
	  (format t "~&--- Warning: The argument of external must be a ~
                     function: ~s~%~
		       --- Path ==> ~s~%" 
		  pair (path-reverse path))))
      t))

   ;; (ATT #(UNDER atom))
   ((and (vectorp (second pair))
	 (> (array-dimension (second pair) 0) 0)
	 (eq (aref (second pair) 0) 'under))
    (fd-p-test 
     (= (length pair) 2) path
     "An UNDER pair must be of the form (ATT under-spec): ~s"
     pair)
    (fd-p-test 
     (equal '(2) (array-dimensions (second pair))) path
     "An UNDER specification must be an array ~
      of the form #(UNDER symbol): ~s" pair)
    (fd-p-test
     (symbolp (aref (second pair) 1)) path
     "The argument of an UNDER specification must be a symbol: ~s" pair)
    (when (and print-messages print-warnings)
      (unless (subtype (aref (second pair) 1))
	(format t "~&--- Warning: The argument of under does not have~%~
                     --- specializations defined: ~s~%~
                     --- Use (define-feature-type ~s (spec1 ... specn)).~%~
                     --- Path ==> ~s~%" 
		pair (aref (second pair) 1) (path-reverse path))))
    t)

   ;; ALL other cases: (ATTRIBUTE VALUE)
   (t
    (fd-p-test (= (length pair) 2) path
	       "A pair must be of the form (ATT VALUE): ~s" pair)
    (fd-p-test  (fd-p-p (cadr pair) path 0 cset-attribute) path
		"A value should be a valid FD: ~s" pair))))


;; ----------------------------------------------------------------------
;;    FD-P-FLAG: Returns T if "sym" is in acceptable form of a flag.
;;               NIL, otherwise.
;;               Right now, any symbol with "%" as its first char is
;;               accepted as a flag.
;; ----------------------------------------------------------------------
(defun fd-p-flag (sym)
  (and (symbolp sym)
       (equal *trace-marker* (char (symbol-name sym) 0))))


;; ----------------------------------------------------------------------
;;    FD-SEM:
;;                assumes correct syntax
;;    Test that a grammar is locally consistent (that is for each branch)
;;    Check the validity of indexes
;;    Count alts and branches
;; ----------------------------------------------------------------------
(defun fd-sem (&optional (fd *u-grammar*) (grammar-p t) 
	       &key (print-messages t) (print-warnings t)
	       (cset-attribute *cset-attribute*))
  "If grammar-p we test a grammar, otw we test an input"
  (declare (special grammar-p print-messages print-warnings))
  (let ((*number-of-trace* 0)
	(*number-of-index* 0)
	(*number-of-demo* 0)
	(correct nil))
    (declare (special *number-of-index* *number-of-trace*
		      *number-of-demo*))
    (catch 'fd-sem
      (setf correct (fd-sem1 fd (make-path) 0 cset-attribute)))
    (when correct
      (if grammar-p
	  (values t *number-of-index* *number-of-trace* *number-of-demo*)
	t))))
      
    
(defmacro fd-sem-err-msg (path str &rest args)
  `(progn
     (when print-messages
       (format t "~%")
       (format t ,str ,@args)
       (format t "~%PATH ==> ~s" (path-reverse ,path)))
     (throw 'fd-sem (values nil ,path))))


(defun fd-sem1 (fd path position cset-attribute)
  (declare (special grammar-p print-messages print-warnings))
  (cond ((leaf-p fd))
	((path-p fd))
	((consp fd)
	 ;; Check the fd at this level only once for contradictions
	 (and (if (= position 0) (fd-not-none fd path) t)
	      (fd-sem-pair (car fd) path position cset-attribute)
	      (fd-sem1 (cdr fd) path (1+ position) cset-attribute)))
	(t (fd-sem-err-msg path "Unknown type: ~s" fd))))


(defun fd-sem-pair (pair path position cset-attribute &aux att)
  (declare (special grammar-p print-messages print-warnings
		    *number-of-index* *number-of-trace* *number-of-demo*))
  (if (fd-p-flag pair)
      (return-from fd-sem-pair t))
  (setf att (if (path-p (car pair))
		(car (path-last (car pair)))
	      (car pair)))
  (setf path (if (leaf-p (car pair))
		 (path-cons (car pair) path)
	       (path-reverse (absolute-path (car pair) (path-reverse path)))))
  (when (and (null grammar-p) (member att *disjunctive-attributes*))
    (when (and print-messages print-warnings)
      (format t "~&--- Warning: Disjunctions in input FD: ~s~%~
                   --- Path ==> ~s~%" pair (path-reverse path))))
  (cond
   ;; Check local consistency of each branch separately:
   ((member att '(alt ralt))
    (multiple-value-bind (branches traced indexed demo) (branches pair)
      (when traced
	(incf *number-of-trace*))
      (when indexed 
	(incf *number-of-index*)
	(when (and print-messages print-warnings)
	  (check-index indexed branches path position)))
      (when demo
	(incf *number-of-demo*))
      (do ((list-of-fd branches (cdr list-of-fd))
	   (correct t)
	   (count 0 (1+ count)))
	  ((or (null list-of-fd) (not correct)) 
	   (if (not correct) (throw 'fd-sem (values nil path)) t))
	  (when print-messages (format t "."))
	  (if (not (fd-sem1 (car list-of-fd) 
			    (path-cons (list att position count traced) 
				       (path-cdr path))
			    0
			    cset-attribute))
	      (fd-sem-err-msg path "This branch is contradictory: ~s" 
			      (car list-of-fd))))))

   ((eq 'opt att)
    (multiple-value-bind (option flag) (option pair)
      (when flag 
	(incf *number-of-trace*))
      (if (not (fd-sem1 option 
			(path-cons (list att position 0 flag)
				   (path-cdr path))
			0
			cset-attribute))
	  (fd-sem-err-msg path "The value of OPT is contradictory: ~s" pair)
	t)))

   ((or (eq att 'pattern) (eq att cset-attribute))
    (unless grammar-p
      (when (and print-messages print-warnings)
	(format t "~&--- Warning: ~s should not be placed in input.~%~
	      	     --- Path ==> ~s~%" pair (path-reverse path))))
    t)

   ((member (second pair) '(any given))
    (when (and (not grammar-p) print-messages print-warnings)
      (format t "~&--- Warning: ~s should not be placed in input.~%~
		   --- Path ==> ~s~%" pair (path-reverse path)))
    t)
   
   ((or (externalp (second pair)) (underp (second pair)))
    (unless grammar-p
      (when (and print-messages print-warnings)
	(format t "~&--- Warning: ~s should not be placed in input.~%~
		     --- Path ==> ~s~%" pair (path-reverse path))))
    t)


   ((member att *special-attributes*))

   (t (fd-sem1 (second pair) path 0 cset-attribute))))



(defun fd-not-none (fd path)
  "Check that fd is not contradictory: different values for same att."
  (declare (special print-messages))
  (pushnew '%break% *trace-disabled*)
  (let* ((*global-tracing* nil)
	 (*use-given* nil)
	 (*use-any* nil)
	 (*use-wait* nil)
	 (*any-at-unification* nil)
	 (*ignore-control* t)
	 (value (u nil fd)))
    (if (eq value *fail*)
	(progn
	  (when print-messages 
	    ;; If pb: redo unif with trace on to identify the culprit:
	    (format t "~%")
	    (let* ((*global-tracing* t)
		   (*use-given* nil)
		   (*use-any* nil)
		   (*use-wait* nil)
		   (*ignore-control* t)
		   (*any-at-unification* nil)
		   (value (u nil (cons '%FD-SEM% fd))))
	      (declare (ignore value))))
	  (fd-sem-err-msg path "FD is contradictory: ~s" fd))
      t)))



;; ----------------------------------------------------------------------
;;   CHECK-INDEX : checks that the index declared in an alt is consistent
;;                 with what's in the branches.
;;                 That is: 
;;                 1/ the path leads to an existing value in all branches
;;                 2/ all the values for the index in the branches are
;;                 different 
;;   ALWAYS return t: just prints warning msgs.
;; ----------------------------------------------------------------------
(defun check-index (index branches path position)
  (let* ((list-of-keys (mapcar 
			#'(lambda (fd) (top-gdp fd (make-path :l index)))
			branches))
	 (branchpos (or (position nil list-of-keys)
			(position 'any list-of-keys)
			(position 'given list-of-keys)
			(position 'none list-of-keys))))
    (if branchpos
	(format t "~&--- Warning: branch #~s does not have a value ~
                         for index ~s~%~
                     --- Path ==> ~s~%"
		(1+ branchpos)
		index 
		(path-reverse 
		 (path-cons (list 'alt position)
			    (path-cdr path))))
      (if (equal list-of-keys (remove-duplicates list-of-keys))
	  t
	(format t "~&--- Warning: several branches have ~
		         the same value for index ~s~%~
                     --- Path ==> ~s~%"
		index
		(path-reverse 
		 (path-cons (list 'alt position)
			    (path-cdr path)))))))
  t)


;; ----------------------------------------------------------------------
;;    GRAMMAR-P:
;;                checks that an FD is a well-formed grammar
;;                ((alt (((cat x1)  ) ... ((cat xn) ...))))
;; ----------------------------------------------------------------------
(defun grammar-p (&optional (grammar *u-grammar*) 
		  &key (print-messages t) (print-warnings t)
		  (expansion t)
		  (cset-attribute *cset-attribute*))
  "Predicate returning T iff grammar is a well-formed FUG, i.e. a
   well-formed FD of the form: ((ALT (((CAT x1) ... ) ... ((CAT xN) ...))))"

  ;; Check if this is a def-alt or a def-conj
  (setf grammar (get-grammar grammar :expansion expansion))
  (when (null grammar) (return-from grammar-p nil))
  (let ((correct (fd-syntax grammar 
			    :print-messages print-messages
			    :print-warnings print-warnings
			    :cset-attribute cset-attribute)))
    (when correct 
      (when print-messages
	(format t "~&Syntax is correct.~%")
	(format t "Checking for contradictions and validity of indexes: "))
      (multiple-value-bind 
	  (correct indexes traces demos)
	  (fd-sem grammar t 
		  :print-messages print-messages 
		  :print-warnings print-warnings
		  :cset-attribute cset-attribute)
	(setq correct
	      (and correct
		   (null (cdr grammar))
		   (eq 'alt (caar grammar))
		   #+ignore
		   (check-index '(cat) (second (car grammar)) (make-path) 0)))
	(if correct
	    (progn
	      (when print-messages
		(format t "~&The grammar is correct.~%~
			It contains ~D indexed alt~:P, ~
                        ~D demo message~:P ~
			and ~D traced alt~:P.~%" 
			indexes demos traces))
	      t)
	  (progn 
	    (when print-messages
	      (format T "~%A grammar should be a valid FD of the form:~%~
  		    ((ALT (((CAT x1) ... ) ... ((CAT xN) ...))))~%~
		    (It is implicitly indexed by the feature CAT.)~%"))
	    nil))))))


;; -----------------------------------------------------------------------
;; Checks an input fd (an fd with no alt)
;; -----------------------------------------------------------------------
(defun fd-p (fd &key (print-messages t) (print-warnings t) 
		(cset-attribute *cset-attribute*))
  (let* ((fd (filter-flags (prep-input fd)))
	 (syntax (fd-syntax fd 
			    :print-messages print-messages
			    :print-warnings print-warnings
			    :cset-attribute cset-attribute)))
    (if syntax
	(progn
	  (when print-messages (format t "Syntax is correct.~%"))
	  (when
	      (fd-sem fd nil
		      :print-messages print-messages
		      :print-warnings print-warnings
		      :cset-attribute cset-attribute)
	    (when print-messages (format t "No contradictions found.~%"))
	    t))
      (when print-messages (format t "Syntax is incorrect.~%")))))


;; -----------------------------------------------------------------------
;; Normalize an input fd so that it works fine with unify
;; -----------------------------------------------------------------------
(defun normalize-fd (fd)
  (let ((*use-given* nil)
	(*use-any* nil)
	(*use-wait* nil)
	(*ignore-control* t)
	(*any-at-unification* nil))
    (pushnew '%break% *trace-disabled*)
    (filter-flags (u nil (prep-input fd)))))

;; -----------------------------------------------------------------------
;; ALT-GDP
;; -----------------------------------------------------------------------
(defun alt-gdp (fd path &optional (reverse-current-path (make-path)))
  ;; A version of gdp that knows about alts and opts.
  ;; In a path: the spec. determining an alt branch is (ALT pos branch#)
  ;; where pos is the pos of the alt pair in the embedding fd, and 
  ;; branch# is the position of the branch within the list of branches.
  ;; A spec of the form (ALT pos) points to the whole alt pair. It must be
  ;; the last element of a path to be legal.
  ;; For opt, spec is (OPT pos).
  ;; The reason is that there can be many ALT or OPTs in a single FD, so
  ;; ALT alone would not uniquely identify which ALT is spec.
  ;; This version of gdp can return a path value.
  ;; Does not check for cycles.
  ;; Does not check for FSET.
  ;; Does not check for SPECIALs.
  (cond 
   ((or (path-null path)   ;; termination test first.
	(not (consp fd)))
    (cond 
     ((path-null path) fd)
     ;; Do we stop at any or do we go below it: below is unspecified.
     ((eq fd 'any) (if (path-null path) 'any nil))
     ((eq fd 'given) (if (path-null path) 'given nil))
     ((eq fd nil) nil)
     ;; Below any other atom is impossible: none
     ((leaf-p fd) 'none)
     ;; For a path: return a path to the value actually pointed to.
     ((path-p fd) 
      (path-append (absolute-path fd (path-reverse reverse-current-path))
		   path))
     (t (error "Unknown type: ~s" fd))))

   ((and (consp (path-car path))
	 (member (car (path-car path)) '(alt ralt))
	 (null (third (path-car path))))
    (if (path-null (path-cdr path))
	(go-down-one-level fd (path-car path))
      (error "A path spec. of the form (ALT pos) must appear last in a ~
                path: ~s" path)))

   ((consp fd)
    (alt-gdp (go-down-one-level fd (path-car path))
	     (path-cdr path)
	     (path-push (path-car path) reverse-current-path)))
   
   (t (error "Not reached"))))

(defun go-down-one-level (fd item)
  (cond 
   ((leaf-p item) (safe-second (safe-assoc item fd)))
   ((consp item)
    ;; Deal with a (alt position branch) or (opt position):
    (let ((pair (nth (second item) fd)))
      (cond 
       ((or (eq (car item) 'alt)
	    (eq (car item) 'ralt))
	(if (third item)
	    (let ((branches (branches pair)))
	      (nth (third item) branches))
	  (list pair)))
       ((eq (car item) 'opt)
	(option pair))
       (t (error "Unknown spec in path: ~s" item)))))
   (t (error "Unknown spec in path: ~s" item))))


(defun get-error-pair (fd)
  (multiple-value-bind (correct path) (fd-syntax fd)
    (declare (ignore correct))
    (alt-gdp fd (path-butlast (path-reverse path)))))



;; -----------------------------------------------------------------------
(provide "$fug5/checker")
;; -----------------------------------------------------------------------
