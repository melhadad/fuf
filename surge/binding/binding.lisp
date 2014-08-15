;;; -*- Mode:Lisp; Syntax:Common-Lisp; Package: FUG5 -*-
;;; -----------------------------------------------------------------------
;;; File:         binding.lisp
;;; Description:  Implementing binding theory for SURGE
;;; Author:       Yael
;;; Created:      18 Mar 1996
;;; Modified:     18 Apr 1996 - Revision of code
;;; Package:      FUG5
;;; -----------------------------------------------------------------------
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

(load "voice.l")
(load "linearize3")

;;; Binding processing is not yet integrated into the linearizer code.
;;; The following two functions add it as a single shot post-process.
;;; Eventually, the call to binding will be part of determine.

(defun try (fd1)
  (let ((fd (uni-fd fd1)))
    (declare (special *input*))
    (if (eq fd :fail)
	fd
      (let ((*input* fd))
	(binding {})
	(print-sentence
	 (capitalize (punctuate (linearize *input* {}) '())))))))

;;; This is to test binding within NPs.
(defun try2 (fd1)
  (let ((fd (uni-fd fd1)))
    (declare (special *input*))
    (if (eq fd :fail)
	fd
      (let ((*input* fd))
	(bt-within-np {} '())
	(print-sentence
	 (capitalize (punctuate (linearize *input* {}) '())))))))



;; Problems
;; Sentences like
;; "In order to post a site to the WWW, you will want to post it on a server."

;;; RECIPROCALS
;;; When 'each other' refers to a conjunction of noun - must be checked,
;; for now I only check if its plural, but conjunction is not
;;; 'Anne and Cathy met each other'


;; Type of inputs to be processed by this module:
;; (def-test b1
;;  "Mary likes herself."
;;  ((cat clause)
;;   (process ((type mental) (lex "like")))
;;   (partic ((processor ((cat proper) (lex "Mary") (gender feminine)))
;;            (phenomenon ((cat proper) (index {^2 processor index})))))))
;;

;; Motivation: content planner and lexicalizer do not know anything about
;; anaphora and binding.  So they prepare the constituents for NPs (propers
;; or common or pronouns) without knowing even if an occurrence might
;; become an anaphora.
;;

;; [YD] o-binders is valid to referential o-commanders, so it should be checked too.
;; In the meantime, we keep (cat proper) in the input and add a feature
;; (binding anaphor/npro/ppro) with type hierarchy:
;;              binding
;;             /      \
;;      referential expletive
;;         /   \         /   \
;;      npro   pro      it   there
;;            /   \
;;          ppro  anaphor
;;                /   \
;;         reflexive   reciprocal
;;
(define-feature-type binding (referential expletive))
(define-feature-type referential (npro pro))
(define-feature-type pro (ppro anaphor))
(define-feature-type anaphor (reflexive reciprocal))
(define-feature-type expletive (expletive-it expletive-there))


;; How to implement:
;; Can be done after all unification is complete - since the decision only
;; affects the type of an NP (from common, proper or pronoun to pronoun or
;; anaphora) and not the structure of the sentence.
;; Therefore can be done just before linearization (or at the same time).
;; and can be done in Lisp (since want to do recursive traversal of the
;; obliqueness structure of the sentence).

;; Procedure:
;; Traverse oblique structure, and for all NP, store the list of
;; local-o-binders.
;; must traverse also peripherials (circum can be a clause)

;; How to get oblique structure:
;; Add to fd features o-command and o-binder:
;; Instead, have grammar add a feature (o-command 1...) to each complement
;; (both participants and circumstantials).
;; These features are given in the voice system to the NPs according to their
;; syntactic feature.

;; ======================================================================
;; Entry point: binding
;; Called on the output of uni-fd
;; Assumptions: the voice system has introduced an o-command feature
;;              in each syntactic role (subject ((o-command 1) ...))
;;                                     (object  ((o-command 2) ...))
;; Argument Path is the position of FD (a clause) within the total FD.
;; ======================================================================

;;[YD] We should note the "complete" heirarchy where expletive subject cannot
;; o-bind anything, so in any case we have to check the value in synt-roles.


;; ASSUMPTION: interface with the grammar is with the functions
;; (get-pred-modifiers), (get-sentence-modifiers), (get-disjuncts) that
;; return lists of roles of each type that are defined in the grammar.

;; The output is a list of relevant roles with descending obliqueness.
(defun get-complements (path)
  "Return a list of paths to all the complements in the clause appearing at
   level path within the total fd"
  (declare (special *input*))
  (append
   (paths-to-relevant-complements path  'circum (get-disjuncts))
   (paths-to-relevant-complements path  'circum (get-sentence-modifiers))
   (paths-to-relevant-complements path  'pred-modif (get-pred-modifiers))
   (paths-to-relevant-synt-roles path)))


;;; synt-roles gets its obliqueness value in voice.l, in features (o-command n)
;;; Meanwhile, I still think its needed because not every subject is less oblique
;;; than its object, like in 'it' and 'there' (non-referential subjects).


(defun paths-to-relevant-synt-roles (path)
  "For the given path, return a list sorted by obliqueness of paths
   to synt-roles in the *input*"
  (sort-complements
    (mapcar
      #'(lambda (role) (path-append path
                                    (make-path :l (list 'synt-roles role))))
        (gdp *input* (path-append path {synt-roles fset})))))


(defun paths-to-relevant-complements (path complement-type complements
                                      &optional acc)
  "For a given path, complement-type is the type of modifier
   (pred, sentence, disjunct) and a list of the possible complements,
   and returns the paths which are relevant to the FD"
  (if (null complements) acc
    (let* ((check-path
            (path-append
             path
             (make-path :l (list complement-type (car complements)))))
	   (value (gdp *input* check-path)))
      (paths-to-relevant-complements
       path complement-type
       (cdr complements)
       (if (or (null value) (subsume 'none value))
           acc
           (cons check-path acc))))))

;;; Question: there is no definite separation in order of both, so perhaps
;;; it should be in one list.
(defun get-sentence-modifiers ()
  '(manner accompaniment opposition behalf distance location duration
    frequency time means reason purpose addition comparison))

(defun get-disjuncts ()
  '(matter standard perspective concession constrast exception substitution
    condition concessive-condition inclusion co-event result))


;; Given a list of paths to all the complements in a clause, sort them by
;; value of obliqueness.
;; Assumption: the voice system has inserted a feature (o-command i) in
;; each complement.
(defun sort-complements (lpaths)
  (mapcar #'car
	  (sort (map-obliqueness lpaths) #'> :key #'cdr)))

(defun map-obliqueness (lpaths)
  (mapcar
   #'(lambda (path)
       (cons path (gdp *input* (path-extend path 'o-command))))
   lpaths))


;; Add dispatch to different kinds of  bindings

(defun binding (path &optional non-local-commanders)
  "Traverse the fd appearing at level path within the total fd
   to determine the types of NPs according to Binding Theory.
   Physically modify the total FD and return it at the end."
  ;; first find the syntactic roles participating then check:
  ;;  1. category (recurse into (cat clause))
  ;;  2. sort by o-command values, make a list of all o-commanders and check
  ;;     coreference with them.
  ;;     (look at each {synt-role subject/object.. o-command} values.)
  ;;  3. accordingly determine which principle holds.

  ;; Non-local-commanders is a list of commanders that can come from
  ;; discourse (non-local to the clause).  It is only used for pronouns
  ;; (not for anaphors) and for pronouns, non-local-commanders appear after
  ;; all local ones.

  ;; complements is the list all the complements appearing within the
  ;; clause appearing at level path within the total fd, sorted by
  ;; obliqueness. (subject last).
  ;; NOTE: use maplist on obliques to obtain:
  ;; (3 2 1) then (2 1) then (1)
  (let ((complements (get-complements path)))

    (maplist
     #'(lambda (commanders)
	 (BT (car commanders)
	     (cdr commanders)
	     non-local-commanders))
     complements))
  *input*)


(defun BT-within-NP (path non-local-commanders)
  "Check if there are binders inside the NP"
  (let* ((oblique-list (paths-to-relevant-np-modifiers
                        path (get-np-modifiers))))
    ;;; make a list of possible NPs
    (maplist
     #'(lambda (commanders)
	 (BT (car commanders)
	     (cdr commanders)
	     non-local-commanders))
     oblique-list)
    (values)))

(defun BT-within-LIST (path  commanders non-local-commanders)
  "Check if there are binders inside a list
   (such as a list of qualifiers of a noun"
  ;; I assume the last in the list is the most oblique one,
  ;; and therefore I'd like to bind it first.
  ;; so I have to reverse the list (a list of paths to the list components)
  ;; and then to send it again to binding procedure.
  (let*
      ((oblique-list (append (get-list-components path) commanders)))
    (maplist
     #'(lambda (commanders)
	 (BT (car commanders)
	     (cdr commanders)
	     non-local-commanders))
     oblique-list)
    (values)))

;; ======================================================================
;; BT gets a path to a complement, a list of local commanders
;; and a list of non-local-commanders for this complement.
;; It first checks the category of the complement.
;; - If it is a clause, it recursively calls binding on this clause with the
;;   local-commanders of the embedding clause added to the list of
;;   non-local-commanders of the embedded clause.
;; - If it is an NP, it checks first Principle A:
;;   if local-o-commanders are coreferenced:
;;   yes: enrich FD with feature (binding anaphor).  The verb decides whether
;;        it will be a reflexive or a reciprocal.  Default is reflexive.
;;   no : check if o-free (not coindexed with local and non-local
;;              commanders) --> make it (binding npro)
;;        if not make him (binding ppro)
;; ======================================================================

;; Deal with CLAUSE, NP and PPs.
;; All other cats don't need any special treatment.
(defun BT (complement-path commanders non-local-commanders)
  (declare (special *input*))
  (let ((category (gdp *input* (path-extend complement-path 'cat))))
    (cond
     ((subsume 'list category)
      (BT-within-LIST complement-path commanders non-local-commanders))

     ;; if clause - recurse on it with the upper o-commands for o-free of
     ;; r-expressions.
     ((subsume 'simple-clause category)
      (binding complement-path
               (append commanders non-local-commanders)))

     ((subsume 'np category)
      (set-binding complement-path category commanders non-local-commanders)
      (if (subsume 'common category)
          (BT-within-NP complement-path
                        (append commanders non-local-commanders))))

     ;; For PPs, get the NP within the NP.
     ((subsume 'pp category)
      (set-binding (path-extend complement-path 'np)
                   (gdp *input* (path-append complement-path {np cat}))
                   commanders
		   non-local-commanders)))))



(defun set-binding (complement-path category commanders non-local-commanders)
  "Add the binding feature of an NP (given its path and category),
   decision: coreference with commanders - anaphor: coreference with
   non-local-commanders - ppro, otherwise npro or ppro (if given as a pronoun)."
  (cond ((coreferenced complement-path commanders)
	 (enrich  (gdp *input* complement-path)
		  '(binding anaphor) (make-frame)))

	;; If cat is already a pronoun in the input, and it is not
	;; an anaphor, no need to check further.
	((subsume 'personal-pronoun category)
         (enrich  (gdp *input* complement-path)
		  '(binding ppro) (make-frame)))

        ;;; check if must be a pronoun, if yes, add (forced-pronoun yes)
        ;;; this is needed to the morphology, to ignore a given lex.
        ((coreferenced complement-path non-local-commanders)
	 (enrich  (gdp *input* complement-path)
		  '(binding ppro) (make-frame))
         (enrich (gdp *input* complement-path)
                 '(forced-pronoun yes) (make-frame)))

	;; If it is not o-free (coindexed with a non-local commander)
	;; must be a ppro.
	;; If complement is free (including case of no commanders),
	;; make it an npro.
	(t (enrich (gdp *input* complement-path)
		   '(binding npro) (make-frame)))))


;; Get-index: path to constituent
;; Return value of index.
;; If constituent is an NP, take its index feature
;; if it is a PP, take the index of its np.
(defun get-index (path)
  (declare (special *input*))
  (let ((cat (gdp *input* (path-extend path 'cat))))
    (when (subsume 'pp cat)
	  (setf path (path-extend path 'np)))
    (gdp *input* (path-extend path 'index))))

;; Coreferenced returns the sublist of commanders-path argument with the
;; same index.
(defun coreferenced (path1 commanders-paths)
  (declare (special *input*))
  (let ((index1 (get-index path1)))
    ;; let each one of nps be a list with: path
    (loop for path in commanders-paths
	  when (eq index1 (get-index path))
	  collect path)))

;; Binding within NP:
;; possessor is like a 'subject' - cannot be commanded
;; Can be commanded: head, objects of PP
;; In cases like "John bought a picture of himself" - to get 'himself' you
;; must add a feature (reflexive yes) since it does not have to be a reflexive,
;; like "I met a friend of mine" , so I guess this is a matter of 'picture
;; nouns' and the grammar cannot know it.

;; This procedure is called by 'binding-within-np'. The complement list
;; is generated by (get-np-modifiers)
(defun paths-to-relevant-np-modifiers (path complements &optional acc)
  (if (null complements) acc
    (let* ((check-path (path-extend path (car complements)))
	   (value (gdp *input* check-path)))
      (paths-to-relevant-np-modifiers
       path
       (cdr complements)
       (if (or (null value) (subsume 'none value))
	   acc
	 (cons check-path acc))))))

(defun get-np-modifiers ()
  '(possessor qualifier))

;;;; This function gets a path to an FD with ((cat list) (distinct ...))
;;;; and returns a list of paths to the distinct components of it,
;;;; in reverse order (we assume the more oblique part come after in the
;;;; list.
(defun get-list-components (path)
  "convert the FD (cat list) to a list, and collect the paths to
   its different components"
  (let ((fd-list (fd-to-list *input* (path-extend path 'distinct))))
     (get-list-components-aux fd-list (path-extend path 'distinct)
                              {car} '())))

(defun get-list-components-aux (fd-list path path-in-list acc)
  (if (null fd-list) acc
    (get-list-components-aux (cdr fd-list)
                             path
                             (path-append {cdr} path-in-list)
                             (cons (path-append path path-in-list) acc))))
