;;; -*- Mode:Lisp; Syntax:Common-Lisp; Package: -*-
;;; -----------------------------------------------------------------------
;;; File:         voice.lisp
;;; Description:  Grammatical system of the clause dealing with voice
;;; Author:       Michael Elhadad
;;; Created:      19 Dec 1991
;;; Modified:     07 Jan 1992: add a realization link when mapping is not
;;;                            direct constituent to constituent.  To be
;;;                            used by the relative system and add info
;;;                            about relative-marker and question-pronoun.
;;;               10 Aug 1992: added clause-level/scoped feature to
;;;                            indicate if clause is top-level or no.
;;;               14 Sep 1992: added clause-level/embedded which is
;;;                            different from scoped.
;;;                            "Who is coming" is scoped but not embedded.
;;;                            embedded necessary to determine if subject
;;;                            inversion is required in "What happens" vs.
;;;                            "What do you think happens".
;;;               27 Jun 1993: added passive-prep (to override "by" in
;;;                            "X is contained in Y", "X is composed of Y").
;;;               05 Jul 1995: SURGE 2.2 VERSION
;;;                            - Allow for no oblique roles
;;;                              - partic-less clauses
;;;                              (e.g. non-effective imperative)
;;;                            - Copy agentless under process
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

(def-alt voice (:index process-type)
  ;; VOICE SYSTEM:
  ;; Map from obliqueness to syntactic roles
  ;; When more than 1 participant:
  ;; Choice receptive/operative should be based on focus *****
  ;; Nuclear Syntactic functions mapped to are:
  ;; Subject, object, iobject, dative, by-obj, subj-comp, obj-comp
  ;; subj-comp and obj-comp from Quirk, examples are:
  ;; She made him a good wife     [S V O SC] = [Ag/Ca Af At]
  ;; She made him a good husband  [S V O OC] = [Ag Af/Ca At]
  ;; In our analysis with composite processes, the distinction is
  ;; determined by what function the carrier fulfils.
  ;; Other complements are added by the circumstantial roles one at a time.
  ;; Things done:
  ;; - Map from oblique to synt-roles
  ;; - Determine opportunities for passivation
  ;; Feature agentless determines whether a by-obj is used in passive
  ;; Feature dative-move determines whether a recipient is realized as
  ;; iobject or as dative pp.
  ;; Special cases:
  ;; - natural phenomenon (it rains)
  ;; - existential (there is a bug)
  ;; - ascriptive equative passive
  (
   ;; NATURAL-PHENOMENON
   ((process-type #(under natural-phenom))
    (process ((voice active)))
    (cset ((- {^ synt-roles object} {^ synt-roles subject}
	      {^ synt-roles subj-comp}
	      {^ synt-roles obj-comp} {^ synt-roles iobject}
	      {^ synt-roles dative} {^ synt-roles by-obj})))
    (synt-roles ((fset (subject))
		 (subject ((lex "it")
			   (cat personal-pronoun)
			   (number singular))))))

   ;; EXISTENTIAL
   ((process-type #(under existential))
    (cset ((- {^ synt-roles object}
	      {^ synt-roles subject}
	      {^ synt-roles obj-comp} {^ synt-roles iobject}
	      {^ synt-roles dative} {^ synt-roles by-obj})))
    (synt-roles ((fset (subject subj-comp))
		 (subject ((lex "there")
			   (cat demonstrative-pronoun)
			   (number {^ ^ ^ oblique 1 number})))
		 (subj-comp {^ ^ oblique 1}))))

   ;; ASCRIPTIVE EQUATIVE: passive with copula is special
   ;; passive is just a swap of the order of arguments around the copula.
   ((process-type #(under ascriptive))
    (process ((mode equative)))
    (:! equative-voice))

   ;; ANY OTHER PROCESS
   ((dummy-constituent none)
    (:! voice-normal))))

;; ===============================
;; 7 possible synt-roles
;; subject, object, iobject, subj-comp, obj-comp, dative, by-obj
;; Each one is either present (cat specified) or not
;; If present, normalize synt-funct and propagate clause-level.
;; ===============================

(def-alt subject-voice
    (:wait {^ synt-roles subject cat})
  (((synt-roles ((subject none))))
   ((synt-roles ((subject ((synt-funct subject)
                           (clause-level ((scoped {^4 scoped})
                                          (embedded {^4 embedded}))))))))))
(def-alt object-voice
    (:wait (({^ synt-roles object cat} given)))
  (((synt-roles ((object none))))
   ((synt-roles ((object ((synt-funct object)
                          (clause-level ((scoped {^4 scoped})
                                         (embedded {^4 embedded}))))))))))
(def-alt subj-comp-voice
    (:wait (({^ synt-roles subj-comp cat} given)))
  (((synt-roles ((subj-comp none))))
   ((synt-roles ((subj-comp ((synt-funct subj-comp)
                             (clause-level ((scoped {^4 scoped})
                                            (embedded {^4 embedded}))))))))))

(def-alt obj-comp-voice
    (:wait (({^ synt-roles obj-comp cat} given)))
  (((synt-roles ((obj-comp none))))
   ((synt-roles ((obj-comp ((synt-funct obj-comp)
                            (clause-level ((scoped {^4 scoped})
                                           (embedded {^4 embedded}))))))))))

(def-alt iobject-voice
    (:wait (({^ synt-roles iobject cat} given)))
  (((synt-roles ((iobject none))))
   ((synt-roles ((iobject ((synt-funct iobject)
                           (syntax ((case objective)))
                           (clause-level ((scoped {^4 scoped})
                                          (embedded {^4 embedded}))))))))))

(def-alt dative-voice
    (:wait (({^ synt-roles dative cat} #(under pp))))
  (((synt-roles ((dative none))))
   ((synt-roles ((dative ((synt-funct dative)
                          (clause-level ((scoped {^4 scoped})
                                         (embedded {^4 embedded})))
			  (question-embedded yes)
			  (relative-embedded yes))))))))

(def-alt by-obj-voice
    (:wait (({^ synt-roles by-obj cat} #(under pp))))
  (((synt-roles ((by-obj none))))
   ((synt-roles ((by-obj ((cat pp)
                          (synt-funct by-obj)
                          (clause-level ((scoped {^4 scoped})
                                         (embedded {^4 embedded}))))))))))

(def-conj voice-synt-roles
  (:! subject-voice)
  (:! object-voice)
  (:! iobject-voice)
  (:! subj-comp-voice)
  (:! obj-comp-voice)
  (:! dative-voice)
  (:! agentless)
  (:! by-obj-voice))


(def-alt equative-voice (:index (process voice))
  ;; Deal with the passive of the copula: X is Y / Y is X.
  (((process ((voice active)))
    (cset ((- {^ synt-roles object}
	      {^ synt-roles obj-comp} {^ synt-roles iobject}
	      {^ synt-roles dative} {^ synt-roles by-obj})))
    (synt-roles ((fset (subject subj-comp))
		 (subject {^ ^ oblique 1})
		 (subj-comp {^ ^ oblique 2}))))
   ((process ((voice passive)))
    (alt equative-passive (:index (process voice))
      (((process ((copula no)))
	(cset ((- {^ synt-roles object} {^ synt-roles subj-comp}
		  {^ synt-roles obj-comp} {^ synt-roles iobject}
		  {^ synt-roles dative})))
	(:! clause-level-at-1)
	(synt-roles
         ((fset (subject by-obj))
          (subject {^2 oblique 2})
          (alt subject-equative-copula-no
               (:wait (({^ subject cat} #(under lexical-cat))))
               (((subject none))
                ((subject ((question-pronoun ((restrictive yes)))))))))))

       ;; This is the real special case
       ((process ((copula yes)))
	(cset ((- {^ synt-roles object}
		  {^ synt-roles obj-comp} {^ synt-roles iobject}
		  {^ synt-roles dative} {^ synt-roles by-obj})))
	(synt-roles
         ((fset (subject subj-comp))
          (subject {^ ^ oblique 2})
          (subj-comp {^ ^ oblique 1})
          (alt subject-equative-copula-yes
               (:wait (({^ subject cat} #(under lexical-cat))))
               (((subject none))
                ((subject ((question-pronoun ((restrictive yes))))))))))
          (alt subj-comp-equative-copula-yes
               (:wait (({^ subj-comp cat} #(under lexical-cat))))
               (((subj-comp none))
                ((subj-comp ((question-pronoun ((restrictive no))))))))))))))


(def-alt voice-normal 
  ;; Voice system when there is no dummy constituent like it/there
  ;; All patterns of obliqueness are:
  ;; 1, 12, 123, 124, 13, 14
  ;; For all passives, the decision with/wo by-obj is made in the
  ;; agent-less alt.
  (
   ;; JR 1-25-93, to allow partic-less clauses (e.g. non-effective imperative)
   ;; Go, Rest.
   ((oblique none)
    (cset ((- {^ synt-roles subject} {^ synt-roles object}
              {^ synt-roles subj-comp}
	      {^ synt-roles obj-comp} {^ synt-roles iobject}
	      {^ synt-roles dative} {^ synt-roles by-obj}))))

   ((oblique ((fset (1))))
    ;; (process ((voice active)))
    (cset ((- {^ synt-roles object} {^ synt-roles subj-comp}
	      {^ synt-roles obj-comp} {^ synt-roles iobject}
	      {^ synt-roles dative} {^ synt-roles by-obj})))
    (synt-roles ((fset (subject))
		 (subject {^2 oblique 1}))))

   ((oblique ((fset (1 2))
              (2 any)))
    (:! voice-12))

   ((oblique ((2 any)
	      (3 any)
	      (fset (1 2 3))))
    (:! voice-123))

   ((oblique ((2 any)
	      (4 any)
	      (fset (1 2 4))))
    (:! voice-124))

   ((oblique ((fset (1 3))
	      (3 any)))
    (:! voice-13))

   ((oblique ((fset (1 4))
	      (4 any)))
    (process ((voice active)))
    (cset ((- {^ synt-roles object}
	      {^ synt-roles obj-comp} {^ synt-roles iobject}
	      {^ synt-roles dative} {^ synt-roles by-obj})))
    (synt-roles ((fset (subject subj-comp))
		 (subject {^ ^ oblique 1})
		 (subj-comp {^ ^ oblique 4}))))))


(def-alt voice-12 (:index (process voice))
  ;; Voice mapping when oblique = (1 2)
  (((process ((voice active)))
    (cset ((- {^ synt-roles subj-comp}
	      {^ synt-roles obj-comp} {^ synt-roles iobject}
	      {^ synt-roles dative} {^ synt-roles by-obj})))
    (synt-roles ((fset (subject object))
		 (subject {^ ^ oblique 1})
		 (object  {^ ^ oblique 2}))))
   ((process ((voice passive)
	      (copula no)))
    (cset ((- {^ synt-roles object} {^ synt-roles subj-comp}
	      {^ synt-roles obj-comp} {^ synt-roles iobject}
	      {^ synt-roles dative})))
    (:! clause-level-at-1)
    (synt-roles ((fset (subject by-obj))
		 (subject {^ ^ oblique 2}))))))


(def-alt voice-123 (:index dative-move) (:bk-class dative-move)
  ;; Voice mapping when oblique = (1 2 3)
  ;; No dative move if possessor is scope of question or relative.
  ;; Easiest way around is to add question-embedded yes to possessor.
  ;; and a prep of to even if there is a dative move.
         ;; (:wait {^ dative-move})
  (((dative-move yes)
    (alt voice-123-dative (:index (process voice))
      (((process ((voice active)))
	(cset ((- {^ synt-roles subj-comp}
		  {^ synt-roles obj-comp}
		  {^ synt-roles dative} {^ synt-roles by-obj})))
	(synt-roles ((fset (subject object iobject))
		     (subject {^ ^ oblique 1})
		     (iobject {^ ^ oblique 2})
		     (object  {^ ^ oblique 3}))))
       ((process ((voice passive)
		  (copula no)))
	(cset ((- {^ synt-roles subj-comp}
		  {^ synt-roles obj-comp} {^ synt-roles iobject}
		  {^ synt-roles dative})))
	(:! clause-level-at-1)
	(synt-roles ((fset (subject object by-obj))
		     (subject {^ ^ oblique 2})
		     (object  {^ ^ oblique 3})))))))
   ((dative-move no)
    (alt voice123-no-dative (:index (process voice))
      (((process ((voice active)))
	(cset ((- {^ synt-roles subj-comp}
		  {^ synt-roles obj-comp} {^ synt-roles iobject}
		  {^ synt-roles by-obj})))
	(synt-roles ((fset (subject object dative))
		     (subject {^2 oblique 1})
		     (dative ((np {^3 oblique 2})
			      (np ((clause-level {^4})))))
		     (object  {^2 oblique 3})))
	(oblique ((2 ((cset ((- realization)))
		      (realization {^3 synt-roles dative}))))))
       ((process ((voice passive)
		  (copula no)))
	(:! clause-level-at-1)
	(cset ((- {^ synt-roles object} {^ synt-roles subj-comp}
		  {^ synt-roles obj-comp} {^ synt-roles iobject})))
	(synt-roles ((fset (subject dative by-obj))
		     (subject {^ ^ oblique 3})
		     (dative ((np {^ ^ ^ oblique 2})))))
	(oblique ((2 ((cset ((- realization)))
		      (realization {^3 synt-roles dative})))))))))))


(def-alt voice-124 (:index (process voice))
  ;; Voice mapping when oblique = (1 2 4)
  (((process ((voice active)))
    (cset ((- {^ synt-roles iobject} {^ synt-roles subj-comp}
	      {^ synt-roles dative} {^ synt-roles by-obj})))
    (synt-roles ((fset (subject object obj-comp))
		 (subject {^ ^ oblique 1})
		 (object  {^ ^ oblique 2})
		 (obj-comp {^ ^ oblique 4}))))
   ((process ((voice passive) (copula no)))
    (cset ((- {^ synt-roles object}
	      {^ synt-roles obj-comp} {^ synt-roles iobject}
	      {^ synt-roles dative})))
    (:! clause-level-at-1)
    (synt-roles ((fset (subject by-obj subj-comp))
		 (subject {^ ^ oblique 2})
		 (subj-comp {^ ^ oblique 4}))))))


(def-alt voice-13 (:index (process voice))
  ;; Voice mapping when oblique = (1 3)
  (((process ((voice active)))
    (cset ((- {^ synt-roles subj-comp}
	      {^ synt-roles obj-comp} {^ synt-roles iobject}
	      {^ synt-roles dative} {^ synt-roles by-obj})))
    (synt-roles ((fset (subject object))
		 (subject {^ ^ oblique 1})
		 (object  {^ ^ oblique 3}))))
   ((process ((voice passive)
	      (copula no)))
    (cset ((- {^ synt-roles object} {^ synt-roles subj-comp}
	      {^ synt-roles obj-comp} {^ synt-roles iobject}
	      {^ synt-roles dative})))
    (:! clause-level-at-1)
    (synt-roles ((fset (subject by-obj))
		 (subject {^ ^ oblique 3}))))))

;; Before deciding whether to use a by-obj or not, place a pointer to the
;; clause-level in the constituent that would become by-obj.
(def-alt clause-level-at-1
    (:wait {^ oblique 1 cat})
  (((oblique ((1 none))))
   ((oblique ((1 given)
	      (1 ((clause-level ((scoped {^4 scoped})
				 (embedded {^4 embedded}))))))))))

(def-alt agentless (:index (process voice))
  ;; Decide whether by-obj is included in passive (non ascriptive equative)
  (((process ((voice active))))
   ((process ((voice passive)
	      (agentless {^2 agentless})))
    (alt agentless2 (:index agentless)
      ;; If by-obj would be scope of question or relative, it must be used.
         (:wait {^ synt-roles by-obj cat})
      (((agentless no)
	(oblique ((1 given)
		  (1 ((realization {^3 synt-roles by-obj})
		      (cset ((- realization)))))))
	(synt-roles
	 ((by-obj ((cat pp)
                   (np {^3 oblique 1})
		   ;; (clause-level ((scoped {^4 scoped})))
		   (relative-embedded yes)
		   (question-embedded yes)
		   (synt-funct by-obj)))))
	(alt passive-prep
	    (((process ((passive-prep given)))
	      (synt-roles ((by-obj ((prep {^3 process passive-prep}))))))
	     ((synt-roles ((by-obj ((prep ((lex "by")))))))))))
       ((agentless yes)
	(scoped no)
	(cset ((- {^ synt-roles by-obj})))
	(synt-roles ((by-obj none)))))))))


;; ============================================================
(provide "voice")
;; ============================================================
