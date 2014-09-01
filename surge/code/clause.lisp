;;; -*- Mode:Lisp; Syntax:Common-Lisp; Package: -*-
;;; -----------------------------------------------------------------------
;;; File:         clause.lisp
;;; Description:  Grammatical systems for clause
;;; Author:       Michael Elhadad & Jacques Robin
;;; Created:      19 Dec 1991
;;; Modified:     07 Jul 92 - added past-participle mood treatment
;;;               14 Jul 92 - added Karen's constraint on pronoun/dative-move
;;;               18 Aug 92 - added adverb
;;;               17 Nov 92 - Jacques Robin commented out waits for lex-cat
;;;               27 Nov 92 - Jacques Robin added predicate-modifiers,
;;;                           and disjuncts, subdivided adjuncts into
;;;                           predicate-adjuncts and sentence-adjuncts,
;;;                           moved out the mood code to a mood.l file
;;;                           Also changed patterns.
;;;                5 Jul 95 - SURGE 2.2 VERSION
;;;                         - added clause-level/embedded
;;;                           which is different from scoped
;;;                           (eg, "Who is coming" is scoped but not embedded);
;;;                           embedded is necessary to determine if subject
;;;                           inversion (eg,  "What happens" vs.
;;;                           "What do you think happens") is required
;;;                         - added alt focus
;;;                6 Nov 95 - removed hard-wired "by" for passive-prep in
;;;                           alt by-obj-cat
;;;               12 May 96 - changed object-subcat to allow wh-clauses
;;;                           as in "Ask him what it is".
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

(def-conj clause-pattern
  (pattern (dots stop-header dots
                 front-adverbial-1 front-adverbial-2
                 dots start dots stop-kernel
                 end-adverbial-1 end-adverbial-2 end-adverbial-3
                 end-adverbial-4 end-adverbial-5))
  (cset ((- stop-header start stop-kernel)))
  (pattern (dots stop-kernel dots (* {^ final-adverbial})))
  ;;  (cset ((- final-adverbial)))
  ;;  (pattern (dots stop-header (* {^ next-to-header}) dots))
  ;;  (cset ((- next-to-header)))
)

;;; BRANCH FOR SIMPLE-CLAUSE
(def-conj simple-clause
  (cat simple-clause)
  (generic-cat clause)
  (complex none)

  ;; Some shorthands that are used for convenience...
  (verb {^ process})
  (proc {^ process})
  (partic {^ participants})
  (lex-roles {^ lexical-roles})
  (circum   {^ circumstances})
  (pred-modif {^ predicate-modifiers})
  (process ((type {^ process-type})
            (dative-move {^2 dative-move})
            (subcat {^2 oblique})))
  (process-type {^ process process-type})

  ;; General structure of a clause
  ;; lexical-roles      : semantic roles specific to a lexical item
  ;; participants       : semantic arguments of the process type
  ;; oblique            : obliqueness hierarchy of semantic arguments
  ;; synt-roles:        : syntactic arguments
  ;; predicate-modifiers: optional semantic roles refining the specification
  ;;                      of the clause predicate
  ;; circumstances      : optional semantic roles specifying the setting of
  ;;                      the clause as a whole
  ;; disjuncts          : peripheral movable optional syntactic constituents
  ;; sentence-adjuncts  : central movable optional syntactic constituents
  ;; predicate-adjuncts : non-movable optional syntactic constituents
  ;; relaters           : if, then, firstly
  ;; headers            : news-genre - time - location - ...
  ;;
  ;; Processing of semantic arguments:
  ;; Map from partic -> oblique -> synt-roles.
  ;; OR Map from lexical-roles -> oblique from lexicon.
  ;; First stage is transitivity, second is voice.
  ;; Government pattern of verbs is specified under subcat which is
  ;; unified with synt-roles (ala Mel'cuk and vaguely subcat of HPSG).

  ;; Specify fset for all components
  (participants ((fset (agent affected created range
                        processor phenomenon
			sayer addressee verbalization
			carrier attribute
			identified identifier
			located location time
			possessor possessed))))

  (oblique ((fset (1 2 3 4 5 6 7 8))))

  (synt-roles
   ((fset (subject object iobject subj-comp obj-comp dative by-obj))))
  (innermost-role {^ oblique 1})

  (pred-modif
   ((fset (score manner means instrument comparison matter direction
                 distance location path origin destination duration time))))

  (circum
   ((fset (location distance origin time duration frequency
                    co-event reason result purpose behalf
                    condition concessive-condition concession contrast
                    exception inclusion substitution addition accompaniment
                    opposition manner means comparison matter standard
                    perspective to-loc from-loc at-loc on-loc in-loc))))

  (relaters ((fset (time cond))))
  (headers ((fset (1 2))))

  ;; Choice is either lexical-roles or partic or none but not both
  (alt type-of-roles
      (((lexical-roles none)
	(partic given))
       ((lexical-roles given)
	(partic none)
	(process-type lexical))
       ((lexical-roles none)
	(partic none)
        (oblique none)
	(alt (((process-type natural-phenom))
	      ;; JR 1/19/93 to avoid stub roles in mono-role imperative clauses
	      ((mood #(under imperative))
	       (process ((alt (((type #(under material))
				(effective #(under no)))
			       ((type #(under mental))
				(transitive #(under no))))))))
	      ((partic any)))))))

  (:! transitivity)
  (:! voice)
  (:& voice-synt-roles)
  (:! mood)
  (:! subject-mood)
  ;; END OF TREATMENT OF INHERENT PARTICIPANTS

  ;; Depends on adverbials
  (:! displaced-constituent)

  ;; If dative-move is not bound yet, it should be yes
  ;; Do not use dative-move if object is a pronoun
  ;; * Mary gave the library it.
  ;; Mary gave it to the library.
  (alt dative-move-default (:bk-class dative-move)
    (((dative-move yes)
      (oblique ((3 given)
		(3 ((cat ((alt (common proper)))))))))
     ;; @TODO: Revise this point: dative-move with lex-roles
     ((process-type lexical)
      (dative-move yes))
     ((dative-move no))))

  ;; check cat of each syntactic role subcategorization system
  (:& subcat)

  (alt controlled (:wait {^ controlled})
    (((controlled none))
     ((:! controller-constituent)
      (controlled ((gap yes))))))

  ;; Process optional participants
  (:! old-circum)
  (:! predicate-modifiers)
  (:! circumstantials)
  (:& relaters)
  (:& clause-pattern)
  (alt adverb (:index adverb)
    (((adverb none))
     ((adverb given)
      (adverb ((cat adv))))))

  ;; DISPLACED CONSTITUENT PROCESSING
  ;; for interrogative and relative mood, the constituent under scope
  ;; is displaced.  We handle it in this alternation.
  (alt scoped-determination (:index scope)
    (((scope none))
     ((scope given) (scope ((clause-level ((scoped yes))))))))

  ;; Every filler of a participant or an adjunct or a qualifier  will get
  ;; a synt-funct set to something else (subject, object, subj-comp, obj-comp,
  ;; qualifier) so it will not unify with main-clause here.
  (alt embedded-determination (:wait synt-funct)
    (((synt-funct main-clause) (embedded no))
     ((control (not (equal #@{^ synt-funct} 'main-clause)))
      (embedded yes))))

  ;; START OF GENERAL THINGS: ORDERING PLUS AGREEMENT
  ;; General things: arrange syntactic roles together and do the agreements.
  ;; The patterns are here of course.

  ;; Focus is a pointer to a semantic constituent on which focus is placed.
  ;; In general, we want to make the syntactic constituent which realizes the
  ;; focus the subject of the clause.  The focus-given alt checks which is
  ;; the syntactic constituent realizing focus.  realization can be:
  ;; HEAD: this means that the pointer is pointing to a semantic constituent
  ;; which is a trace in a relative clause, and is therefore realized
  ;; by the head of the NP.
  ;; In this case, we want the trace to be the subject.
  ;; A PATH: to the syntactic constituent realizing the focus.
  ;; Note: this only works for participants who can become subjects.
  ;; other fronting devices (it-clefts dislocation etc) are not implemented.
  (alt focus
      (((focus none))
       ((focus given)
	(alt focus-given
	    (((focus ((synt-funct given)))
	      (focus {^ synt-roles subject}))
	     ((focus ((realization head)))
	      (focus {^ synt-roles subject}))
	     ((focus ((synt-funct none)
		      (realization {^2 synt-roles subject})))))))))

  ;; Number and person agreement (verb is process)
  (verb ((cat verb-group)
	 (modality {^ ^ modality})
	 (adverb {^ ^ adverb})
	 (epistemic-modality {^ ^ epistemic-modality})
	 (deontic-modality {^ ^ deontic-modality})
	 (tense  {^ ^ tense})
	 (polarity {^ ^ polarity})
	 (insistence {^ ^ insistence})
	 (person {^ ^ synt-roles subject person})
	 (number {^ ^ synt-roles subject number})))

  ;; particle is for verbs like "take off"
  ;; This is for MOVABLE particles only.
  ;; non-movable particles should be treated as in the example in ir8.l
  (cset ((- particle)))
  (alt particle (:demo "Does the verb have a particle?")
    (:wait {^ verb particle})
    (((verb ((particle none))))
     ((verb ((particle given)))
      (particle ((cat adv)
		 (lex {^ ^ verb particle}))))))

  ;; Default constituent ordering
  (pattern
   (dots start {^ synt-roles subject} dots
         process dots
	 {^ synt-roles iobject} dots
	 {^ synt-roles object} dots
	 particle dots
	 {^ synt-roles subj-comp} {^ synt-roles obj-comp} dots
	 {^ synt-roles by-obj} {^ synt-roles dative} stop-kernel dots)))



;; ============================================================
;; SUBCATEGORIZATION
;; ============================================================

(def-conj subcat
  (:! subject-subcat)
  (:! object-subcat)
  (synt-roles ((subj-comp ((:! subj-comp-cat)))
               (obj-comp ((:! obj-comp-cat)))))
  (synt-roles ((:! dative-cat)
               (:! by-obj-cat)))
)


(def-alt subject-subcat
    (:index (synt-roles subject cat))
  (:wait (({^ synt-roles subject cat} given)))
  ;; Syntactic category of subject is compatible with verb?
  ;; This depends on particular verb, information we will get from the
  ;; lexicon. So far, we check whether subject-clause and object-clause
  ;; are ok.
  ;; SUBJECT CAT = NP, CLAUSE.
  (((synt-roles ((subject none))))
   ((synt-roles
     ((subject ((cat np)
                (syntax ((case ((alt (given subjective)))))))))))
   ((synt-roles ((subject ((cat #(under clause))))))
    (alt subject-clause (:index (process subject-clause))
         (:demo "For clausal subjects, what type of clause must be used?")
         (((process ((subject-clause infinitive)))
           (synt-roles ((subject ((mood {^3 process subject-clause}))))))
          ((process ((subject-clause #(under present-participle))))
           (synt-roles ((subject ((mood {^3 process subject-clause}))))))
          ((process ((subject-clause that)))
           (synt-roles ((subject ((mood bound-nominal)))))))))
   ((synt-roles ((subject ((cat #(under list)))))))))


(def-alt object-subcat
    ;; OBJECT CAT = NP, CLAUSE, PP? (deal with?)
    (:index  (synt-roles object cat))
  (:wait (({^ synt-roles object cat} given)))
  (((synt-roles ((object none))))
   ((synt-roles ((object ((cat np)
                          (syntax ((case objective))))))))
   ((synt-roles ((object ((cat pp))))))
   ((synt-roles ((object ((cat #(under clause))))))
    (alt object-clause (:index (process object-clause))
         (:demo "For clausal objects, what type of clause must be used?")
         (((process ((object-clause infinitive)))
           (synt-roles ((object ((mood infinitive))))))
          ((process ((object-clause #(under present-participle))))
           (synt-roles ((object ((mood present-participle))))))
          ((process ((object-clause that)))
           (synt-roles ((object ((mood bound-nominal))))))
          ((process ((object-clause wh)))
           (synt-roles ((object ((mood wh)))))))))
   ((synt-roles ((object ((cat #(under list)))))))))


(def-alt subj-comp-cat
    ;; SUBJ-COMP CAT = NP, AP, PP, ADV,
    ;; + ADDRESS, DATE JR-11-21-92
    ;; + participles JR-1-30-93
    ;; + phrases JR-3-8-95
    (:index cat)
  (:wait cat)
  (none
   ((cat ap))
   ((cat #(under np)))
   ((cat #(under pp)))
   ((cat #(under list)))
   ;; JR-1-30-93: added
   ((cat #(under verb))
    (ending ((alt (#(under past-participle)
                    #(under present-participle))))))
   ;; JR-11-21-92: added
   ((cat #(under address)))
   ((cat #(under date)))
   ((cat #(under phrase)))
   ((cat #(under adv)))))


(def-alt obj-comp-cat
    ;; OBJ-COMP CAT = NP, AP, PP, ADV,
    ;; + ADDRESS, DATE JR-11-21-92
    ;; + participles JR-1-30-93
    ;; + phrases JR-3-8-95
    (:index cat)
  (:wait cat)
  (none
   ((cat ap))
   ((cat #(under np)))
   ((cat #(under pp)))
   ((cat #(under list)))
   ;; JR-1-30-93: added
   ((cat #(under verb))
    (ending ((alt (#(under past-participle)
                    #(under present-participle))))))
   ;; JR-11-21-92:
   ((cat #(under address)))
   ((cat #(under date)))
   ((cat #(under phrase)))
   ((cat #(under adv)))))


(def-alt by-obj-cat
    ;; BY-OBJ CAT = PP, set prep
    (:wait (({^ by-obj cat} #(under pp))))
  (((by-obj none))
   ((by-obj given)
    (by-obj ((cat pp)
             (synt-funct by-obj)
             ;; Prep set in alt agentless
             (np ((cat np))))))))


(def-alt dative-cat
  ;; DATIVE CAT = PP, set prep
    (:index (dative cat))
  (:wait (({^ dative cat} #(under pp))))
  (((dative none))
   ((dative given)
    (dative ((cat pp)
	     ({^ ^ process dative-prep} given)
	     (prep ((lex {^4 process dative-prep})))
	     (np ((cat np))))))
   ((dative ((cat pp)
	     (prep ((lex "to")))
	     (synt-funct dative)
	     (np ((cat np))))))))


;; ============================================================
(provide "clause")
;; ============================================================
