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
  (circum   {^ circumstances})
  (pred-modif {^ predicate-modifiers})
  (process ((type {^ process-type})))
  (process-type {^ process process-type})
  (lex-roles {^ lexical-roles})

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
  ;;
  ;; Processing of semantic arguments:
  ;; Map from partic -> oblique -> synt-roles.
  ;; OR Map from lexical-roles -> oblique from lexicon.
  ;; First stage is transitivity, second is voice.
  ;; Government pattern of verbs is specified under subcat which is
  ;; unified with synt-roles (ala Mel'cuk and vaguely subcat of HPSG).
  (participants ((fset (agent affected created range
      		        processor phenomenon
			sayer addressee verbalization
			carrier attribute
			identified identifier
			located location time
			possessor possessed))))
  ;; Choice is either lexical-roles or partic or none but not both
  (alt type-of-roles
      (((lexical-roles none)
	(partic given))
       ((lexical-roles given)
	(partic none)
	(process-type lexical))
       ((lexical-roles none)
	(partic none)
	(alt (((process-type natural-phenom))
	      ;; JR 1/19/93 to avoid stub roles in mono-role imperative clauses
	      ((mood #(under imperative))
	       (process ((alt (((type #(under material))
				(effective #(under no)))
			       ((type #(under mental))
				(transitive #(under no))))))))
	      ((partic any)))))))

  (oblique ((fset (1 2 3 4 5 6 7 8))))
  (synt-roles
   ((fset (subject object iobject subj-comp obj-comp dative by-obj))))
  (innermost-role {^ oblique 1})
  (:! mood)
  (:! transitivity)
  (:! voice)
  ;; END OF TREATMENT OF INHERENT PARTICIPANTS

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

  (:! agentless)
  (:! displaced-constituent)

  (alt controlled (:wait {^ controlled})
    (((controlled none))
     ((:! controller-constituent)
      (controlled ((gap yes))))))

  ;; If dative-move is not bound yet, it should be yes
  ;; Do not use dative-move if object is a pronoun
  ;; * Mary gave the library it.
  ;; Mary gave it to the library.
  (process ((dative-move {^2 dative-move})))
  (alt dative-move-default (:bk-class dative-move)
    (((dative-move yes)
      (oblique ((3 given)
		(3 ((cat ((alt (common proper)))))))))
     ((process-type lexical)
      (dative-move yes))
     ((dative-move no))))

  ;; check cat of each syntactic role subcategorization system
  (:& subcat)

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
	 {^ synt-roles by-obj} {^ synt-roles dative} stop-kernel dots))


  ;; Case assignment
  ;; Subject of present-participle mood can be possessive mood.
  (alt synt-role-case
    (;; JR-added 1/28/93 to allow partic-less material clauses (e.g., Go!).
     ((synt-roles none))
     ((synt-roles given)
      (synt-roles ((alt (:wait {^ subject syntax case})
                     (((subject ((syntax ((case subjective))))))
		      ((subject ((syntax ((case given))))))))
		   (opt ((object  ((syntax ((case objective)))))))
		   (opt ((iobject ((syntax ((case objective)))))))))))))


;; ============================================================
;; SUBCATEGORIZATION
;; ============================================================

(def-conj subcat
  ;; Get lexical information on government pattern
  (process ((subcat {^2 oblique})))
  ;; special treatment of subject clauses (this is the right time
  ;; because the subject is now bound).
  (:! subject-mood)
  (:! subject-subcat)
  (:! object-subcat)
  (:& subj-comp-cat)
  (:& obj-comp-cat)
  (:! by-obj-cat)
  (synt-roles ((:! dative-cat)))
)


(def-alt subject-subcat
  ;; Syntactic category of subject is compatible with verb?
  ;; This depends on particular verb, information we will get from the
  ;; lexicon. So far, we check whether subject-clause and object-clause
  ;; are ok.
  ;; SUBJECT CAT = NP, CLAUSE.
  (((synt-roles ((subject none))))
   ((alt subject-subcat2 (:index (synt-roles subject cat))

;; JR-11-17-92: commented out the wait
      (:wait (({^ synt-roles subject cat} #(under lexical-cat))))

      (((synt-roles ((subject none))))
       ((synt-roles ((subject ((cat np))))))
       ((synt-roles ((subject ((cat #(under clause))))))
	(alt subject-clause	(:index (process subject-clause))
	  (:demo "For clausal subjects, what type of clause ~
                     must be used?")
	  (((process ((subject-clause infinitive)))
	    (synt-roles ((subject ((mood {^3 process subject-clause}))))))
	   ((process ((subject-clause #(under present-participle))))
	    (synt-roles ((subject ((mood {^3 process subject-clause}))))))
	   ((process ((subject-clause that)))
	    (synt-roles ((subject ((mood bound-nominal)))))))))
       ((synt-roles ((subject ((cat #(under list)))))))))
    (opt ((synt-roles ((subject ((synt-funct subject))))))))))


(def-alt object-subcat
  ;; OBJECT CAT = NP, CLAUSE, PP? (deal with?)
  (((synt-roles ((object none))))
   ((alt object-subcat1 (:index  (synt-roles object cat))

;; JR-11-17-92: commented out the wait
       (:wait (({^ synt-roles object cat} #(under lexical-cat))))

      (((synt-roles ((object none))))
       ((synt-roles ((object ((cat np))))))
       ((synt-roles ((object ((cat pp))))))
       ((synt-roles ((object ((cat #(under clause))))))
	(alt object-clause (:index (process object-clause))
	  (:demo "For clausal objects, what type of clause ~
                    must be used?")
	  (((process ((object-clause infinitive)))
	    (synt-roles ((object ((mood infinitive))))))
	   ((process ((object-clause #(under present-participle))))
	    (synt-roles ((object ((mood present-participle))))))
	   ((process ((object-clause that)))
	    (synt-roles ((object ((mood bound-nominal))))))
	   ((process ((object-clause wh)))
	    (synt-roles ((object ((mood wh)))))))))
       ((synt-roles ((object ((cat #(under list)))))))))
    (opt ((synt-roles ((object ((synt-funct object))))))))))


(def-conj subj-comp-cat
  ;; SUBJ-COMP CAT = NP, AP, PP, ADV,
  ;; + ADDRESS, DATE JR-11-21-92
  ;; + participles JR-1-30-93
  ;; + phrases JR-3-8-95
  (synt-roles ((subj-comp ((alt subj-comp-cat1 (:index cat)
				(:wait (({^ cat} #(under lexical-cat))))
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
			   (opt ((synt-funct subj-comp))))))))


(def-conj obj-comp-cat
  ;; OBJ-COMP CAT = NP, AP, PP, ADV,
  ;; + ADDRESS, DATE JR-11-21-92
  ;; + participles JR-1-30-93
  ;; + phrases JR-3-8-95
  (synt-roles ((obj-comp ((alt obj-comp-cat1 (:index cat)
			       (:wait (({^ cat} #(under lexical-cat))))
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
			  (opt ((synt-funct obj-comp))))))))


(def-alt by-obj-cat
  ;; BY-OBJ CAT = PP, set prep
  (((synt-roles ((by-obj none))))
   ((synt-roles ((alt by-obj-cat1 (:index (by-obj cat))
		   (((by-obj none))
		    ((by-obj given)
		     (by-obj ((cat pp)
			      (synt-funct by-obj)
			      ;; Prep set in alt agentless - no need here
			      ;; (prep ((lex "by")))
			      (np ((cat np)
				   (syntax ((case objective)))))))))))))))

(def-alt dative-cat
  ;; DATIVE CAT = PP, set prep
  (((dative none))
   ((dative given)
    (dative ((cat pp)
	     ({^ ^ process dative-prep} given)
	     (prep ((lex {^4 process dative-prep})))
	     (np ((cat np)
		  (syntax ((case objective))))))))
   ((dative ((cat pp)
	     (prep ((lex "to")))
	     (synt-funct dative)
	     (np ((cat np)
		  (syntax ((case objective))))))))))


;; ============================================================
(provide "clause")
;; ============================================================
