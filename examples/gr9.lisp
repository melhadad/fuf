;;; -*- Mode:Lisp; Syntax:Common-Lisp; -*-
;;; ------------------------------------------------------------
;;; File        : GR9.L
;;; Description : 
;;; Author      : Michael Elhadad
;;; Created     : 17 Jun 91 
;;; Modified    : 
;;; Language    : FUF5
;;; ------------------------------------------------------------

(in-package "FUG5")

;; ------------------------------------------------------------------
;; External functions 
;; ------------------------------------------------------------------

(defun aspect-choice-1 (path)
  (let ((alt-name (car (path-last path))))
    `((alt ,alt-name
       (((aspect event)
	 (tpattern ((:rt0 :equals :et))))
	((aspect ((alt (stative process))))
	 (tpattern ((:rt0 :during :et))))))))
  )


;; NOTE: all non-implemented features, or things to do are marked in
;; comments with a ***** sign.

(defun gsetup9 ()
  ;; Reset type declaration in effect before new decl.
  (clear-bk-class)
  (reset-typed-features)

  ;; TYPE DECLARATIONS
  ;; =================

  ;; SIMPLE - COMPLEX RELATIONS
  ;; Treat complex constructions to be the same cat as their constituents
  ;; For example, a conjunction of NPs is an NP.
  ;; To handle complex constructions, the feature complex is added to a
  ;; constituent. 
  ;; (complex none) -> simple constituent
  ;; (complex conjunction) 
  ;; (complex apposition)
  ;; (complex list)
  ;; [Distinctions are explained below]
  ;; Whenever the grammar puts restrictions on the filler of a syntactic
  ;; function (like subject must be an NP), we mean either simple or complex.
  ;; But grammatical description of complex is different.  So use types to
  ;; allow for a simple reference to either simple or complex:
  (define-feature-type clause (simple-clause complex-clause))
  (define-feature-type verb-group (simple-verb-group complex-verb-group))
  (define-feature-type np (simple-np complex-np))
  (define-feature-type pp (simple-pp complex-pp))
  (define-feature-type ap (simple-ap complex-ap))
  (define-feature-type noun (simple-noun complex-noun))
  (define-feature-type adj  (simple-adj complex-adj))

  ;; A single complex construct handles all the similarities between complex
  ;; constructs and handles the recursion.
  (define-feature-type complex (clause verb-group np ap pp noun adj))

  ;; The NP hierarchy
  (define-feature-type simple-np (pronp common proper))
  (define-feature-type complex-np (pronp common proper))
  (define-feature-type pronp (personal-pronoun question-pronoun
					       relative-pronoun 
					       quantified-pronoun demonstrative-pronoun))

  (define-feature-type det (possessive-det demonstrative-det regular-det))


  ;; TRANSITIVITY SYSTEM:
  ;; Process-type       | Nuclear Semantic roles
  ;; -------------------+-------------------------------------------------
  ;; - simple           |
  ;; --- event          |
  ;; ----- material     | agent affected created range
  ;; -----              | [features: 
  ;;                    |  effective: yes, no
  ;;                    |  agentive: yes, no
  ;;                    |  event-as: process, participant
  ;;                    |  effect-type: creative, dispositive]
  ;; ----- mental       | processor, phenomenon
  ;; ------- perceptual |
  ;; ------- cognitive  |
  ;; ------- emotional  |
  ;; ----- verbal       | sayer, addressee, verbalization
  ;; --- relation       | carrier, attribute
  ;; ---                | [features: 
  ;;                    |  change-mode: maintain, change, neutral
  ;;                    |  mode: attributive, equative
  ;;                    |  relation-as: process, participant
  ;;                    | when mode attributive    | carrier, attribute
  ;;                    | when mode equative       | identified, identifier
  ;; ----- ascriptive   |
  ;; ----- locative     |  located, location
  ;; ------- spatial    |  
  ;; ------- temporal   |  
  ;; ------- accompaniment 
  ;; ------- existential|
  ;; ----- possessive   |  possessor, possessed
  ;;                    |
  ;; - composite        | [features:
  ;;                    |  process: 
  ;;                    |   event-struct
  ;;                    |   relation-struct]
  ;;                    | + the mappings are expressed in input.

  ;; Notes on treatment of relational processes:
  ;; relation-mode determines whether relation is symmetrical or not (can it
  ;; be passivized).
  ;; For relation processes, relation can be expressed in the verb or
  ;; in the participant (in which case it is a PP with the prep expressing
  ;; the relation).
  ;; relation-mode   | attributive, equative
  ;; circumstance-as | process, participant

  ;; Examples of composite processes [as per Fawcett-87]:
  ;; Complete listing in code before transitivity system of composite processes.
  ;; I went home           = Carrier+Agent Proc Loc
  ;; I sent Ivy to Peru    = Ag Pro Carrier+Affected Loc
  ;; The package went to Pery = Affected+Carrier Proc Location
  ;; Ivy received the keys = Possessor+Affected Proc Possessed
  ;; Ivy took the key      = Agent+Possessor Proc Possessed [change]
  ;; Ivy kept the key      = Agent+Possessor Proc Possessed [maintain]
  ;; I gave Ivy the key    = Agent Proc Affected+Possessor Possessed
  ;; I cooked Lopez diner  = Agent Proc Possessor Created+Possessed
  ;; The kettle boiled dry = Affected+Carrier Proc Attribute
  ;; They elected Ivy the boss = Agent Proc Affected+Identified Identifier
  ;; He painted the shed green = Agent Proc Affected+Carrier Attribute
  ;; Ike drove Ivy to Peru = Ag Proc Aff+Ca Loc


  (define-feature-type process-class (simple-process composite))
  (define-feature-type simple-process (event relation))
  (define-feature-type event (material mental verbal))
  (define-feature-type mental   (perception thinking))
  (define-feature-type relation (ascriptive possessive locative))
  (define-feature-type locative (spatial temporal accompaniment
					 existential natural-phenom))

  ;; Composite functions: only the possible combinations are defined here
  (define-feature-type agent (agent-carrier third-party-agent agent-identified))
  (define-feature-type affected (affected-carrier affected-identified))
  (define-feature-type created (created-carrier))
  (define-feature-type carrier (located possessor))
  (define-feature-type located (agent-carrier affected-carrier created-carrier))
  (define-feature-type possessor 
      (agent-carrier affected-carrier created-carrier))
  (define-feature-type identified (agent-identified affected-identified))


  ;; OPTIONAL CIRCUMSTANCIAL ROLES:
  ;; The default preposition can be overriden by specifying the corresponding
  ;; feature in the verb (cf. info needed in verb) or adding a feature prep
  ;; in the role.  The feature in the role has priority if it is given.
  ;; -------------------------------------------------------------------
  ;; Role name        | Default preposition  | Default relative pronoun 
  ;; -----------------+----------------------+--------------------------
  ;; to-loc           | to                   | where
  ;; from-loc         | from                 | where
  ;; at-loc           | at                   | where
  ;; in-loc           | in                   | where
  ;; on-loc           | on                   | where
  ;; instrument       | with                 | with (embedded)
  ;; purpose          | in order to + clause | 
  ;;                  | for + np             | for (embedded)
  ;; at-time          | at                   | when
  ;; accompaniment    | with                 | with (embedded)


  ;; time-relater (must be an adverb, appears first in clause)
  ;; cond-relater (must be an adverb, appears just after time - if or then)


  ;; INFORMATION NEEDED IN VERB:
  ;;      Feature     |               Possible values
  ;; -----------------+------------------------------------------------
  ;; process-class    :    any process-type
  ;; process-structure:    like in process described in transitivity above
  ;; dative-prep      : 	 "to", "for"            
  ;; to-loc-prep      :    "to", ... any preposition
  ;; from-loc-prep    :    "from",... 
  ;; on-loc-prep      :    "on",...
  ;; instrument-prep  :    "with", "using" ...                         
  ;; purpose-prep     :    "for"
  ;; accompaniment-prep:   "with",...
  ;; subject-clause   :    infinitive, present-participle, that, none
  ;; object-clause    :    infinitive, present-participle, that, none
  ;; particle         :    "off" ... (for "to take off" when particle is
  ;;                  :    mobile, when it is not mobile, lex should be 
  ;;                  :    "give up" (in one string).
  ;; preposition      :    cf Quirk 12 (not implemented yet)


  ;; MOOD SYSTEM:
  ;; ------------------------------------------------------------------
  ;; MOOD: finite/non-finite
  ;; FINITE: declarative/interrogative/bound/relative
  ;; NON-FINITE: imperative/present-participle/infinitive
  ;; INTERROGATIVE: yes-no/wh
  ;; RELATIVE: simple-relative/embedded-relative/be-deleted-relative/
  ;;           wh-nominal-relative/wh-ever-nominal-relative

  ;; Not all implemented (eg under relative only simple and embedded)
  (define-feature-type mood (finite non-finite))
  (define-feature-type finite (declarative interrogative bound relative))
  (define-feature-type non-finite (imperative present-participle infinitive
					      past-participle))
  (define-feature-type interrogative (yes-no wh))
  (define-feature-type relative
      (simple-relative embedded-relative be-deleted-relative 
		       wh-nominal-relative wh-ever-nominal-relative))



  ;; MODALITY
  ;; ------------------------------------------------------------------
  ;; EPISTEMIC-MODALITY: fact/inference/possible/"should"... (the modal)
  ;; DEONTIC-MODALITY: duty/authorization/(the modal - string)
  (define-feature-type modality (epistemic-modality deontic-modality))
  (define-feature-type epistemic-modality (fact inference possible))
  (define-feature-type deontic-modality (duty authorization))

  ;; SCOPE: role (value is the name of the role under scope in the relative 
  ;;        clause)

  ;; SEMANTIC INFORMATION NEEDED IN NPs:
  ;; ------------------------------------------------------------------
  ;; SYNTAX:
  ;; animate:           	 yes/no                                             
  ;; number:            	 plural/singular                                    
  ;; definite:          	 yes/no                                             
  ;; person:            	 first/second/third                                 
  ;; gender:            	 masculine/feminine/neuter                          
  ;; case:              	 subjective/objective/possessive/reflexive 
  ;; distance:          	 far/near                                           
  ;; countable:         	 yes/no                                             
  ;; collective:        	 yes/no                                             

  ;; CONSTITUENTS OF NPs   relevant features
  ;; ------------------------------------------------------------------
  ;; determiner:           definite/distance/demonstrative/possessive
  ;; describer:            
  ;; head:                 (syntax) lex/animate/person/number/gender/case
  ;; classifier:           
  ;; qualifier:            restrictive [yes/no]
  ;; possessor:            an NP
  ;; (possessive determiners are described as NPs under possessor)

  ;; CONSTITUENTS OF APs   relevant features
  ;; ------------------------------------------------------------------
  ;; describer:            an adverb
  ;; head:                 an adj
  ;; qualifier:            restrictive [yes/no] in general a PP

  ;; NOTE ON CONJUNCTION AND APPOSITION:
  ;; Complex constructs used to be of (cat list) with (cat X) under common.
  ;; Now a complex is a (cat X) with a feature (complex T) where T can be:
  ;; conjunction:  the complex is a conjunction, specify the conjunction
  ;; apposition: apposition specify whether restrictive or not
  ;; list: flat list with no clear semantic explaining the juxtaposition of
  ;; the elements into a complex.

  ;; Define synonyms for tense names in such a way that tense name can be
  ;; used as an index in the alt
  ;; Cf Halliday-85 IFG pp180-181 for this list of 36 tenses of English.
  (define-feature-type present (simple-present tense-2))
  (define-feature-type past (simple-past tense-1))
  (define-feature-type future (simple-future tense-3))
  (define-feature-type past-perfect (past-in-past tense-4))
  (define-feature-type present-perfect (past-in-present tense-5))
  (define-feature-type future-perfect (past-in-future tense-6))
  (define-feature-type past-progressive (present-in-past tense-7))
  (define-feature-type present-progressive (present-in-present tense-8))
  (define-feature-type future-progressive (present-in-future tense-9))
  (define-feature-type tense-10 (future-in-past past-future))
  (define-feature-type tense-11 (future-in-present))
  (define-feature-type tense-12 (future-in-future))
  (define-feature-type tense-13 (past-in-future-in-past past-future-perfect))
  (define-feature-type tense-14 (past-in-future-in-present))
  (define-feature-type tense-15 (past-in-future-in-future))
  (define-feature-type tense-16 (present-in-past-in-past
				 past-perfect-progressive))
  (define-feature-type tense-17 (present-in-past-in-present
				 present-perfect-progressive))
  (define-feature-type tense-18 (present-in-past-in-future 
				 future-perfect-progressive))
  (define-feature-type tense-19 (present-in-future-in-past
				 past-future-progressive))
  (define-feature-type tense-20 (present-in-future-in-present
				 present-future-progressive))
  (define-feature-type tense-21 (present-in-future-in-future
				 future-future-progressive))
  (define-feature-type tense-22 (future-in-past-in-past))
  (define-feature-type tense-23 (future-in-past-in-present))
  (define-feature-type tense-24 (future-in-past-in-future))
  (define-feature-type tense-25 (past-in-future-in-past-in-past))
  (define-feature-type tense-26 (past-in-future-in-past-in-present))
  (define-feature-type tense-27 (past-in-future-in-past-in-future))
  (define-feature-type tense-28 (present-in-past-in-future-in-past
				 past-future-perfect-progressive))
  (define-feature-type tense-29 (present-in-past-in-future-in-present
				 present-future-perfect-progressive))
  (define-feature-type tense-30 (present-in-past-in-future-in-future))
  (define-feature-type tense-31 (present-in-future-in-past-in-past))
  (define-feature-type tense-32 (present-in-future-in-past-in-present))
  (define-feature-type tense-33 (present-in-future-in-past-in-future))
  (define-feature-type tense-34 (present-in-past-in-future-in-past-in-past))
  (define-feature-type tense-35 (present-in-past-in-future-in-past-in-present))
  (define-feature-type tense-36 (present-in-past-in-future-in-past-in-future))

  ;; ------------------------------------------------------------------
  ;; BK-CLASS DEFINITIONS
  ;; ------------------------------------------------------------------
  (clear-bk-class)
  (define-bk-class 'transitive-class 'transitivity)
  (define-bk-class 'voice-class 'voice-class)
  
  ;; These are completely determined by the morphology features
  ;; they are not unified with the grammar
  (register-categories-not-unified 'cat
				   '(verb punctuation prep conj relpro adv modal ordinal cardinal))

  (setq *u-grammar*
	'((alt


	   ;; first only simple constituents - complex follows
	   ;;==============================================================
	   ;; 01 CAT CLAUSE : clause --------------------------------------
	   ;;==============================================================
	   (((cat simple-clause)
	     (generic-cat clause)
	     (complex none)

	     ;; Some shorthands that are used for convenience...
	     (verb {^ process})
	     (proc {^ process})
	     (partic {^ participants})
	     (circum   {^ circumstances})
	     (process ((type {^2 process-type})
		       (mode {^2 relation-mode})))


	     ;; MOOD SYSTEM
	     ;; For relative and interrogative, the mapping scope to role is
	     ;; delayed after the transitivity system (same time as the voice
	     ;; system does the mapping semantic/syntactic roles).
	     ;; In this first system, all pre-selections that can be derived from
	     ;; the mood are set.
	     (alt mood (:index mood) 
		  (:demo "Deciding between mood finite and non-finite")

		  (((mood finite)
		    (alt finite (:index mood)
			 (:demo 
			  "Is the clause simple declarative, interrogative, relative ~
                or subordinate?")
			 (((mood declarative)
			   ;; start is a dummy constituent used only in the patterns.
			   (pattern (dots start dots)))

			  ((mood interrogative)
			   ;; for both yes-no and wh questions, front the tensed part
			   ;; of the verb group and the not particle.
			   ;; copy everything from tensed-feature except the gap
			   ;; (therefore cannot just conflate them).
			   ;; Note: these are all the features known to the morphology.
			   (scope ((gap yes)))
			   (verb ((interrogative {^ ^ mood})))
			   (cset ((- fronted-aux)))
			   (fronted-aux 
			    ((person {^ ^ verb tensed-feature person})
			     (number {^ ^ verb tensed-feature number})
			     (ending {^ ^ verb tensed-feature ending})
			     (tense  {^ ^ verb tensed-feature tense })
			     ;; don't give it a cat until
			     ;; verb-group is processed...
			     (lex    {^ ^ verb tensed-feature lex}))))

			  ((mood bound)
			   (pattern (binder start dots))
			   (binder ((cat conj)))
			   (opt binder
				((binder ((lex ((alt ("that" "whether" "if"))))))))
			   (binder ((lex given))))

			  ;; relative -- mapping scope/role is done in voice system.
			  ;; Just indicate here that scope has a gap.
			  ((mood relative)
			   (scope ((gap yes)))))))

		   ((mood non-finite)
		    (alt non-finite (:index mood)
			 (:demo "Is the clause imperative, present-participle ~
                          or infinitive?")
			 (((mood imperative)
			   (modality none)
			   (epistemic-modality none)
			   (deontic-modality none)
			   (process ((ending root)
				     (voice active)))
			   (innermost-role ((alt (none ((gap yes)))))))

			  ((mood present-participle)
			   (process ((ending present-participle)))
			   (modality none)
			   (epistemic-modality none)
			   (deontic-modality none))

			  ((mood infinitive)
			   (modality none)
			   (epistemic-modality none)
			   (deontic-modality none)
			   (process ((ending infinitive)))))))))

      
	     ;; DISPLACED CONSTITUENT PROCESSING 
	     ;; for interrogative and relative mood, the constituent under scope
	     ;; is displaced.  We handle it in this alternation.
	     ;; Do this BEFORE the transitivity system so that displaced constituents get filled
	     ;; in their right thematic structure and "given" tests will work.
	     ;; For example ((scope ((role phenomenon)))) will map phenomenon to a non-empty value.
	     ;; 
	     ;; There are 2 cases:
	     ;; - Either scope names an existing constituent which happens to be "displaced" 
	     ;;   e.g.,  ((partic ((agent ((cat proper) (lex "John"))))) (mood wh) (scope ((role agent))) ...)
	     ;;   This case is caused in cases a base FD is constructed programmatically and "transformed" by adding scope/mood.
	     ;; - Or the scope is specified but the constituent to which it points is not provided in input.
	     ;;   In this case, scope can have additional semantic features that are propagated as needed:
	     ;;   e.g.,   ((scope ((role carrier) (animate yes))) ...)
	     ;;
	     (alt scope-place (:index mood)
		  (((mood yes-no)
		    ;; No displaced component except inversion of verb/subject
		    (cset ((- fronted-aux)))
		    (pattern (dots fronted-aux fronted-not start dots)))

		   ((mood wh)
		    ;; scope is like in relative clauses.  Contains the name
		    ;; of a top-level role on which the question is asked
		    ;; as in: (scope ((role carrier)))
		    ;; NOTE: no need to put participants in there, just the name.
		    ;; Two cases handled: corresponding comp is an NP or a PP.
		    ;; ***** Should check for other cats (adverb, ap).
		    ;; ***** Should check features of verb to decide whether
		    ;; to use a pp or an np as question element
		    ;; ***** Should do questions like "to which house did you
		    ;; go?" (scope is sub-constituent) PRETTY HARD.
		    ;; 
		    ;; @@@ created
		    ;; @@@ range
		    ;; @@@ sayer
		    ;; @@@ addressee
		    ;; @@@ verbalization
		    ;; 
		    ;; @@@ How does it go with composite?
		    ;;
		    (scope ((gap yes)))
		    (alt question-elt-cat 
			 (
			  ;; Simple question: <wh> vs. embedded question <prep> <wh>
			  ((question ((cat question-pronoun)
				      (restrictive {^2 scope restrictive})
				      (syntax {^2 scope syntax})
				      (semantics ((index {^3 scope semantics index})))))

			   ;; Dispatch on role - fill out scope
			   (alt question-elt (:index (scope role))
				(demo-message "Choosing a question element.")
				(((scope ((role at-loc))) 
				  (scope {^ circum at-loc-comp np})
				  (circum ((at-loc-comp ((gap yes)))))
				  (alt circum-at-loc-scope 
				       (((circum ((at-loc given))))
					((circum ((at-loc ((cat question-pronoun))))))))
				  (question ((cat question-pronoun) (lex "where"))))

				 ;; @todo: Deal with existential
				 ((scope ((role location)))
				  (proc ((type spatial)))
				  (alt partic-location-scope
				       (((partic ((location given))))
					((partic ((location ((cat question-pronoun))))))))
				  (scope {^ partic location})
				  (alt scope-role-location-mode 
				       (((circumstance-as participant)
					 (relation-mode attributive)
					 (question ((cat question-pronoun) (lex "where"))))
					((relation-mode equative)))))

				 ((scope ((role to-loc))) 
				  (scope {^ circum to-loc-comp np})
				  (alt circum-to-loc-scope 
				       (((circum ((to-loc given))))
					((circum ((to-loc ((cat question-pronoun))))))))
				  (circum ((to-loc-comp ((gap yes)))))
				  (question ((cat question-pronoun) (lex "where"))))

				 ((scope ((role at-time)))
				  (question ((cat question-pronoun) (lex "when")))
				  (circum ((at-time-comp ((gap yes)))))
				  (alt circum-at-time-scope
				       (((circum ((at-time ((cat #(under adv))))))
					 (scope {^ circum at-time}))
					((circum ((at-time ((cat #(under clause))))))
					 (scope {^ circum at-time}))
					((circum ((at-time ((cat #(under np))))))
					 (scope {^ circum at-time}))
					((circum ((at-time ((cat #(under pp))))))
					 (scope {^ circum at-time np}))
					((circum ((at-time ((cat relative-pronoun)
							    (time-type ((alt (given "at"))))))))
					 (scope {^ circum at-time})))))

				 ((scope ((role time)))
				  (alt partic-time-scope
				       (((partic ((time given))))
					((partic ((time ((cat question-pronoun))))))))
				  (alt partic-time-scope-mode
				       (((circumstance-as participant)
					 (relation-mode attributive)
					 (question ((cat question-pronoun) (lex "when"))))
					((relation-mode equative)
					 (question ((restrictive {^2 scope restrictive}))))))
				  (scope {^ partic time}))

				 ((scope ((role reason)))
				  (question ((cat question-pronoun) (lex "why")))
				  (alt circum-reason-scope
				       (((circum ((reason ((cat #(under clause))))))
					 (scope {^ circum reason-comp}))
					((circum ((reason ((cat #(under pp))))))
					 (scope {^ circum reason-comp}))
					((circum ((reason ((cat question-pronoun)))
						  (reason-comp ((gap yes)))))
					 (scope {^ circum reason-comp np})))))

				 ((scope ((role manner)))
				  (question ((cat question-pronoun) (lex "how")))
				  (alt circum-manner-scope
				       (((circum ((manner ((cat #(under clause))))))
					 (scope {^ circum manner-comp}))
					((circum ((manner ((cat #(under adv))))))
					 (scope {^ circum manner-comp}))
					((circum ((manner ((cat #(under pp))))))
					 (scope {^ circum manner-comp}))
					((circum ((manner ((cat question-pronoun)))
						  (manner-comp ((gap yes)))))
					 (scope {^ circum manner-comp np})))))

				 ((scope ((role agent)))
				  (alt partic-agent-scope
				       (((partic ((agent given))))
					((partic ((agent ((cat question-pronoun))))))))
				  (scope {^ partic agent}))

				 ((scope ((role affected)))
				  (alt partic-affected-scope
				       (((partic ((affected given))))
					((partic ((affected ((cat question-pronoun))))))))
				  (scope {^ partic affected}))

				 ((scope ((role processor)))
				  (alt partic-processor-scope
				       (((partic ((processor given))))
					((partic ((processor ((cat question-pronoun))))))))
				  (scope {^ partic processor}))

				 ((scope ((role phenomenon)))
				  (alt partic-phenomenon-scope
				       (((partic ((phenomenon given))))
					((partic ((phenomenon ((cat question-pronoun))))))))
				  (scope {^ partic phenomenon}))

				 ;; Possessor can be an embedded question in the case of dative-move no
				 ;; All the types of carrier
				 ((scope ((role carrier)))
				  (alt question-role-carrier
				       (((scope ((role #(under located))))
					 (proc ((alt (((type locative))
						      ((type composite) (relation-type locative))))))
					 (scope {^ partic located}))
					((scope ((role #(under possessor))))
					 (alt (((proc ((type composite) (relation-type possessive)))
						(dative-move yes))
					       ((proc ((type possessive))))))
					 (scope {^ partic possessor}))
					((scope {^ partic carrier})
					 (proc ((alt (((type ascriptive))
						      ((type compositve) (relation-type ascriptive)))))))))

				  (alt partic-carrier-scope
				       (((scope ((cat given))))
					((scope ((cat question-pronoun)))))))

				 ((scope ((role possessed)))
				  (alt partic-possessed-scope
				       (((partic ((possessed given))))
					((partic ((possessed ((cat question-pronoun))))))))
				  (scope {^ partic possessed}))

				 ((scope ((role attribute)))
				  (alt partic-attribute-scope
				       (((partic ((attribute given))))
					((partic ((attribute ((cat question-pronoun))))))))
				  (scope {^ partic attribute}))

				 ((scope ((role identified)))
				  (alt partic-identified-scope
				       (((partic ((identified given))))
					((partic ((identified ((cat question-pronoun))))))))
				  (scope {^ partic identified}))

				 ((scope ((role identifier)))
				  (question ((restrictive yes)))
				  (alt partic-identifier-scope
				       (((partic ((identifier given))))
					((partic ((identifier ((cat question-pronoun))))))))
				  (scope {^ partic identifier}))))

			   (opt ((scope ((cat question-pronoun))))))

			  ;; Scope of question in a PP (embedded alt) - scope points to the "semantic" constituent
			  ;; which is generally an NP embedded in a complement of type PP.
			  ;; Make sure to make the complement gapped and process the NP scope to propagate semantic features.
			  ;; question is a PP (prep wh)
			  ;; scope points to the wh element (np type)
			  (			   
			   (question ((cat pp)
				      (prep {^2 prep})
				      (np ((cat question-pronoun)
					   (syntax ((case objective)))
					   (semantics {^3 scope semantics})))))
			   (alt embedded-question-elt (:index  (scope role))
				(demo-message "This is an embedded question.  What preposition must be used?")
				(
				 ;; instrument --> with what
				 ((scope ((role instrument)))
				  (alt (((circum ((instrument given))))
					((circum ((instrument ((cat question-pronoun))))))))
				  (circum ((instrument-comp ((gap yes)))))
				  (alt (((prep ((lex {^2 circum instrument-comp prep lex}))))
					((prep ((lex "with"))))))
				  (scope {^ circum instrument-comp np}))

				 ;; possessor can participate in a dative-move alternation
				 ;; if there is no dative-move - must be an embedded question
				 ;; to whom did I give a book / whom did I give a book
				 ;; The embedded case is:
				 ;; (dative-move no) (proc ((type composite) (relation-type possessive)))
				 ((scope ((role #(under possessor))))
				  (proc ((type composite) (relation-type possessive)))
				  (dative-move no)
				  (synt-roles ((dative ((gap yes)))))
				  (alt partic-possessor-scope-dative-no
				       (((partic ((possessor given))))
					((partic ((possessor ((cat question-pronoun))))))))
				  (scope {^ synt-roles dative np})
				  (prep {^ synt-roles dative prep}))

				 ;; accompaniment core process -> with wh...
				 ((scope ((role #(under location))))
				  (proc ((type accompaniment)))
				  (prep ((lex "with")))
				  (alt partic-location-scope-accompaniment 
				       (((partic ((location given))))
					((partic ((location ((cat question-pronoun))))))))
				  (scope {^ partic location}))

				 ;; accompaniment circum: with wh...
				 ((scope ((role accompaniment)))
				  (opt ((prep ((lex "with")))))
				  (alt circum-accompaniment-scope
				       (((circum ((accompaniment given))))
					((circum ((accompaniment ((cat question-pronoun))))))))
				  (circum ((accompaniment-comp ((gap yes)))))
				  (scope {^ circum accompaniment-comp np}))

				 ((scope ((role to-loc)))
				  (opt ((prep ((lex "to")))))
				  (question ((np ((lex "where")))))
				  (alt circum-to-loc-scope
				       (((circum ((to-loc given))))
					((circum ((to-loc ((cat question-pronoun))))))))
				  (circum ((to-loc-comp ((gap yes)))))
				  (scope {^ circum to-loc-comp np}))

				 ((scope ((role from-loc)))
				  (question ((np ((lex "where")))))
				  (opt ((prep ((lex "from")))))
				  (alt circum-from-loc-scope
				       (((circum ((from-loc given))))
					((circum ((from-loc ((cat question-pronoun))))))))
				  (circum ((from-loc-comp ((gap yes)))))
				  (scope {^ circum from-loc-comp np}))

				 ((scope ((role on-loc)))
				  ;; (question ((np ((lex "where")))))
				  (opt ((prep ((lex "on")))))
				  (alt circum-on-loc-scope
				       (((circum ((on-loc given))))
					((circum ((on-loc ((cat question-pronoun))))))))
				  (circum ((on-loc-comp ((gap yes)))))
				  (scope {^ circum on-loc-comp np}))

				 ((scope ((role in-loc)))
				  ;; (question ((np ((lex "where")))))
				  (opt ((prep ((lex "in")))))
				  (alt circum-in-loc-scope
				       (((circum ((in-loc given))))
					((circum ((in-loc ((cat question-pronoun))))))))
				  (circum ((in-loc-comp ((gap yes)))))
				  (scope {^ circum in-loc-comp np}))

				 ;; purpose can be np or clause - if it is specified skip else default
				 ((scope ((role purpose)))
				  (opt ((prep ((lex "for")))))
				  (alt circum-purpose-scope
				       (((circum ((purpose ((cat #(under clause))))))
					 (scope {^ circum purpose-comp}))
					((circum ((purpose ((cat #(under pp))))))
					 (scope {^ circum purpose-comp np}))
					((circum ((purpose ((cat #(under np))))))
					 (scope {^ circum purpose}))
					((circum ((purpose ((cat question-pronoun)))))
					 (scope {^ circum purpose-comp np}))))
				  (circum ((purpose-comp ((gap yes))))))

				 ((scope ((role behalf)))
				  (opt ((prep ((lex "for")))))
				  (alt circum-behalf-scope
				       (((circum ((behalf ((cat #(under clause))))))
					 (scope {^ circum behalf-comp}))
					((circum ((behalf ((cat #(under pp))))))
					 (scope {^ circum behalf-comp np}))
					((circum ((behalf ((cat #(under np))))))
					 (scope {^ circum behalf}))
					((circum ((behalf ((cat question-pronoun)))))
					 (scope {^ circum behalf-comp np}))))
				  (circum ((behalf-comp ((gap yes))))))))

			   )))

		    (pattern (dots question fronted-aux fronted-not start dots)))

		   ;; MOOD RELATIVE
		   ((mood relative)
		    (alt (trace relative) (:index mood) 
			 (demo-message "Is the relative clause simple or embedded in a PP?")
			 (((mood simple-relative)

			   ;; Example: the woman who lives there
			   ;;          the man whom I know
			   ;;          the reason why I came
			   ;;          the person whom I gave a book
			   ;; Simple relative is the qualifier of an NP. The NP
			   ;; is a constituent of the relative clause, as indicated
			   ;; by the scope constituent:
			   ;; if NP is agent, do (scope ((role agent))) in the relative
			   ;; clause. Scope inherits the relevant features from the 
			   ;; head of the enclosing NP.
			   ;; @@@ semantics is pointing outside the scope of the local clause
			   ;; @@@ this is not good - better pass it down from np to qualifier
			   (pattern (relative-marker start dots))
			   (scope ((gap yes)))

			   ;; (scope (((semantics ((index {^4 semantics index}))))))

			   (alt relative-marker (:index (scope role))
				(demo-message "Choosing a relative pronoun.")
				(

				 ((scope ((role at-loc))) 
				  (alt circum-at-loc-scope
				       (((circum ((at-loc given))))
					((circum ((at-loc ((cat relative-pronoun))))))))
				  (circum ((at-loc-comp ((gap yes)))))
				  (scope {^ circum at-loc-comp np})
				  (relative-marker ((cat relpro) (lex "where"))))
				 
				 ;; spatial is a simple-relative -- accompaniment - must be embedded relative below
				 ((scope ((role location)))
				  (proc ((type spatial)))
				  (alt partic-location-scope
				       (((partic ((location given))))
					((partic ((location ((cat relative-pronoun))))))))
				  (scope {^ partic location})
				  (alt scope-role-location-mode 
				       (((circumstance-as participant)
					 (relation-mode attributive)
					 (relative-marker ((cat relpro) (lex "where"))))
					((relation-mode equative)))))

				 ((scope ((role at-time)))
				  (relative-marker ((cat relpro) (lex "when")))
				  (circum ((at-time-comp ((gap yes)))))
				  (alt circum-at-time-scope
				       (((circum ((at-time ((cat #(under adv))))))
					 (scope {^ circum at-time}))
					((circum ((at-time ((cat #(under clause))))))
					 (scope {^ circum at-time}))
					((circum ((at-time ((cat #(under np))))))
					 (scope {^ circum at-time}))
					((circum ((at-time ((cat #(under pp))))))
					 (scope {^ circum at-time np}))
					((circum ((at-time ((cat relative-pronoun)
							    (time-type ((alt (given "at"))))))))
					 (scope {^ circum at-time})))))

				 ((scope ((role time)))
				  (alt partic-time-scope
				       (((partic ((time given))))
					((partic ((time ((cat relative-pronoun))))))))
				  (alt relative-marker-time
				       (((relation-mode equative))
					((relation-mode attributive)
					 (relative-marker ((cat relpro) (lex "when"))))))
				  (scope {^ partic time}))

				 ((scope ((role reason)))
				  (relative-marker ((cat relpro) (lex "why")))
				  (circum ((reason-comp ((gap yes)))))
				  (alt circum-reason-scope
				       (((circum ((reason ((cat #(under clause))))))
					 (scope {^ circum reason}))
					((circum ((reason ((cat #(under pp))))))
					 (scope {^ circum reason np}))
					((circum ((reason ((cat #(under np))))))
					 (scope {^ circum reason}))
					((circum ((reason ((cat relative-pronoun)))))
					 (scope {^ circum reason})))))

				 ((scope ((role manner)))
				  (relative-marker ((cat relpro) (lex "how")))
				  (circum ((manner-comp ((gap yes)))))
				  (alt circum-manner-scope
				       (((circum ((manner ((cat #(under clause))))))
					 (scope {^ circum manner}))
					((circum ((manner ((cat #(under adv))))))
					 (scope {^ circum manner}))
					((circum ((manner ((cat #(under pp))))))
					 (scope {^ circum manner np}))
					((circum ((manner ((cat #(under np))))))
					 (scope {^ circum manner}))
					((circum ((manner ((cat relative-pronoun)))))
					 (scope {^ circum manner})))))

				 ((scope ((role agent)))
				  (alt partic-agent-scope
				       (((partic ((agent given))))
					((partic ((agent ((cat relative-pronoun))))))))
				  (scope {^ partic agent}))

				 ((scope ((role affected)))
				  (alt partic-affected-scope
				       (((partic ((affected given))))
					((partic ((affected ((cat relative-pronoun))))))))
				  (scope {^ partic affected}))

				 ((scope ((role processor)))
				  (alt partic-processor-scope
				       (((partic ((processor given))))
					((partic ((processor ((cat relative-pronoun))))))))
				  (scope {^ partic processor}))

				 ((scope ((role phenomenon)))
				  (alt partic-phenomenon-scope
				       (((partic ((phenomenon given))))
					((partic ((phenomenon ((cat relative-pronoun))))))))
				  (scope {^ partic phenomenon}))

				 ((scope ((role carrier)))
				  (alt relative-role-carrier
				       (((scope ((role #(under located))))
					 (proc ((alt (((type locative))
						      ((type composite) (relation-type locative))))))
					 (scope {^ partic located}))
					((scope ((role #(under possessor))))
					 (alt (((proc ((type composite) (relation-type possessive)))
						(dative-move yes))
					       ((proc ((type possessive))))))
					 (scope {^ partic possessor}))
					((scope {^ partic carrier})
					 (proc ((alt (((type ascriptive))
						      ((type composite) (relation-type ascriptive)))))))))
				  (alt (((scope ((cat given))))
					((scope ((cat relative-pronoun)))))))

				 ((scope ((role possessed)))
				  (alt partic-possessed-scope
				       (((partic ((possessed given))))
					((partic ((possessed ((cat relative-pronoun))))))))
				  (scope {^ partic possessed}))

				 ((scope ((role attribute)))
				  (alt partic-attribute-scope
				       (((partic ((attribute given))))
					((partic ((attribute ((cat relative-pronoun))))))))
				  (scope {^ partic attribute}))

				 ((scope ((role identified)))
				  (alt partic-identified-scope
				       (((partic ((identified given))))
					((partic ((identified ((cat relative-pronoun))))))))
				  (scope {^ partic identified}))

				 ((scope ((role identifier)))
				  (restrictive ((alt (given yes))))
				  (alt partic-identifier-scope
				       (((partic ((identifier given))))
					((partic ((identifier ((cat relative-pronoun))))))))
				  (scope {^ partic identifier}))

				 ))

			   ;; propagate restrictive
			   (restrictive {^ relative-marker restrictive})
			   (scope ((restrictive {^2 restrictive})))
			   (alt relative-marker2
				(((relative-marker ((lex given))))
				 ((alt restrictive-relative (:index restrictive)
				       (((restrictive yes)
					 (relative-marker ((cat relpro) (lex "that"))))
					((restrictive no)
					 (relative-marker 
					  ((cat relative-pronoun)
					   (semantics {^ ^ scope semantics})
					   (syntax {^ ^ scope syntax}))))))))))

			  ;; EMBEDDED RELATIVE - SCOPE is within a PP
			  ((mood embedded-relative)
			   ;; Example: the box in which we store the elixir
			   ;;          an experience the likes of which you have never seen
			   (pattern (prep relative-marker start dots))
			   (prep ((cat prep)))
			   (scope ((gap yes)
				   (restrictive {^2 restrictive})))
			   (relative-marker ((cat relative-pronoun)
					     (restrictive no)
					     (animate {^2 scope animate})
					     (semantics {^ ^ scope semantics})
					     (syntax ((case objective)))))
			   (alt scope-embedded (:index (scope role))
				(demo-message "This is an embedded relative.  What preposition must be used?")

				(
				 ;; scope points to the "semantic" element - generally an NP
				 ;; but can also be a clause or an adv.
				 ;; scope can point just to a name (and the grammar fills it up with defaults 
				 ;; a few defaults can be specified at the scope level similar to an NP - animate, restrictive).
				 ;; or it can point to an existing component
				 ;; (and the grammar gaps it and inherit its semantics).

				 ((scope ((role instrument)))
				  (opt ((prep ((lex "with")))))
				  (alt circum-instrument-scope
				       (((circum ((instrument given))))
					((circum ((instrument ((cat relative-pronoun))))))))
				  (circum ((instrument-comp ((gap yes)))))
				  (scope {^ circum instrument}))

				 ;; possessor can participate in a dative-move alternation
				 ;; if there is no dative-move - must be an embedded relative
				 ;; to whom / whom
				 ;; The embedded case is:
				 ;; (dative-move no) (proc ((type composite) (relation-type possessive)))
				 ((scope ((role #(under possessor))))
				  (proc ((type composite) (relation-type possessive)))
				  (dative-move no)
				  (alt partic-possessor-scope-dative
				       (((partic ((possessor given))))
					((partic ((possessor ((cat relative-pronoun))))))))
				  (synt-roles ((dative ((gap yes)))))
				  (scope {^ synt-roles dative np})
				  (prep {^ synt-roles dative prep}))

				 ;; spatial is a simple-relative -- accompaniment - must be embedded relative below
				 ((scope ((role #(under location))))
				  (proc ((type accompaniment)))
				  (prep ((lex "with")))
				  (alt partic-location-scope-accompaniment
				       (((partic ((location given))))
					((partic ((location ((cat relative-pronoun))))))))
				  (scope {^ partic location}))

				 ((scope ((role accompaniment)))
				  (opt ((prep ((lex "with")))))
				  (alt circum-accompaniment-scope
				       (((circum ((accompaniment given))))
					((circum ((accompaniment ((cat relative-pronoun))))))))
				  (circum ((accompaniment-comp ((gap yes)))))
				  (scope {^ circum accompaniment}))

				 ((scope ((role to-loc)))
				  (opt ((prep ((lex "to")))))
				  (alt circum-to-loc-scope
				       (((circum ((to-loc given))))
					((circum ((to-loc ((cat relative-pronoun))))))))
				  (circum ((to-loc-comp ((gap yes)))))
				  (scope {^ circum to-loc-comp np}))

				 ((scope ((role from-loc)))
				  (opt ((prep ((lex "from")))))
				  (alt circum-from-loc-scope
				       (((circum ((from-loc given))))
					((circum ((from-loc ((cat relative-pronoun))))))))
				  (circum ((from-loc-comp ((gap yes)))))
				  (scope {^ circum from-loc-comp np}))

				 ((scope ((role on-loc)))
				  (opt ((prep ((lex "on")))))
				  (alt circum-on-loc-scope
				       (((circum ((on-loc given))))
					((circum ((on-loc ((cat relative-pronoun))))))))
				  (circum ((on-loc-comp ((gap yes)))))
				  (scope {^ circum on-loc-comp np}))

				 ((scope ((role in-loc)))
				  (opt ((prep ((lex "in")))))
				  (alt circum-in-loc-scope
				       (((circum ((in-loc given))))
					((circum ((in-loc ((cat relative-pronoun))))))))
				  (circum ((in-loc-comp ((gap yes)))))
				  (scope {^ circum in-loc-comp np}))

				 ;; purpose can be np or clause - if it is specified skip else default
				 ((scope ((role purpose)))
				  (opt ((prep ((lex "for")))))
				  (alt circum-purpose-scope
				       (((circum ((purpose ((cat #(under clause))))))
					 (scope {^ circum purpose-comp}))
					((circum ((purpose ((cat #(under pp))))))
					 (scope {^ circum purpose-comp np}))
					((circum ((purpose ((cat #(under np))))))
					 (scope {^ circum purpose}))
					((circum ((purpose ((cat relative-pronoun))))))))
				  (circum ((purpose-comp ((gap yes))))))

				 ((scope ((role behalf)))
				  (opt ((prep ((lex "for")))))
				  (alt circum-behalf-scope
				       (((circum ((behalf ((cat #(under clause))))))
					 (scope {^ circum behalf-comp}))
					((circum ((behalf ((cat #(under pp))))))
					 (scope {^ circum behalf-comp np}))
					((circum ((behalf ((cat #(under np))))))
					 (scope {^ circum behalf}))
					((circum ((behalf ((cat relative-pronoun))))))))
				  (circum ((behalf-comp ((gap yes))))))

			   )))

			  ;; ***** be-deleted not implemented
			  ;; Actually debatable whether useful: in the meantime, can
			  ;; generate them as AP in qualifier (cf note below on
			  ;; criteria to distinguish adjectives and participles).
			  ;; Example: Goldwater /who was/ crushed by his defeat
			  ;;          the team /that is/ expected to win
			  ;;          an enchanting island /that is/ waiting to be seen
			  ((mood be-deleted-relative))

			  ;; ***** wh-nominal not implemented
			  ;; Example: They were amazed at which one they chose
			  ;;          I couldn't believe how many people understood
			  ;;          What they did next was surprising
			  ;; To rework when nominalization is implemented [cf Vendler]
			  ((mood wh-nominal-relative))

			  ;; ***** wh-ever-nominal not implemented
			  ;; Example: Whoever did it will be sorry
			  ;; Cf nominalization.
			  ((mood wh-ever-nominal-relative)))))

		   ;; OTHER MOODS -- Nothing to do here
		   ((mood declarative))
		   ((mood bound))
		   ((mood non-finite))))


	     ;; TRANSITIVITY SYSTEM
	     ;; AND VOICE SYSTEM    --> operative, middle, receptive.
	     ;; Transitivity system determines what configuration of participants
	     ;; can be used for the current process.
	     ;; The voice system does the mapping semantic -> syntactic roles
	     ;; We do both at once in the following alt.
	     ;; For voice:
	     ;; When more than 1 participant:
	     ;; Choice receptive/operative is based on focus
	     ;; Nuclear Syntactic functions mapped to are: 
	     ;; subject, object, iobject, dative, by-obj, subj-comp, obj-comp
	     ;; subj-comp and obj-comp from Quirk, examples are:
	     ;; She made him a good wife     [S V O SC] = [Ag/Ca Af At]
	     ;; She made him a good husband  [S V O OC] = [Ag Af/Ca At]
	     ;; Other complements are added by the circumstantial roles one at a time.
	     ;; Things done:
	     ;; - Determine configuration of participants
	     ;; - Determine configuration of syntactic roles
	     ;; - Map participants to syntactic roles
	     ;; - Determine opportunities for passivation
	     ;; - Provides default verbs for processes
	     ;; There are 35 kernel templates, with 35 active and 18 passive forms
	     ;; Feature agentless determines whether a by-obj is used in passive
	     ;; Feature dative-move determines whether a recipient is realized as
	     ;; iobject or as dative pp.
	     (alt process (:index process-type)
		  (:demo "Is the process simple or composite?")
		  ;; Simple processes: list of templates
		  ;; Material: 
		  ;; Ag:       John runs.
		  ;; Ag+Af     John eats a pie.
		  ;; Ag+Cr     John cooks diner.
		  ;; Ag+Rg     John sings a song.
		  ;; Af        The sun shines.
		  ;; Cr        The window popped.
		  ;; Mental:   
		  ;; Pr        I think.
		  ;; Pr+Ph     I think it's good.
		  ;; Verbal:    
		  ;; Sa        It talks.
		  ;; Sa+Ad     John talks to Steve.
		  ;; Sa+Ve     Steve says fix it.
		  ;; Sa+Ad+Ve  Steve asks Doree to fix it.
		  ;; Ascriptive:
		  ;; Ca        I am.
		  ;; Ca+At     It is not personal.
		  ;; Id+Ir     Steve is Doree's advisor.
		  ;; Possessive:
		  ;; Ca+Pd  [Pr = Ca]   Steve has advisees.
		  ;; Id+Pd  [Pr = Id]   Steve owns a box.
		  ;; Locative:
		  ;; [none]             It rains.
		  ;; Ca                 There is a unicorn.
		  ;; Ca+Ln  [Ca = Ld]   Steve is in his office.
		  ;; Id+Ln  [Id = Ld]   Steve's office contains a computer.
		  (((process-type simple-process)
		    (alt simple-process (:index process-type)
			 (
			  ;; Material: Ag+Af or Ag+Cr or Ag+Rg
			  ((process-type material)
			   (participants ((fset (agent affected created range))))
			   (alt agentive (:index (process agentive))
				(((process ((agentive yes)))
				  (innermost-role {^ participants agent})
				  (alt effective (:index (process effective))
				       (((process ((effective yes)))
					 (alt effect-type (:index (process effect-type))
					      (((process ((effect-type dispositive)))
						(participants ((affected given)
							       (fset (agent affected))))
						;; VOICE: Map to syntactic functions
						(alt ag-af-voice (:index (process voice))
						     (((process ((voice active)))
						       (synt-roles
							((fset (subject object))
							 (subject {^ ^ participants agent})
							 (object  {^ ^ participants affected}))))
						      ((process ((voice passive)))
						       (synt-roles
							((fset (subject by-obj))
							 (subject {^ ^ participants affected})))))))
			     			     
					       ((process ((effect-type creative)))
						(participants ((created given)
							       (fset (agent created))))
						;; VOICE
						(alt ag-cr-voice (:index (process voice))
						     (((process ((voice active)))
						       (synt-roles
							((fset (subject object))
							 (subject {^ ^ participants agent})
							 (object  {^ ^ participants created}))))
						      ((process ((voice passive)))
						       (synt-roles
							((fset (subject by-obj))
							 (subject {^ ^ participants created})))))))

					       ;; end of effective yes
					       )))

					((process ((effective no)
						   (agentive yes)))
					 (participants ((fset (agent range))))
					 (alt event-as (:index (process event-as))
					      (((participants ((range given)))
						;; VOICE
						(alt ag-ra-voice (:index (process voice))
						     (((process ((voice active)))
						       (synt-roles
							((fset (subject object))
							 (subject {^ ^ participants agent})
							 (object  {^ ^ participants range}))))
						      ((process ((voice passive)))
						       (synt-roles
							((fset (subject by-obj))
							 (subject {^ ^ participants range})))))))
					       ((participants ((range none)))
						;; VOICE: no passive!
						(process ((voice active)))
						(synt-roles 
						 ((fset (subject))
						  (subject {^ ^ participants agent})))))))))

				  ;; For passive voice, have an explicit by-obj?
				  (opt ((process ((voice passive)))
					(alt agless (:index agentless)
					     (((agentless no)
					       (synt-roles
						((by-obj ((np {^ ^ participants agent}))))))
					      ((agentless yes)
					       (synt-roles ((by-obj none))))))))
		  

				  ;; end of agentive yes
				  )

				 ((process ((agentive no)
					    (effective yes)))
				  (alt af-type (:index (process effect-type))
				       (((process ((effect-type dispositive)))
					 (innermost-role {^ participants affected})
					 (participants 
					  ((fset (affected))
					   (affected given)))
					 ;; VOICE: no passive!
					 (process ((voice active)))
					 (synt-roles ((fset (subject))
						      (subject {^ ^ participants affected}))))

					((process ((effect-type creative)))
					 (innermost-role {^ participants created})
					 (participants ((fset (created))
							(created given)))
					 ;; VOICE: no passive!
					 (process ((voice active)))
					 (synt-roles ((fset (subject))
						      (subject {^ ^ participants created})))))))

				 ;; end of material
				 )))

			  ;; Mental processes:
			  ((process-type mental)
			   (innermost-role {^ participants processor})
			   (participants ((fset (processor phenomenon))))
			   ;; VOICE: depends on transitivity
			   (alt mental-transitive (:index (process transitive))
				(((process ((transitive yes)))
				  ;; (participants ((phenomenon any)))
				  (alt pr-ph-voice (:index (process voice))
				       (((process ((voice active)))
					 (synt-roles 
					  ((fset (subject object))
					   (subject {^ ^ participants processor})
					   (object {^ ^ participants phenomenon}))))
					((process ((voice passive)))
					 (synt-roles ((fset (subject by-obj))
						      (subject {^ ^ participants phenomenon})))
					 (alt pr-ph-agless (:index agentless)
					      (((agentless no)
						(synt-roles
						 ((by-obj ((np {^ ^ ^ participants phenomenon}))))))
					       ((agentless yes)
						(synt-roles ((by-obj none))))))))))
				 ((process ((transitive no)
					    (voice active)))  
				  (participants ((phenomenon none)))
				  (synt-roles ((fset (subject))
					       (subject {^ ^ participants processor})))))))
	       
			  ;; Verbal processes:  *****
			  ((process-type verbal)
			   (innermost-role {^ participants sayer})
			   (participants ((fset (sayer addressee verbalization)))))
	       

			  ;; Relational processes:
			  ((process-type relation)

			   ;; General things on Mode 
			   (alt mode (:index (process mode))
				(((process ((mode attributive)
					    (voice active)))
				  (innermost-role {^ participants carrier}))

				 ((process ((mode equative)))
				  (innermost-role {^ participants identified}))))

			   (process ((change-mode ((alt (current maintain change))))))

			   ;; Specializations of process/roles
			   ;; Ascriptive, Locative, Possessive
			   (alt relation-type (:index process-type)
				(
				 ;; ascriptive - sort of default
				 ;; Corresponding default verb
				 ;; Note that "to be" in equative and attrib. has different
				 ;; properties in terms of subject-clause/object-clause.
				 ((process-type ascriptive)
				  (alt asc-mode (:index (process mode))
				       (((process ((mode attributive)))
					 (participants ((fset (carrier attribute))
							(carrier given)))
					 (alt verb-be-attributive 
					      (:demo "What verb for ascriptive attributive?")
					      (((process ((lex "be")
							  (subject-clause infinitive)
							  (object-clause none))))
					       ((process ((lex given)))))))
					((process ((mode equative)))
					 (alt verb-be-equative 
					      (:demo "What verb for ascriptive equative?")
					      (((process ((lex "be")
							  (subject-clause that)
							  (object-clause present-participle))))
					       ((process ((lex given))))))))))

				 ;; locative can have 0, 1 or 2 participants
				 ((process-type locative)
				  (alt loc-mode (:index (process mode))
				       (((process ((mode attributive)
						   (voice active)))
					 (alt loc-arity (:index process-type)
					      (
					       ;; NATURAL-PHENOMENON
					       ((process-type natural-phenom)
						(participants none)
						(synt-roles-done yes) ;; This is a special case
						(synt-roles 
						 ((fset (subject))
						  (subject ((lex "it")
							    (number singular)
							    (cat personal-pronoun))))))
					       ;; EXISTENTIAL
					       ((process-type existential)
						(participants ((fset (carrier located))
							       (located {^ carrier})
							       (carrier given)))
						(synt-roles-done yes) ;; This is a special case
						(synt-roles 
						 ((fset (subject subj-comp))
						  (subject ((lex "there")
							    (number {^ ^ ^ participants carrier number})
							    (cat demonstrative-pronoun)))
						  (subj-comp {^ ^ participants carrier}))))
					       ;; TEMPORAL
					       ((process-type temporal)
						(participants 
						 ((fset (carrier attribute located time))
						  (located {^ carrier})
						  (located given)
						  (time {^ attribute}))))
					       ;; ANY OTHER LOCATIVE
					       ((process-type locative)
						(participants 
						 ((fset (carrier attribute located location))
						  (located {^ carrier})
						  (located given)
						  (location {^ attribute})
						  (location given)))))))

					((process ((mode equative)))
					 (participants 
					  ((fset (identifier identified located location))
					   (located {^ identified})
					   (location {^ identifier}))))))

				  ;; DEFAULT VERB LOCATIVE
				  (process ((alt (((lex "be")
						   (subject-clause none)
						   (object-clause none))
						  ((lex given)))))))


				 ((process-type possessive)
				  (alt pos-mode (:index (process mode))
				       (((process ((mode attributive)))
					 (participants 
					  ((fset (carrier attribute possessor possessed))
					   (possessor {^ carrier})
					   (possessed {^ attribute})))
					 ;; Default verb is "has"
					 (alt verb-have-possessive
					      (:demo "What verb for possessive?")
					      (((process ((lex "have")
							  (subject-clause infinitive)
							  (object-clause none))))
					       ((process ((lex given)))))))

					((process ((mode equative)))
					 (participants 
					  ((fset (identifier identified possessor possessed))
					   (possessor {^ identified})
					   (possessed {^ identifier})))
					 ;; Default verb is "own"
					 (alt verb-own-possessive
					      (:demo "What verb for possessive?")
					      (((process ((lex "own")
							  (subject-clause none)
							  (object-clause none))))
					       ((process ((lex given))))))))))))


			   ;; VOICE for relation except when already done
			   (alt voice-relation (:index (process voice))
				(:ignore-when (({^ synt-roles-done} given)))
				(((process ((voice active)
					    (mode attributive)))
				  (synt-roles 
				   ((fset (subject subj-comp))
				    (subject {^ ^ participants carrier})
				    (subj-comp {^ ^ participants attribute}))))

				 ((process ((voice active)
					    (mode equative)))
				  (synt-roles
				   ((fset (subject subj-comp))
				    (subject {^ ^ participants identified})
				    (subj-comp {^ ^ participants identifier}))))

				 ((process ((voice passive)
					    (mode equative)))
				  (alt equative-passive 
				       (
					;; ascriptive passive is just a swap of the
					;; order of arguments around the copula.
					((process-type ascriptive)
					 (process ((copula yes)))
					 (synt-roles ((fset (subject subj-comp))
						      (subject {^ ^ participants identifier})
						      (subj-comp {^ ^ participants identified}))))
					;; all the other types have a by-obj
					((process-type ((alt (locative possessive))))
					 (synt-roles 
					  ((fset (subject by-obj))
					   (subject {^ ^ participants identifier})))
					 (alt rel-agless (:index agentless)
					      (((agentless no)
						(synt-roles
						 ((by-obj ((np {^ ^ ^ participants identified}))))))
					       ((agentless yes)
						(synt-roles ((by-obj none)))))))))))))

			  ;; end of process-type simple
			  )))


		   ;; COMPOSITE PROCESSES:
		   ;; compose only material + relation
		   ;; Index on agentive/effective/effect-type
		   ;; Best to view composition as adding a result relation to an action
		   ;; Table of possible combinations:
		   ;; event = Ag, Ag+Af, Ag+Cr, Af, Cr 
		   ;; relation = Ca+Att, Ca+Pos, Ca+Loc, Id+Ir, [Id+Loc, Id+Pos]
		   ;; The following embedded alts identify only the permissible 
		   ;; combinations [think of it as a matrix event/rel]
		   ;; Ag+Af/Ca+At:  they made him rich
		   ;; Ag+Af/Id+Ir:  they made him the boss
		   ;; Ag+Af/Ca+Pos: the boss gave the babe cold cash
		   ;; Ag+Af/Ca+Loc: I push the box to the left
		   ;; Ag+Cr/Ca+At:  he cooked the diner spicy (?)
		   ;; Ag+Cr/Ca+Loc: The program popped the window on the screen
		   ;; Ag/Ca+At:     he became rich
		   ;; Ag/Id+Ir:     he became the boss
		   ;; Ag/Ca+Pos:    the boss bought a Rolls
		   ;; Ag/Ca+Loc:    he went home
		   ;; Af/Ca+At:     the kettle boiled dry
		   ;; Af/Ca+Pos:    the boss received cash
		   ;; Af/Ca+Loc:    the box fell on the floor
		   ;; Cr/Ca+At:     he was born blind (?)
		   ;; Cr/Ca+Loc:    the window popped on the screen
		   ((process-type composite)
		    (participants ((range none))) ;; don't want to mess with that
		    (alt composite-agentive (:index (process agentive))
			 (((process ((agentive yes)))
			   (innermost-role {^ participants agent})
			   (participants ((agent given)
					  (agent ((function agent)))))
			   (alt composite-effective (:index (process effective))
				(((process ((effective yes)))
				  (participants ((agent ((function third-party-agent)))))
				  (alt composite-effect-type (:index (process effect-type))
				       (((process ((effect-type dispositive)))
					 ;; Structure is Ag+Af

					 ;; Enumerate acceptable relation types
					 (alt ag-af-relation (:index (process relation-type))
					      (
					       ;; Ag+Af/Ca+At: they made him rich
					       ((process ((relation-type ascriptive)
							  (mode attributive)))
						(participants 
						 ((attribute given)
						  (carrier ((function carrier)))
						  (affected {^ carrier})
						  (fset (agent affected carrier attribute))))
						;; Default verb is "make"
						(process
						 ((alt (((lex "make"))
							((lex given))))))
						;; VOICE
						(alt ag-af-voice1 (:index (process voice))
						     (((process ((voice active)))
						       (synt-roles
							((fset (subject object obj-comp))
							 (subject {^ ^ participants agent})
							 (object {^ ^ participants affected})
							 (obj-comp {^ ^ participants attribute}))))
						      ((process ((voice passive)))
						       (synt-roles
							((fset (subject subj-comp by-obj))
							 (subject {^ ^ participants affected})
							 (subj-comp {^ ^ participants attribute})))))))

					       ;; Ag+Af/Id+Ir: they elected him the boss
					       ((process ((relation-type ascriptive)
							  (mode equative)))
						(participants 
						 ((identifier given)
						  (identified ((function identified)))
						  (affected {^ identified})
						  (fset (agent affected identified identifier))))
						;; Default verb is "make"
						(process
						 ((alt (((lex "make"))
							((lex given))))))
						;; VOICE
						(alt ag-af-voice2 (:index (process voice))
						     (((process ((voice active)))
						       (synt-roles
							((fset (subject object obj-comp))
							 (subject {^ ^ participants agent})
							 (object {^ ^ participants affected})
							 (obj-comp {^ ^ participants identifier}))))
						      ((process ((voice passive)))
						       (synt-roles
							((fset (subject subj-comp by-obj))
							 (subject {^ ^ participants affected})
							 (subj-comp {^ ^ participants identifier})))))))
			 
					       ;; Ag+Af/Ca+Pos: the babe gave the boss cold cash
					       ((process ((relation-type possessive)
							  (mode attributive)))
						(participants
						 ((possessed given)
						  (carrier ((function carrier)))
						  (affected {^ carrier})
						  (possessor {^ carrier})
						  (possessed {^ attribute})
						  (fset (agent affected carrier attribute
							       possessor possessed))))
						;; Default verb is "give"
						(process
						 ((alt (((lex "give"))
							((lex given))))))
						;; VOICE: handle dative-move and 2-way passive
						(alt ag-af-voice3 (:index (process voice))
						     (((process ((voice active)))
						       (synt-roles
							((fset (subject object iobject dative))
							 (subject {^ ^ participants agent})
							 (object {^ ^ participants possessed})))
						       (alt dative-move (:index dative-move)
							    (((dative-move yes)
							      (synt-roles 
							       ((iobject {^ ^ participants affected})
								(dative none))))
							     ((dative-move no)
							      (synt-roles
							       ((iobject none)
								(dative ((np {^ ^ ^ participants affected})
									 ))))))))

						      ;; Mary is given a book by John
						      ((process ((voice passive)))
						       (dative-move yes)
						       (synt-roles
							((fset (subject object by-obj))
							 (subject {^ ^ participants affected})
							 (object {^ ^ participants possessed}))))

						      ;; A book is given by John to Mary
						      ((process ((voice passive)))
						       (dative-move no)
						       (synt-roles
							((fset (subject by-obj dative))
							 (subject {^ ^ participants possessed})
							 (dative ((np {^ ^ ^ participants affected})))
							 )))

						      )))
			 
					       ;; Ag+Af/Ca+Loc: I push the box to the left
					       ((process ((relation-type locative)
							  (mode attributive)))
						(participants
						 ((location given)
						  (carrier ((function carrier)))
						  (located {^ carrier})
						  (location {^ attribute})
						  (affected {^ carrier})
						  (fset (agent affected carrier attribute
							       located location))))
						;; Default verb is "move"
						(process
						 ((alt (((lex "move"))
							((lex given))))))
						;; VOICE
						(alt ag-af-voice4 (:index (process voice))
						     (((process ((voice active)))
						       (synt-roles
							((fset (subject object obj-comp))
							 (subject {^ ^ participants agent})
							 (object {^ ^ participants affected})
							 (obj-comp {^ ^ participants location}))))
						      ((process ((voice passive)))
						       (synt-roles
							((fset (subject subj-comp by-obj))
							 (subject {^ ^ participants affected})
							 (subj-comp {^ ^ participants location})))))))
					       ))
					 ;; Check now that affected or any synonym is indeed given
					 (participants ((affected given)
							(affected ((function affected))))))

					((process ((effect-type creative)))
					 ;; Structure is Ag+Cr
					 ;; default verb is "create"
					 (process ((alt (((lex "create")
							  (object-clause none))
							 ((lex given))))))
					 ;; Enumerate acceptable relation types
					 (alt ag-cr-relation (:index (process relation-type))
					      (
					       ;; Ag+Cr/Ca+At:  he cooked the diner spicy (?)
					       ((process ((relation-type ascriptive)
							  (mode attributive)))
						(participants 
						 ((attribute given)
						  (fset (agent created carrier attribute)))))

					       ;; Ag+Cr/Ca+Loc: The prg popped the wnd on the screen
					       ((process ((relation-type locative)
							  (mode attributive)))
						(participants
						 ((location given)
						  (located {^ carrier})
						  (attribute {^ location})
						  (fset (agent created carrier attribute
							       located location)))))
					       ))
					 ;; Check now that created or any synonym is given
					 (participants ((created given)
							(carrier {^ created})
							(created ((function created)))))
					 ;; VOICE (common for both patterns)
					 (alt ag-cr-voice (:index (process voice))
					      (((process ((voice active)))
						(synt-roles
						 ((fset (subject object obj-comp))
						  (subject {^ ^ participants agent})
						  (object {^ ^ participants created})
						  (obj-comp {^ ^ participants attribute}))))
					       ((process ((voice passive)))
						(synt-roles
						 ((fset (subject subj-comp by-obj))
						  (subject {^ ^ participants created})
						  (subj-comp {^ ^ participants attribute})))))))
		     
					;; end of effective yes
					)))

				 ;; Agent only
				 ((process ((effective no)))
				  ;; Enumerate permissible relation types
				  (alt ag-relation (:index (process relation-type))
				       (
					;; Ag/Ca+At:     he became rich
					((process ((relation-type ascriptive)
						   (mode attributive)
						   (voice active)))
					 (participants ((carrier ((function carrier)))
							(carrier {^ agent})
							(attribute given)
							(fset (agent carrier attribute))))
					 ;; Default verb is "become"
					 (process ((alt (((lex "become")
							  (subject-clause none)
							  (object-clause none)
							  (change-mode change))
							 ((lex given))))))
					 ;; VOICE: active only
					 (synt-roles
					  ((fset (subject subj-comp))
					   (subject {^ ^ participants agent})
					   (subj-comp {^ ^ participants attribute}))))

					;; Ag/Id+Ir:     he became the boss
					((process ((relation-type ascriptive)
						   (voice active)
						   (mode equative)))
					 (participants ((identifier given)
							(identified ((function identified)))
							(identified {^ agent})
							(fset (agent identified identifier))))
					 ;; Default verb is "become"
					 (process ((alt (((lex "become")
							  (subject-clause none)
							  (object-clause none)
							  (change-mode change))
							 ((lex given))))))
					 ;; VOICE: active only
					 (synt-roles
					  ((fset (subject subj-comp))
					   (subject {^ ^ participants agent})
					   (subj-comp {^ ^ participants identifier}))))
			 
					;; Ag/Ca+Pos:    the boss bought a Rolls
					((process ((relation-type possessive)
						   (mode attributive)))
					 (participants
					  ((possessed given)
					   (carrier ((function carrier)))
					   (carrier {^ agent})
					   (attribute {^ possessed})
					   (possessor {^ carrier})
					   (fset (agent carrier attribute
							possessor possessed))))
					 ;; Default verb is "get"
					 (process ((alt (((lex "get"))
							 ((lex given))))))
					 ;; VOICE
					 (alt ag-pos-voice (:index (process voice))
					      (((process ((voice active)))
						(synt-roles
						 ((fset (subject object))
						  (subject {^ ^ participants agent})
						  (object {^ ^ participants possessed}))))
					       ((process ((voice passive)))
						(synt-roles
						 ((fset (subject by-obj))
						  (subject {^ ^ participants possessed})))))))
			 
					;; Ag/Ca+Loc:    he went home
					((process ((relation-type locative)
						   (voice active)
						   (mode attributive)))
					 (participants
					  ((location given)
					   (attribute {^ location})
					   (carrier {^ located})
					   (carrier {^ agent})
					   (carrier ((function carrier)))
					   (fset (agent carrier attribute
							located location))))
					 ;; Default verb is "go"
					 (process ((alt (((lex "go")
							  (object-clause none))
							 ((lex given))))))
					 ;; VOICE: active only
					 (synt-roles
					  ((fset (subject subj-comp))
					   (subject {^ ^ participants agent})
					   (subj-comp {^ ^ participants attribute}))))

					)))))

			   ;; Handle the optional by-obj in passive forms
			   (opt 
			    ((process ((voice passive)))
			     (alt agless (:index agentless)
				  (((agentless no)
				    (synt-roles
				     ((by-obj ((np {^ ^ ^ participants agent}))))))
				   ((agentless yes)
				    (synt-roles
				     ((by-obj none))))))))

			   ;; end of agentive-yes
			   )

			  ((process ((agentive no)
				     (effective yes)
				     (mode attributive))) ;; no equative allowed 
			   (participants ((agent none)
					  (carrier ((function carrier)))))
			   (alt composite-effect-type2 (:index (process effect-type))
				(((process ((effect-type dispositive)))
				  ;; Structure is Af
				  (innermost-role {^ participants affected})
				  (participants ((carrier {^ affected})
						 (affected given)
						 (affected ((function affected)))))
				  ;; Enumerate acceptable relation types
				  (alt af-relation (:index (process relation-type))
				       (
					;; Af/Ca+At:     the kettle boiled dry
					((process ((relation-type ascriptive)
						   (voice active)))
					 (participants 
					  ((attribute given)
					   (fset (affected carrier attribute))))
					 ;; Default verb is "turn"
					 (process ((alt (((lex "turn"))
							 ((lex given))))))
					 ;; VOICE
					 (synt-roles
					  ((fset (subject subj-comp))
					   (subject {^ ^ participants affected})
					   (subj-comp {^ ^ participants attribute}))))

					;; Af/Ca+Pos:    the boss received cash
					((process ((relation-type possessive)))
					 (participants
					  ((possessed given)
					   (possessed {^ attribute})
					   (possessor {^ carrier})
					   (fset (affected carrier attribute
							   possessor possessed))))
					 ;; Default verb is "get"
					 (process ((alt (((lex "get"))
							 ((lex given))))))
					 ;; VOICE
					 (alt af-pos-voice (:index (process voice))
					      (((process ((voice active)))
						(synt-roles
						 ((fset (subject object))
						  (subject {^ ^ participants affected})
						  (object {^ ^ participants possessed}))))
					       ((process ((voice passive)))
						(synt-roles
						 ((fset (subject by-obj))
						  (subject {^ ^ participants possessed})))
						(alt agless (:index agentless)
						     (((agentless no)
						       (synt-roles
							((by-obj ((np {^ ^ ^ participants affected}))))))
						      ((agentless yes)
						       (synt-roles
							((by-obj none))))))))))

			 
					;; Af/Ca+Loc:    the box fell on the floor
					((process ((relation-type locative)
						   (voice active)))
					 (participants
					  ((location given)
					   (location {^ attribute})
					   (located {^ carrier})
					   (fset (affected carrier attribute
							   located location))))
					 ;; Default verb is "move"
					 (process ((alt (((lex "move")
							  (subject-clause none))
							 ((lex given))))))
					 ;; VOICE
					 (synt-roles
					  ((fset (subject subj-comp))
					   (subject {^ ^ participants affected})
					   (subj-comp {^ ^ participants location}))))

					)))

				 ((process ((effect-type creative)))
				  ;; Structure is Cr
				  (innermost-role {^ participants created})
				  (participants ((carrier {^ created})
						 (created given)
						 (created ((function created)))))
				  ;; Enumerate acceptable relation types
				  (alt cr-relation (:index (process relation-type))
				       (
					;; Cr/Ca+At:     The window popped wide
					((process ((relation-type ascriptive)
						   (voice active)))
					 (participants 
					  ((attribute given)
					   (fset (created carrier attribute))))
					 (process ((lex given)))
					 ;; VOICE
					 (synt-roles
					  ((fset (subject subj-comp))
					   (subject {^ ^ participants created})
					   (subj-comp {^ ^ participants attribute}))))

					;; Cr/Ca+Loc:    the window popped on the screen
					((process ((relation-type locative)
						   (voice active)))
					 (participants
					  ((location given)
					   (location {^ attribute})
					   (located {^ carrier})
					   (fset (created carrier attribute 
							  located location))))
					 (process ((lex given)))
					 ;; VOICE
					 (synt-roles
					  ((fset (subject subj-comp))
					   (subject {^ ^ participants created})
					   (subj-comp {^ ^ participants location}))))
					)))

				 ;; end of agentive-no
				 )))

			  ;; end of composite
			  )))

		   ;; end of transitivity system
		   ))

      
	     ;; CHECK CAT OF EACH SYNTACTIC ROLE

	     ;; special treatment of subject clauses (this is the right time
	     ;; because the subject is now bound). 
	     (alt subject-mood	(:index mood) 
		  (:demo "Is a subject required or does it need a special treatment?")
		  (((mood finite)
		    (synt-roles ((subject given))))
		   ((mood non-finite)
		    (alt subject-mood-nf (:index mood)
			 (((mood infinitive)
			   (synt-roles 
			    ((opt INF-SUB 
				  ((subject given)
				   (subject ((cat np) (syntax ((case objective)))))))))
			   (syntax 
			    ((opt ((case given) 
				   (case purposive) 
				   ({^ pattern} (in-order dots))))))
			   ;; When the clause is subject or purpose, and there is a subject,
			   ;; use a FOR-clause as in "for her to do it is a bold
			   ;; statement" or "in order for him to eat, we must cook"
			   (alt keep-for
				(:demo "Should we use a for in front of the subject?")
				(((keep-for yes)
				  (syntax ((case ((alt (subjective purposive))))))
				  (synt-roles
				   ((subject given)
				    (subject ((gap none)))))
				  (pattern (dots for start dots))
				  (for ((cat conj) (lex "for"))))
				 ((keep-for no)
				  (synt-roles ((subject ((gap yes)))))))))

			  ((mood present-participle)
			   ;; subject is optional or in possessive form
			   (synt-roles
			    ((alt subject-subject (:index subject)
				  (((subject none))
				   ((subject given)
				    (subject ((cat np)
					      (syntax ((case possessive))))))
				   ((subject ((gap yes)))))))))
			  ((mood past-participle)
			   (synt-roles ((subject none))))
			  ((mood imperative)
			   ;; subject is optional in realization
			   (synt-roles ((alt (((subject none))
					      ((subject ((gap yes))))))))))))))


	     ;; Syntactic categories of subject/object are compatible with verb?
	     ;; This depends on particular verb, information we will get from the
	     ;; lexicon. So far, we check whether subject-clause and object-clause
	     ;; are ok. 
	     ;; ***** SHOULD BE EXTENDED TO INCLUDE A FULL SUBCAT TREATMENT
	     ;; ***** or something like Melcuk's government.
	     ;; SUBJECT CAT = NP, CLAUSE.
	     (alt subject-subcat (:index (synt-roles subject cat))
		  (((synt-roles ((subject none))))
		   ((synt-roles ((subject ((cat np))))))
		   ((synt-roles ((subject ((cat clause)))))
		    (alt subject-clause	(:index (process subject-clause))
			 (:demo "For clausal subjects, what type of clause ~
                     must be used?")
			 (((process ((subject-clause infinitive)))
			   (synt-roles ((subject ((mood {^ ^ ^ process subject-clause}))))))
			  ((process ((subject-clause present-participle)))
			   (synt-roles ((subject ((mood {^ ^ ^ process subject-clause}))))))
			  ((process ((subject-clause that)))
			   (synt-roles ((subject ((mood bound)
						  (binder ((lex "that")))))))))))
		   ((synt-roles ((subject ((cat list))))))))
      
	     ;; OBJECT CAT = NP, CLAUSE
	     (alt object-subcat (:index  (synt-roles object cat))
		  (((synt-roles ((object none))))
		   ((synt-roles ((object ((cat np))))))
		   ((synt-roles ((object ((cat clause)))))
		    (alt object-clause (:index (process object-clause))
			 (:demo "For clausal objects, what type of clause ~
                    must be used?")	  
			 (((process ((object-clause infinitive)))
			   (synt-roles ((object ((mood infinitive))))))
			  ((process ((object-clause present-participle)))
			   (synt-roles ((object ((mood present-participle))))))
			  ((process ((object-clause that)))
			   (synt-roles ((object ((mood bound)
						 (binder ((lex "that")))))))))))
		   ((synt-roles ((object ((cat list))))))))

	     ;; SUBJ-COMP CAT = NP, AP, PP, ADV
	     (synt-roles ((subj-comp ((alt subj-comp-cat (:index (subj-comp cat))
					   (none
					    ((cat ap))
					    ((cat np))
					    ((cat pp))
					    ((cat list))
					    ((cat adv))))))))

	     ;; OBJ-COMP CAT = NP, AP, PP, ADV
	     (synt-roles ((obj-comp ((alt obj-comp-cat (:index (obj-comp cat))
					  (none
					   ((cat ap))
					   ((cat np))
					   ((cat pp))
					   ((cat list))
					   ((cat adv))))))))

	     ;; BY-OBJ CAT = PP, set prep
	     (synt-roles ((by-obj ((alt by-obj-cat (:index (by-obj cat))
					(none
					 ((cat pp)
					  (prep ((lex "by")))
					  (np ((cat np))))))))))

	     ;; DATIVE CAT = PP, set prep
	     (synt-roles 
	      ((dative ((alt dative-cat (:index (dative cat))
			     (none
			      ((cat pp)
			       ({^ ^ process dative-prep} given)
			       (prep ((lex {^ ^ ^ ^ process dative-prep}))))
			      ((cat pp)
			       (prep ((lex "to"))))))))))



	     ;; END OF TREATMENT OF INHERENT PARTICIPANTS


	     ;; START OF TREATMENT OF CIRCUMSTANCIAL PARTICIPANTS
	     ;; OPTIONAL CASES:
	     ;; These cases can occur with all process-types.
	     ;; They handle "circumstances".
	     ;; All have the same structure.
	     ;; Order in pattern should be studied with care. I have now a
	     ;; standard order. 
	     ;; All roles are mapped to corresponding syntactic complements.
	     ;; (eg, at-loc -> at-loc-comp).
	     ;; ***** Would be useful to embed more and have one constituent
	     ;; inherent-roles and one circumstancial-roles with an FSET in each.
	     ;; CAREFUL that most are inherent participants in the context of a
	     ;; relational clause (eg, "The meeting is at 9" - "at 9" is not an
	     ;; at-time circumstance, it is an attribute of the temporal
	     ;; clause.)
	     ;; ***** Should list criteria for each role and work on a more
	     ;; exhaustive list of roles.  Relate this list to relational
	     ;; processes. 
	     ;;
	     ;; circums are generally received as "np" type and mapped to "pp" complements.
	     ;; other options are: clause circums (reason, purpose, behalf)
	     ;; and adverbial circums (at-time and at-loc, to-loc)
	     ;; 
	     (circum (
		      (alt at-loc
			   (demo-message "Is there an at-loc role?")
			   (((at-loc none))
			    ((at-loc ((cat #(under adv)))) ;; here
			     (at-loc-comp {^ at-loc}))
			    ((at-loc ((cat #(under np))))
			     ;; get prep from role if given, otw from verb, otw default.
			     (opt ((at-loc ((prep given)))
				   (at-loc-comp ((prep ((lex {^3 at-loc prep})))))))
			     (opt (({^ verb at-loc-prep} given)
				   (at-loc-comp ((prep ((lex {^4 verb at-loc-prep})))))))
			     (at-loc-comp ((cat pp)
					   (opt ((prep ((lex "at")))))
					   (np {^ ^ at-loc}))))))

		      (alt to-loc
			   (demo-message "Is there a to-loc role?")
			   (((to-loc none))
			    ((at-loc ((cat #(under adv))))  ;; there
			     (at-loc-comp {^ at-loc}))
			    ((to-loc ((cat #(under np))))
			     ;; get prep from role if given, otw from verb, otw default.
			     (opt ((to-loc ((prep given)))
				   (to-loc-comp ((prep ((lex {^3 to-loc prep})))))))
			     (opt (({^ verb to-loc-prep} given)
				   (to-loc-comp ((prep ((lex {^4 verb to-loc-prep})))))))
			     (to-loc-comp ((cat pp)
					   (opt ((prep ((lex "to")))))
					   (np {^ ^ to-loc}))))))

		      (alt from-loc 
			   (demo-message "Is there a from-loc role?")
			   (((from-loc none))
			    ((from-loc given)
			     (opt ((from-loc ((prep given)))
				   (from-loc-comp ((prep ((lex {^3 from-loc prep})))))))
			     (opt (({^ verb from-loc-prep} given)
				   (from-loc-comp 
				    ((prep ((lex {^4 verb from-loc-prep})))))))
			     (from-loc-comp ((cat pp)
					     (opt ((prep ((lex "from")))))
					     (np {^ ^ from-loc}))))))
      
		      (alt on-loc
			   (demo-message "Is there an on-loc role?")
			   (((on-loc none))
			    ((on-loc given)
			     (opt ((on-loc ((prep given)))
				   (on-loc-comp ((prep ((lex {^3 on-loc prep})))))))
			     (opt (({^ verb on-loc-prep} given)
				   (on-loc-comp ((prep ((lex {^4 verb on-loc-prep})))))))
			     (on-loc-comp ((cat pp)
					   (opt ((prep ((lex "on")))))
					   (np {^ ^ on-loc}))))))
      
		      (alt in-loc 
			   (demo-message "Is there an in-loc role?")
			   (((in-loc none))
			    ((in-loc given)
			     (opt ((in-loc ((prep given)))
				   (in-loc-comp ((prep ((lex {^3 in-loc prep})))))))
			     (opt (({^ verb in-loc-prep} given)
				   (in-loc-comp ((prep ((lex {^4 verb in-loc-prep})))))))
			     (in-loc-comp ((cat pp)
					   (opt ((prep ((lex "in")))))
					   (np {^ ^ in-loc}))))))
      
		      (alt instrument (demo-message "Is there an instrument role?")
			   (((instrument none))
			    ((instrument given)
			     (opt ((instrument ((prep given)))
				   (instrument-comp ((prep ((lex {^3 instrument prep})))))))
			     (opt 
			      (({^ verb instrument-prep} given)
			       (instrument-comp 
				((prep ((lex {^4 verb instrument-prep})))))))
			     (instrument-comp
			      ((cat pp)
			       (opt ((prep ((lex "with")))))
			       (np {^ ^ instrument}))))))
      
		      ;; Answer to "who/what with?"
		      (alt accompaniment (demo-message "Is there an accompaniment role?")
			   (((accompaniment none))
			    ((accompaniment given)
			     (opt 
			      ((accompaniment ((prep given)))
			       (accompaniment-comp ((prep ((lex {^3 accompaniment prep})))))))
			     (opt 
			      (({^ verb accompaniment-prep} given)
			       (accompaniment-comp 
				((prep ((lex {^4 verb accompaniment-prep})))))))
			     (accompaniment-comp
			      ((cat pp)
			       (opt ((prep ((lex "with")))))
			       (np {^ ^ accompaniment}))))))

		      ;; Answer to "how?"
		      ;; adverb (slowly)
		      ;; pp (with care)
		      ;; clause (by doing this) @@@Add binder to mood present-participle
		      (alt manner (demo-message "Is there a manner role?")
			   (((manner none))
			    ((manner given)
			     (alt manner-cat 
				  (((manner ((cat adv)))
				    (manner-comp {^ manner}))
				   ((manner ((cat #(under clause))
					     (mood present-participle)
					     (binder ((lex ((alt (none given "by"))))))))
				    (manner-comp {^ manner}))
				   ((manner ((cat #(under pp))))
				    (manner-comp {^ manner}))
				   ((manner ((cat np)))
				    (manner-comp ((cat pp)
						  (np {^2 manner})))
				    (alt manner-pp-prep
					 (((manner ((prep given)))
					   (manner-comp ((prep ((lex {^3 manner prep}))))))
					  ((manner-comp ((prep ((lex {^4 verb manner-prep})
								(lex given))))))
					  ((manner-comp ((prep ((lex "with"))))))))))))))
				    
		      ;; THREE CAUSE COMPLEMENTS (as by Hallyday): reason, purpose, behalf
		      ;; purpose: answer to "what for?"
		      (alt purpose (demo-message "Is there a purpose role?")
			   (((purpose none))
			    ((purpose given)
			     (purpose ((cat clause)
				       (mood infinitive)
				       (syntax ((case purposive)))
				       (punctuation ((before ",") (after ",")))
				       (opt ((keep-in-order yes) (in-order ((lex "in order") (cat conj)))))))
			     (purpose-comp {^ purpose}))
			    ((purpose given)
			     (purpose ((cat np)))
			     (opt ((purpose ((prep given)))
				   (purpose-comp ((prep ((lex {^3 purpose prep})))))))
			     (opt (({^ verb purpose-prep} given)
				   (purpose-comp 
				    ((prep ((lex {^4 verb purpose-prep})))))))
			     (purpose-comp 
			      ((cat pp)
			       (opt ((prep ((lex "for")))))
			       (np {^ ^ purpose}))))
			    ((purpose ((gap yes))))))
      
		      ;; reason: answer to "why? How?"
		      (alt reason (demo-message "Is there a reason role?")
			   (((reason none))
			    ((reason given)
			     (reason ((cat np)))
			     (opt ((reason ((prep given)))
				   (reason-comp ((prep ((lex {^ ^ ^ reason prep})))))))
			     (opt (({^ verb reason-prep} given)
				   (reason-comp 
				    ((prep ((lex {^4 verb reason-prep})))))))
			     (reason-comp 
			      ((cat pp)
			       (opt ((prep ((lex "because of")))))
			       (np {^ ^ reason}))))
			    ((reason given)
			     (reason-comp {^ reason})
			     (reason-comp ((cat clause)
					   (mood bound)
					   (binder ((lex "because"))))))
			    ((reason ((gap yes))))))

		      ;; behalf: answer to "who for?"
		      (alt behalf (demo-message "Is there a behalf role?")
			   (((behalf none))
			    ((behalf given)
			     (behalf ((cat np)))
			     (opt ((behalf ((prep given)))
				   (behalf-comp ((prep ((lex {^ ^ ^ behalf prep})))))))
			     (opt (({^ verb behalf-prep} given)
				   (behalf-comp 
				    ((prep ((lex {^4 verb behalf-prep})))))))
			     (behalf-comp 
			      ((cat pp)
			       (opt ((prep ((lex "for")))))
			       (np {^ ^ behalf}))))
			    ((behalf given)
			     (behalf ((cat clause)
				      (mood infinitive)))
			     (behalf-comp {^ behalf}))
			    ;; behalf as a for-to infinitive clause
			    ;; Note: subject must be given and is actually the behalf
			    ;; "You have to do it for John to read" (Winograd p.472)
			    ((behalf ((gap yes))))))
      

		      ;; All the possible time roles under one at-time plus a time-type
		      ;; feature sub-categorizing it.  This means I can have only one time
		      ;; role per clause.
		      ;; The list of time-type is given in Quirk 11.27
		      ;; Ex: in the afternoon, later, when I have time, last Thursday
		      ;; ***** Should implement the semantics of time-type with tpattern.
		      (alt at-time
			   (demo-message "Is there an at-time role?")
			   (((at-time none))
			    ((at-time given)
			     (alt at-time 
				  (:index (at-time cat))
				  (((at-time ((cat adv)))
				    (at-time-comp {^ at-time}))
				   ((at-time  ((cat clause)
					       (mood bound)
					       (binder ((lex {^ ^ time-type})))
					       (time-type ((alt ("after" "as" "before" "once" "since" "until" "when" "while" "now that"))))))
				    (at-time-comp {^ at-time}))
				   ((at-time ((cat np)))
				    (time-type ((alt ("at" "on" "in" "for" "before" "after" "since" "until"))))
				    (at-time-comp ((cat pp)
						   (prep ((lex {^ ^ ^ time-type})))
						   (np {^ ^ at-time})))))))
			    ((at-time ((gap yes))))))

		      ))

	     ;; END OF CIRCUMSTANTIAL ROLES TREATMENT


	     ;; time-relater are "first", "next", "then" occuring first in the
	     ;; clause.  Note that they are not movable when used in this relater
	     ;; sense.  So they are not just simply adverbials.
	     ;; ***** To re-do when complex clauses are implemented.
	     (alt time-relater (demo-message "Is there a time-relater?")
		  (((time-relater none))
		   ((time-relater given)
		    (time-relater ((cat adv)
				   (punctuation ((after ",")))))
		    (pattern (time-relater dots)))))
	     
	     ;; cond-relater is "if", "then" or "else" - this is used until we
	     ;; come up with a better treatment of complex clauses. *****
	     (alt cond-relater (demo-message "Is there a cond-relater?")
		  (((cond-relater none))
		   ((cond-relater given)
		    (cond-relater ((cat adv)))
		    (pattern (time-relater cond-relater dots)))))

	     ;; START OF GENERAL THINGS: ORDERING PLUS AGREEMENT
	     ;; General things: arrange syntactic roles together
	     ;; and do the agreements.
	     ;; The patterns are here of course.
      
	     ;; Focus first (change when add modifiers)
	     ;; This is an example of mergeable pattern - where an ordering
	     ;; constraint can have an effect on voice choice.
	     ;; Not delicate enough though... so instead use conflation below.
	     ;; (pattern ((* focus) dots))  
	     ;; (focus {^ subject})
      
	     ;; Number and person agreement (verb is process)
	     (verb ((cat verb-group)
		    (modality {^ ^ modality})
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
	     (alt particle (demo-message "Does the verb have a particle?")
		  (((verb ((particle none))))
		   ((verb ((particle given)))
		    (particle ((cat adv)
			       (lex {^ ^ verb particle}))))))

	     ;; Arrange order of complements
	     ;; start is a dummy constituent used only for the ordering
	     ;; constraints
	     ;; A single pattern will do
	     (pattern
	      (dots start {^ synt-roles subject} dots process
		    dots {^ synt-roles iobject} dots
		    {^ synt-roles object} dots particle dots
		    {^ synt-roles subj-comp}
		    {^ synt-roles obj-comp}
		    dots
		    {^ synt-roles by-obj}
		    {^ synt-roles dative}
		    dots))

	     ;; Case assignment
	     (synt-roles 
	      ((opt ((subject ((syntax ((case subjective)))))))
	       (opt ((object  ((syntax ((case objective)))))))
	       (opt ((iobject ((syntax ((case objective)))))))))

	     ;; Place optional roles
	     (alt purpose-fronted
		  (((pattern (dots {^ circum purpose-comp} dots start dots)))
		   ((pattern (dots start dots {^ circum purpose-comp} dots)))))
	     
	     (pattern (dots start dots process dots 
			    {^ circum manner-comp}
			    {^ circum accompaniment-comp}
			    {^ circum behalf-comp}{^ circum reason-comp}
			    {^ circum at-loc-comp} {^ circum from-loc-comp} {^ circum to-loc-comp}
			    {^ circum on-loc-comp} {^ circum in-loc-comp}
			    {^ circum at-time-comp} 
			    {^ circum instrument-comp}
			    dots))

	     )

	    ;;==============================================================
	    ;; 02 CAT VERB-GROUP -------------------------------------------
	    ;;==============================================================
	    ((cat simple-verb-group)
	     (generic-cat verb-group)
	     (complex none)
	     (interrogative ((alt (none yes-no wh)))) ;; default is not interrogative
	     (insistence ((alt (no yes)))) ;; default is not insistent

	     (alt verb-polarity (:index polarity)
		  (((polarity positive)
		    (notf none))
		   ((polarity #(under negative))
		    (notf ((lex "not")
			   (cat adv))))))
      
	     ;; the tensed feature may be the event or any of the auxs.
	     (tensed-feature ((person { ^ ^ person})
			      (number { ^ ^ number})
			      (ending { ^ ^ ending})))

	     ;; there is always an event verb -- its also called the main verb.
	     (event ((cat verb)
		     (lex { ^ ^ lex})))

	     ;; deal with modality: allow only one type or no modality at all
	     ;; default is no modality
	     (alt only-one-modal
		  (((epistemic-modality none)
		    (deontic-modality none))
		   ((epistemic-modality given)
		    (deontic-modality none))
		   ((deontic-modality given)
		    (epistemic-modality none))))

	     ;; now, code the tense-specific stuff
	     ;; ***** Done only for case of non-modal finite tense
	     (alt tense-selection (:index tense)
		  (demo-message "what is the tense?")
	

		  ;; SIMPLE TENSES
		  ( 
		   ;; tense 2, the default:
		   ;; I take the bus.[a]
		   ;; The bus is taken by me.[p]
		   ((tense present)
		    (tpattern ((:st  :equals :rt0)
			       (:st  :none   :rt1)
			       (:et  :none   :rt1)
			       (:rt0 :none   :rt1)))
		    ({^ verb} #(external aspect-choice-1))
		    (time-frame present)
		    (simple yes)
		    (verb-aspect root)
		    (tensed-feature ((tense present))))

		   ;; Tense 1: past
		   ;; I took the bus.[a]
		   ;; The bus was taken by me.[p]
		   ((tense past)
		    (tpattern ((:rt0 :precedes :st)
			       (:st  :none     :rt1)
			       (:et  :none     :rt1)
			       (:rt0 :none     :rt1)))
		    (simple yes)
		    (time-frame past)
		    ({^ verb} #(external aspect-choice-1))
		    (tensed-feature ((tense past))))

		   ;; tense 3
		   ;; I will take the bus.[a]
		   ;; The bus will be taken by me.[b]
		   ;; for the future tenses, the auxillary "will" is
		   ;; treated as a modal.  There is no tensed feature
		   ;; and no agreement is necessary.
		   ((tense future)
		    (tpattern ((:st  :precedes :rt0)
			       (:st  :none     :rt1)
			       (:et  :none     :rt1)
			       (:rt0 :none    :rt1)))
		    ({^ verb} #(external aspect-choice-1))
		    (simple no)
		    (time-frame future)
		    (verb-aspect root))

		   ;; tense 4
		   ;; I had taken the bus.(a)
		   ;; The bus had been taken by me.(p)
		   ((tense past-perfect)
		    (time-frame past)
		    (simple no)
		    (tpattern ((:rt1 :precedes :st)
			       (:rt0 :precedes :rt1)
			       (:st :none :rt2)
			       (:rt1 :none :rt2)
			       (:rt0 :none :rt2)
			       (:et :none :rt2)))
		    (first-verb {^ have-1})
		    (tensed-feature ((tense past)))
		    ({^ verb} #(external aspect-choice-1))
		    (verb-aspect past-participle))

		   ;; tense 5
		   ;; I have taken the bus
		   ;; The bus has been taken by me.
		   ((tense  present-perfect)
		    (time-frame present)
		    (simple no)
		    (tpattern ((:rt0 :precedes :rt1)
			       (:rt1 :equals :st)
			       (:st :none :rt2)
			       (:rt1 :none :rt2)
			       (:et :none :rt2)
			       (:rt0 :none :rt2)))
		    (first-verb {^ have-1})
		    (tensed-feature ((tense present)))
		    ({^ verb} #(external aspect-choice-1))
		    (verb-aspect past-participle))

		   ;; tense 6
		   ;; I will have taken the bus.[a]
		   ;; The bus will have been taken by me.[p]
		   ((tense future-perfect)
		    (simple no)
		    (time-frame future)
		    (tpattern ((:st :precedes :rt0)
			       (:rt0 :precedes :rt1)
			       (:st :none :rt2)
			       (:rt1 :none :rt2)
			       (:et :none :rt2)
			       (:rt0 :none :rt2)))
		    (aux ((lex "will")
			  (cat modal)))
		    (tensed-feature {^ aux})
		    (first-verb {^ aux})
		    (have-1 ((ending root)))
		    ({^ verb} #(external aspect-choice-1))
		    (verb-aspect past-participle))

		   ;; tense 7
		   ;; I was taking the bus.(a)
		   ;; The bus was being taken by me.(p)
		   ((tense past-progressive)
		    (simple no)
		    (time-frame past)
		    (tpattern ((:rt0 :precedes :st)
			       (:rt0 :includes :et)
			       (:st :none :rt1)
			       (:et :none :rt1)
			       (:rt0 :none :rt1)))
		    (aspect ((alt aspect-7 (event process))))
		    (be-1 ((tense past)))
		    (be-1 { ^ tensed-feature})
		    (verb-aspect present-participle))

		   ;; tense 8
		   ;; I am taking the bus.(a)
		   ;; The bus is being taken by me.(p)
		   ((tense present-progressive)
		    (simple no)
		    (time-frame present)
		    (tpattern ((:rt0 :precedes :st)
			       (:rt0 :includes :et)
			       (:st :none :rt1)
			       (:et :none :rt1)
			       (:rt0 :none :rt1)))
		    (aspect ((alt aspect-8 (event process))))
		    (be-1 ((tense present)))
		    (be-1 { ^ tensed-feature})
		    (verb-aspect present-participle))

		   ;; tense 9
		   ;; I will be taking the bus.(a)
		   ;; The bus will be being taken by me.(p)
		   ((tense future-progressive)
		    (simple no)
		    (time-frame future)
		    (tpattern ((:st :precedes :rt0)
			       (:rt0 :includes :et)
			       (:st :none :rt1)
			       (:et :none :rt1)
			       (:rt0 :none :rt1)))
		    (aspect ((alt aspect-9 (event process))))
		    (aux ((lex "will")
			  (cat modal)))
		    (be-1 ((ending root)))
		    (verb-aspect present-participle))
	 
		   ;; tense 10
		   ;; I was going to take the bus.[a]
		   ;; The bus was going to be taken by me.[p]
		   ((tense tense-10)
		    (time-frame past)
		    (tpattern ((:rt1 :precedes :st) (:rt1 :precedes :rt0)
			       (:rt1 :none :rt2) (:st :none :rt2)
			       (:et :none :rt2)))
		    (simple no)
		    ({^ verb} #(external aspect-choice-1))
		    (verb-aspect root)
		    (beg ((tense past)))
		    (beg { ^ tensed-feature}))
	 
		   ;; tense 11
		   ;; I am going to take the bus.[a]
		   ;; The bus is going to be taken by me.[p]
		   ((tense tense-11)
		    (time-frame present)
		    (tpattern ((:st :equals :rt1) (:rt1 :precedes :rt0)
			       (:st :none :rt2) (:et :none :rt2)
			       (:rt1 :none :rt2) (:rt0 :none :rt2)))
		    (simple no)
		    ({^ verb} #(external aspect-choice-1))
		    (verb-aspect root)
		    (beg ((tense present)))
		    (beg { ^ tensed-feature}))

		   ;; tense 12
		   ;; I will be going to take.[a]
		   ;; The bus will be going to be taken.[b]
		   ((tense tense-12)
		    (time-frame future)
		    (tpattern ((:st :precedes :rt1)
			       (:rt1 :precedes :rt0)
			       (:st :none :rt2) (:et :none :rt2)
			       (:rt1 :none :rt2) (:rt0 :none :rt2)))
		    (simple no)
		    ({^ verb} #(external aspect-choice-1))
		    (verb-aspect root)
		    (beg ((ending root)))
		    (aux ((lex "will")
			  (cat modal))))

		   ;; tense 13
		   ;; I was going to have taken the bus.[a]
		   ;; The bus was going to have been taken by me.[p]
		   ((tense tense-13)
		    (simple no)
		    (time-frame past)
		    (tpattern ((:rt1 :precedes :st) (:rt1 :precedes :rt0)
			       (:rt0 :precedes :rt2) (:st :none :rt3)
			       (:rt0 :none :rt3) (:rt1 :none :rt3)
			       (:rt2 :none :rt3) (:et :none :rt3)))
		    ({^ verb} #(external aspect-choice-1))
		    (beg ((tense past)))
		    (beg { ^ tensed-feature})
		    (have-2 ((ending root)))
		    (verb-aspect past-participle))

		   ;; tense 14
		   ;; I am going to have taken the bus.[a]
		   ;; The bus is going to have been taken by me.[p]
		   ((tense tense-14)
		    (simple no)
		    (time-frame present)
		    (tpattern ((:st :equals :rt1) (:rt1 :precedes :rt0)
			       (:rt0 :precedes :rt2) (:st :none :rt3)
			       (:rt0 :none :rt3) (:rt1 :none :rt3)
			       (:rt2 :none :rt3)))
		    ({^ verb} #(external aspect-choice-1))
		    (beg ((tense present)))
		    (beg { ^ tensed-feature})
		    (have-2 ((ending root)))
		    (verb-aspect past-participle))
	  
		   ;; tense 15
		   ;; I will be going to have taken the bus.[a]
		   ;; The bus will be going to have been taken by me.[p]
		   ((tense tense-15)
		    (simple no)
		    (time-frame future)
		    (tpattern ((:st :precedes :rt1) (:rt1 :precedes :rt0)
			       (:rt0 :precedes :rt2) (:st :none :rt3)
			       (:rt0 :none :rt3) (:rt1 :none :rt3)
			       (:rt2 :none :rt3)))
		    ({^ verb} #(external aspect-choice-1))
		    (aux ((lex "will") (cat modal)))
		    (beg ((ending root)))
		    (have-2 ((ending root)))
		    (verb-aspect past-participle))

		   ;; tense 16
		   ;; I had been taking the bus.[a]
		   ;; The bus had been being taken by me.[p]
		   ((tense tense-16)
		    (simple no)
		    (time-frame past)
		    (tpattern ((:rt1 :precedes :st) (:rt0 :precedes :rt1)
			       (:rt0 :during :et) (:st :none :rt2)
			       (:rt0 :none :rt2) (:rt1 :none :rt2)))
		    (aspect ((alt (process event))))
		    (have-1 ((tense past)))
		    (have-1 { ^ tensed-feature})
		    (be-1 ((ending past-participle)))
		    (verb-aspect present-participle))

		   ;; tense 17
		   ;; I have been taking the bus.[a]
		   ;; The bus has been being taken by me.[p]
		   ((tense tense-17)
		    (time-frame present)
		    (simple no)
		    (tpattern ((:rt1 :equals :st) (:rt0 :precedes :rt1)
			       (:rt0 :during :et) (:st :none :rt2)
			       (:rt0 :none :rt2) (:rt1 :none :rt2)))
		    (aspect ((alt (process event))))
		    (have-1 ((tense present)))
		    (have-1 { ^ tensed-feature})
		    (be-1 ((ending past-participle)))
		    (verb-aspect present-participle))

		   ;; tense 18
		   ;; I will have been taking the bus.[a]
		   ;; The bus will have been being taken by me.[p]
		   ((tense tense-18)
		    (time-frame future)
		    (simple no)
		    (tpattern ((:rt1 :precedes :st) (:rt0 :precedes :rt1)
			       (:rt0 :during :et) (:st :none :rt2)
			       (:rt0 :none :rt2) (:rt1 :none :rt2)))
		    (aspect ((alt (process event))))
		    (aux ((lex "will") (cat modal)))
		    (have-1 ((ending root)))
		    (be-1 ((ending past-participle)))
		    (verb-aspect present-participle))

		   ;; tense 19
		   ;; I was going to be taking the bus.[a]
		   ;; The bus was going to be being taking the bus.[p]
		   ((tense tense-19)
		    (simple no)
		    (time-frame past)
		    (tpattern ((:rt1 :precedes :st) (:rt1 :precedes :rt0)
			       (:rt0 :during :et) (:st :none :rt2)
			       (:rt0 :none :rt2) (:rt1 :none :rt2)
			       (:et :none :rt2)))
		    (aspect ((alt aspect-19 (process event))))
		    (beg ((tense past)))
		    (beg { ^ tensed-feature})
		    (be-1 ((ending root)))
		    (verb-aspect present-participle))

		   ;; tense 20
		   ;; I am going to be taking the bus.[a]
		   ;; The bus is going to be being taken by me.[p]
		   ((tense tense-20)
		    (time-frame present)
		    (simple no)
		    (tpattern ((:rt1 :equals :st) (:rt1 :precedes :rt0)
			       (:rt0 :during :et) (:st :none :rt2)
			       (:rt0 :none :rt2) (:rt1 :none :rt2)
			       (:et :none :rt2)))
		    (aspect ((alt aspect-20 (process event))))
		    (beg ((tense present)))
		    (beg { ^ tensed-feature})
		    (be-1 ((ending root)))
		    (verb-aspect present-participle))

		   ;; tense 21
		   ;; I will be going to be taking the bus.[a]
		   ;; The bus will be going to be being taken by me.[p]
		   ((tense tense-21)
		    (simple no)
		    (time-frame future)
		    (tpattern ((:st :precedes :rt1) (:rt1 :precedes :rt0)
			       (:rt0 :during :et) (:st :none :rt2)
			       (:rt0 :none :rt2) (:rt1 :none :rt2)
			       (:et :none :rt2)))
		    (aspect ((alt aspect-21 (process event))))
		    (aux ((lex "will") (cat modal)))
		    (beg ((ending root)))
		    (be-1 ((ending root)))
		    (verb-aspect present-participle))

		   ;; ***** NO SEMANTICS
		   ;; tense 22
		   ;; I had been going to take the bus.[a]
		   ;; The bus had been going to be taken by me.[p]
		   ((tense tense-22)
		    (simple no)
		    (time-frame past)
		    ;; (tpattern ((:rt0 :precedes :et)))
		    (have-1 ((tense past)))
		    (have-1 { ^ tensed-feature})
		    (beg ((ending past-participle)))
		    (verb-aspect root)
		    )

		   ;; ***** NO SEMANTICS
		   ;; tense 23
		   ;; I have been going to take the bus.[a]
		   ;; The bus has been going to be taken by me.[p]
		   ((tense tense-23)
		    (simple no)
		    (time-frame present)
		    ;; (tpattern ((:rt0 :precedes :et)))
		    (have-1 ((tense present)))
		    (have-1 { ^ tensed-feature})
		    (beg ((ending past-participle)))
		    (verb-aspect root)
		    )

		   ;; ***** NO SEMANTICS
		   ;; tense 24
		   ;; I will have been going to take the bus.[a]
		   ;; The bus will have been going to be taken by me.[p]
		   ((tense tense-24)
		    (simple no)
		    (time-frame future)
		    ;; (tpattern ((:rt0 :precedes :et)))
		    (aux ((lex "will") (cat modal)))
		    (have-1 ((ending root)))
		    (beg ((ending past-participle)))
		    (verb-aspect root)
		    )

		   ;; tense 25
		   ;; I had been going to have taken the bus.[a]
		   ;; The bus had been going to have been taken by me.[p]???
		   ((tense tense-25)
		    (simple no)
		    (time-frame past)
		    (tpattern ((:rt2 :precedes :st)
			       (:rt1 :precedes :rt2)
			       (:rt1 :precedes :rt0)
			       (:rt0 :precedes :rt3)))
		    ({^ verb} #(external aspect-choice-1))
		    (verb-aspect past-participle)
		    (have-1 ((tense past)))
		    (have-1 {^ tensed-feature})
		    (beg ((ending past-participle)))
		    (have-2 ((ending root))))

		   ;; tense 26
		   ;; I have been going to have taken the bus.[a]
		   ;; The bus has been going to have been taken by me.[p]???
		   ((tense tense-26)
		    (simple no)
		    (time-frame present)
		    (tpattern ((:rt2 :equals :st)
			       (:rt1 :precedes :rt2)
			       (:rt1 :precedes :rt0)
			       (:rt0 :precedes :rt3)))
		    ({^ verb} #(external aspect-choice-1))
		    (verb-aspect past-participle)
		    (have-1 ((tense present)))
		    (have-1 {^ tensed-feature})
		    (beg ((ending past-participle)))
		    (have-2 ((ending root))))

		   ;; tense 27
		   ;; I will have been going to have taken the bus.[a]
		   ;; The bus will have been going to have been taken by me.[p]???
		   ((tense tense-27)
		    (simple no)
		    (time-frame future)
		    (tpattern ((:st :precedes  :rt2)
			       (:rt1 :precedes :rt2)
			       (:rt1 :precedes :rt0)
			       (:rt0 :precedes :rt3)))
		    ({^ verb} #(external aspect-choice-1))
		    (verb-aspect past-participle)
		    (aux ((lex "will") (cat modal)))
		    (have-1 ((ending root)))
		    (beg ((ending past-participle)))
		    (have-2 ((ending root))))

		   ;; tense 28
		   ;; I was going to have been taking the bus.[a]
		   ;; The bus was going to have been being taken by me.[p]
		   ((tense tense-28)
		    (simple no)
		    (time-frame past)
		    (tpattern ((:rt1 :precedes :st) (:rt1 :precedes :rt0)
			       (:rt0 :precedes :rt2) (:rt0 :during :et)))
		    (aspect ((alt aspect-28 (event process))))
		    (tensed-feature {^ beg}) ;; first-verb
		    (tensed-feature ((tense past)))
		    (have-2 ((ending root)))
		    (be-1 ((ending past-participle)))
		    (verb-aspect present-participle))

		   ;; tense 29
		   ;; I am going to have been taking the bus.[a]
		   ;; The bus is going to have been being taken by me.[p]
		   ((tense tense-29)
		    (simple no)
		    (time-frame present)
		    (tpattern ((:rt1 :equals :st) (:rt1 :precedes :rt0)
			       (:rt0 :precedes :rt2) (:rt0 :during :et)))
		    (aspect ((alt aspect-29 (event process))))
		    (beg ((tense present)))
		    (beg { ^ tensed-feature})
		    (have-2 ((ending root)))
		    (be-1 ((ending past-participle)))
		    (verb-aspect present-participle))

		   ;; tense 30
		   ;; I will be going to have been taking the bus.[a]
		   ;; The bus will be going to have been being taken by me.[p]
		   ((tense tense-30)
		    (simple no)
		    (time-frame future)
		    (tpattern ((:st :precedes :rt1) (:rt1 :precedes :rt0)
			       (:rt0 :precedes :rt2) (:rt0 :during :et)))
		    (aspect ((alt aspect-30 (event process))))
		    (aux ((lex "will") (cat modal)))
		    (beg ((ending root)))
		    (have-2 ((ending root)))
		    (be-1 ((ending past-participle)))
		    (verb-aspect present-participle))

		   ;; tense 31
		   ;; I had been going to be taking the bus.[a]
		   ;; The bus had been going to be being taken by me.[p]
		   ((tense tense-31)
		    (simple no)
		    (time-frame past)
		    (tpattern ((:rt2 :precedes :st) (:rt1 :precedes :rt2)
			       (:rt1 :precedes :rt0) (:rt0 :during :et)
			       (:st :none :rt3) (:rt0 :none :rt3) (:rt1 :none :rt3)
			       (:rt2 :none :rt3) (:et :none :rt3)))
		    (aspect ((alt (process event))))
		    (have-1 ((tense past)))
		    (have-1 { ^ tensed-feature})
		    (beg ((ending past-participle)))
		    (be-1 ((ending root)))
		    (verb-aspect present-participle))

		   ;; tense 32
		   ;; I have been going to be taking the bus.[a]
		   ;; The bus has been going to be being taken by me.[p]
		   ((tense tense-32)
		    (simple no)
		    (time-frame present)
		    (tpattern ((:rt2 :equals :st) (:rt1 :precedes :rt2)
			       (:rt1 :precedes :rt0) (:rt0 :during :et)
			       (:st :none :rt3) (:rt0 :none :rt3) (:rt1 :none :rt3)
			       (:rt2 :none :rt3) (:et :none :rt3)))
		    (aspect ((alt (process event))))
		    (have-1 ((tense present)))
		    (have-1 { ^ tensed-feature})
		    (beg ((ending past-participle)))
		    (be-1 ((ending root)))
		    (verb-aspect present-participle))

		   ;; tense 33
		   ;; I will have been going to be taking the bus.[a]
		   ;; The bus will have been going to be being taken by me.[p]
		   ((tense tense-33)
		    (simple no)
		    (time-frame future)
		    (tpattern ((:st :precedes :rt1) (:rt1 :precedes :rt2)
			       (:rt1 :precedes :rt0) (:rt0 :during :et)
			       (:st :none :rt3) (:rt0 :none :rt3) (:rt1 :none :rt3)
			       (:rt2 :none :rt3) (:et :none :rt3)))
		    (aspect ((alt (process event))))
		    (aux ((lex "will") (cat modal)))
		    (have-1 ((ending root)))
		    (beg ((ending past-participle)))
		    (be-1 ((ending root)))
		    (verb-aspect present-participle))
	 
		   ;; tense 34
		   ;; I had been going to have been taking the bus.[a]
		   ;; The bus had been going to have been being taken by me.[p]
		   ((tense tense-34)
		    (simple no)
		    (time-frame past)
		    (tpattern ((:rt2 :precedes :st)
			       (:rt1 :precedes :rt2)
			       (:rt1 :precedes :rt0)
			       (:rt0 :precedes :rt3)
			       (:rt0 :during :et)))
		    (aspect ((alt (process event))))
		    (have-1 ((tense past)))
		    (have-1 { ^ tensed-feature})
		    (have-2 ((ending root)))
		    (beg ((ending past-participle)))
		    (be-1 ((ending past-participle)))
		    (verb-aspect present-participle))

		   ;; tense 35
		   ;; I have been going to have been taking the bus.[a]
		   ;; The bus has been going to have been being taken by me.[p]
		   ((tense tense-35)
		    (simple no)
		    (time-frame present)
		    (tpattern ((:st :equals :rt2)
			       (:rt1 :precedes :rt2)
			       (:rt1 :precedes :rt0)
			       (:rt0 :precedes :rt3)
			       (:rt0 :during :et)))
		    (aspect ((alt (process event))))
		    (have-1 ((tense present)))
		    (have-1 { ^ tensed-feature})
		    (have-2 ((ending root)))
		    (beg ((ending past-participle)))
		    (be-1 ((ending past-participle)))
		    (verb-aspect present-participle))

		   ;; tense 36
		   ;; I will have been going to have been taking the bus.[a]
		   ;; The bus will have been going to have been being taken by me.[p]
		   ((tense tense-36)
		    (simple no)
		    (time-frame future)
		    (tpattern ((:st :precedes :rt1)
			       (:rt1 :precedes :rt2)
			       (:rt1 :precedes :rt0)
			       (:rt0 :precedes :rt3)
			       (:rt0 :during :et)))
		    (aspect ((alt (process event))))
		    (aux ((lex "will") (cat modal)))
		    (have-1 ((ending root)))
		    (have-2 ((ending root)))
		    (beg ((ending past-participle)))
		    (be-1 ((ending past-participle)))
		    (verb-aspect present-participle))
		   ))

	     (alt modality
		  (demo-message "what modality is used w/ this verb?")
		  (
		   ((epistemic-modality none)
		    (deontic-modality none)
		    %TRACE-OFF%
		    (control (control-demo "No modality in this clause."))
		    %TRACE-ON%
		    (tensed-feature {^ first-verb})
		    (alt time-frame (:index time-frame)
			 (((time-frame present))
			  ((time-frame past))
			  ((time-frame future)
			   (aux ((lex "will") (cat modal))))
			  )))
		   ((epistemic-modality fact)
		    (tensed-feature {^ first-verb})
		    (alt (:index time-frame)
			 (((time-frame present))
			  ((time-frame past))
			  ((time-frame future)
			   (aux ((lex "will") (cat modal))))
			  )))
		   ((epistemic-modality inference)
		    (first-verb ((ending root)))
		    (alt (:index time-frame)
			 (((time-frame present)
			   (aux ((lex "must") (cat modal))))
			  ((time-frame future)
			   ;; there is already a will 
			   )
			  ((time-frame past)
			   (aux ((lex "must've") (cat modal))))
			  )))
		   ((epistemic-modality possible)
		    (first-verb ((ending root)))
		    (alt (:index time-frame)
			 (((time-frame present)
			   (aux ((lex "can") (cat modal))))
			  ((time-frame future)
			   (aux ((lex "can") (cat modal))))
			  ((time-frame past)
			   (aux ((lex "could have")))))))
		   ((epistemic-modality given)
		    (first-verb ((ending root)))
		    (aux ((cat modal)
			  (lex {^ ^ epistemic-modality}))))
		   ((deontic-modality duty)
		    (first-verb ((ending root)))
		    (alt (:index time-frame)
			 (((time-frame present)
			   (aux ((lex "must") (cat modal))))
			  ((time-frame future)
			   (aux ((lex "must") (cat modal))))
			  ((time-frame past)
			   (aux ((lex "must've") (cat modal))))
			  )))
		   ((deontic-modality given)
		    (first-verb ((ending root)))
		    (aux ((cat modal)
			  (lex {^ ^ deontic-modality}))))
		   ((modality given)
		    (first-verb ((ending root)))
		    (aux ((cat modal)
			  (lex {^ ^ modality}))))
		   ))
	
      ;;; Now, the tense feature should be selected.  Deal
      ;;; with voice, interrogative and polarity.
      
	     (alt voice-1 (:index voice) 
		  (
		   ;; First: very special case of "to be"
		   ;; passive is "to be" - no auxiliary for negation and question
		   ((event ((lex "be")))
		    (alt simple-be (:index simple)
			 ;; for simple tenses, don't add "do"
			 (((simple yes)
			   (event {^ first-verb}))
			  ((simple no)
			   (event ((ending {^ ^ verb-aspect})))))))
		   ;; then special case of all copulae: passive is themselves
		   ;; and all other verbs at active
		   ((alt (((voice active))
			  ((copula #(under yes)))))
		    (alt simple-do (:index simple)
			 (
			  ;; For simple tenses, the auxillary "do" must be added
			  ;; e.g. I do not open the door
			  ;; or   Did you open the door?
			  ((simple yes)
			   (alt simple-polarity (:index polarity)
				(
				 ((polarity negative) ;; e.g. I did not see it.
				  (aux ((lex "do") (cat verb)))
				  (aux {^ tensed-feature})
				  (event ((ending root))))
				 ((polarity positive) ;; e.g. I saw it
				  (alt simple-interrogative (:index interrogative)
				       (( ;; When wh question where scope is subject don't use
					 ;; aux. 
					 (interrogative wh)
					 ({^ synt-roles subject} {^ ^ ^ scope})
					 (aux none)
					 (event { ^ tensed-feature}))
					((interrogative interrogative)
					 (aux ((lex "do") (cat verb)))
					 (aux {^ tensed-feature})
					 (event ((ending root))))
					((alt simple-insistence (:index insistence)
					      (((insistence no)
						(aux none)
						(event { ^ tensed-feature}))
					       ((insistence yes)
						(aux ((lex "do") (cat verb)))
						(aux {^ tensed-feature})
						(event ((ending root)))))))
					))
				  ))
				))
			  ((simple no)
			   ;; there is an aux of one kind or another,
			   ;; the default ending of the event should be
			   ;; set event ending???
			   (event ((ending {^ ^ verb-aspect})))))
			 ))
		   ((voice passive)
		    (copula no)
		    (alt passive-simple (:index simple)
			 ( ;; no auxilliary necessarily
			  ((simple yes)
			   (event ((ending past-participle)))
			   (be-2 { ^ tensed-feature}))
			  ;; there's an auxilliary
			  ((simple no)
			   (event ((ending past-participle)))
			   (be-2 ((ending { ^ ^ verb-aspect})))))  ))
		   ))

	     ;; ***** WORK ON NEGATION OF NON-FINITE
	     (alt polarity-notf-placement (:index polarity) 
		  (((polarity positive))
		   ((polarity negative)
		    (alt aux-notf (:index aux)
			 (((aux given)
			   (pattern (dots aux notf dots)))
			  ((aux none)
			   (alt have-1-notf (:index have-1)
				(((have-1 given)
				  (pattern (dots have-1 notf dots)))
				 ((have-1 none)
				  (alt beg-notf (:index beg)
				       (((beg given)
					 (pattern (dots beg notf dots)))
					((beg none)
					 (alt have-2-notf (:index have-2)
					      (((have-2 given)
						(pattern (dots have-2 notf dots)))
					       ((have-2 none)
						(alt be-1-notf (:index be-1)
						     (((be-1 given)
						       (pattern (dots be-1 notf dots)))
						      ((be-1 none)
						       (alt be-2-notf (:index be-2)
							    (((be-2 given)
							      (pattern (dots be-2 notf dots)))
							     ((be-2 none)
							      ;; there are no aux'es but the polarity
							      ;; is negative.  It must be the "be"
							      ;; special case: I am not a tree.
							      (pattern (dots event notf dots))))
							    )))
						     )))
					      )))
				       )))
				)))
			 )))
		  )

	     ;; Now fill the slots for auxiliaries if necessary
	     (alt (((be-1 none))
		   ((be-1 ((lex "be")
			   (cat verb))))))

	     (alt (((be-2 none))
		   ((be-2 ((lex "be")
			   (cat verb))))))
      
	     (alt (((have-1 none))
		   ((have-1 ((lex "have")
			     (cat verb))))))

	     (alt (((have-2 none))
		   ((have-2 ((lex "have")
			     (cat verb))))))

      
	     (alt (((beg none))
		   ((beg ((lex "be")
			  (cat verb)))
		    ;; we need to add the going to which is never tensed.  So,
		    ;; make it a cat modal.
		    (going-to ((lex "going to")
			       (cat modal))))))

	     ;; Put everything together.
	     ;; NOTE: an adverb could be added here but would need to be placed in
	     ;; a manner similar to the notf above (that means trouble...)
	     ;; For interrogative, the tensed-feature has been fronted, so
	     ;; don't put it here
	     (alt pattern-interrogative (:index interrogative)
		  (((interrogative none)
		    (pattern (aux dots have-1 dots beg dots going-to dots
				  have-2 dots be-1 dots be-2 dots event dots)))
		   ((interrogative interrogative)
		    (tensed-feature ((gap yes)))
		    ;; If tensed-feature not bound yet, it means aux should be
		    ;; fronted by the question.
		    (alt aux-or-tensed
			 (((tensed-feature ((lex given))))
			  ((tensed-feature {^ aux}))))
		    ;; Now is the time to give a cat to fronted-aux at clause level
		    ;; @@@NOTE: This is a non-local update -- 
		    ;; would be better to preset the fronted-aux to tensed-feature at clause-level.
		    (tensed-feature ((cat {^ ^ ^ fronted-aux cat})))

		    (alt notf-interrogative
			 (((notf none))
			  ((notf ((gap yes)
				  (lex {^ ^ ^ fronted-not lex})
				  (cat {^ ^ ^ fronted-not cat}))))))
		    (pattern (aux dots have-1 dots beg dots going-to dots
				  have-2 dots be-1 dots be-2 dots event dots))))))

     

	    ;;==============================================================
	    ;; 03 CAT NP ---------------------------------------------------
	    ;;==============================================================
	    ;; Prototypical sequence: determiner modifiers head qualifiers
	    ;; determiner = (pre-determiner determiner ordinal cardinal)
	    ;; modifiers  = (describer classifier)
	    ;; qualifiers = (restrictive non-restrictive possessive-marker)
	    ;; We expect in the input at the top-level constituents:
	    ;; - definite yes/no (default is yes).
	    ;; - a lex that will be interpreted as the lex of the head.
	    ;; - describer: an embedded list of describers.
	    ;; - classifier: an embedded list of classifiers.
	    ;; - qualifiers: an embedded list of qualifiers.
     
	    ((cat #(under np))
	     (complex none)
	     ;; GENERAL NP =================================================
	     ;; General structure: head, syntax, semantics, determiner.
	     (pattern (dots head dots))
	     (generic-cat np)
	     (syntax ((fset (animate gender case definite person
				     number a-an distance countable))
		      (animate {^ ^ animate})
		      (number {^ ^ number})
		      (gender {^ ^ gender})
		      (case   {^ ^ case})
		      (person {^ ^ person})
		      (definite {^ ^ definite})
		      (a-an {^ ^ a-an})
		      (distance {^ ^ distance})
		      (countable {^ ^ countable})))
	     (semantics ((fset (index describer qualifier classifier))
			 (index ((fset (animate gender person number countable))
				 (animate {^ ^ ^ animate})
				 (number {^ ^ ^ number})
				 (gender {^ ^ ^ gender})
				 (person {^ ^ ^ person})
				 (countable {^ ^ ^ countable})))
			 (describer {^ ^ describer})
			 (qualifier {^ ^ qualifier})
			 (classifier {^ ^ classifier})))
	     (head ((lex {^ ^ lex})))

	     ;; NP-TYPE: Pronouns, common, proper =========================
	     (alt np-type 
		  (:index cat)
		  (demo-message "Is this a common noun, a pronoun or a proper noun?")
		  ;; COMMON NOUNS -------------------------------------------
		  ;; Only common nouns can have determiners.
		  (((cat common)
		    (np-type common)
		    (head ((cat noun)
			   ;; Single head or conjunction of nouns?
			   (alt  common-head
				 (((lex given)
				   ;; these only accepted by morphology.
				   (fset (cat generic-cat lex number a-an feature)))
				  ((complex given)
				   ;; to make morphology understand this is not a
				   ;; simple noun.
				   (cat noun)
				   (common ((cat noun))))
				  ((gap given))))
			   (number {^ ^ syntax number})
			   (a-an {^ ^ syntax a-an})))
		    (person third)
		    (definite ((alt (yes no))))
		    (countable ((alt (yes no))))
		    (pattern (determiner dots))
		    (determiner ((cat det)
				 (syntax ((definite  {^ ^ ^ syntax definite})
					  (countable {^ ^ ^ syntax countable})
					  (number    {^ ^ ^ syntax number})
					  (distance  {^ ^ ^ syntax distance}))))))
	
		   ;; PRONOUNS ------------------------------------------------
		   ((cat pronp)
		    (np-type pronp)
		    ;; pronouns allow no classifier, no determiner, 
		    ;; all except quantified have no describer.
		    ;; can have qualifiers
		    (semantics ((classifier none)))
		    (determiner none)
		    (head ((cat pronoun) ;; known to morphology plus its args.
			   (fset (cat pronoun-type case gender animate feature 
				      syntax lex number distance person restrictive))
			   (case {^ ^ syntax case})
			   (gender {^ ^ syntax gender})
			   (number {^ ^ syntax number})
			   (animate {^ ^ syntax animate})
			   (distance {^ ^ syntax distance})
			   (person {^ ^ syntax person})
			   (restrictive {^ ^ restrictive})))
		    (pattern (head dots))
		    ;; Pronoun-type: personal, demonstrative, question, quantified
		    ;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		    (alt pronoun-type 
			 (:index cat)
			 (demo-message "Is this a personal, demonstrative, question ~
                          or quantified pronoun?")
			 (((cat personal-pronoun)
			   (np-type personal-pronoun)
			   ;; are gender and person specific, default is third masculine.
			   (head ((pronoun-type personal))) ;; arg to morph.
			   (syntax
			    ((alt (:index person)
				  (((person third))
				   ((person first) (animate yes))
				   ((person second) (animate yes))))
			     (alt gender
				  (:index gender)
				  (((gender neuter) (animate no))
				   ((gender masculine) (animate yes))
				   ((gender feminine) (animate yes))))))
			   (definite yes)
			   (semantics ((describer none))))
			  ((cat demonstrative-pronoun)
			   (np-type demonstrative-pronoun)
			   (head ((pronoun-type demonstrative)))
			   (syntax ((definite yes)
				    (person ((opt third)))
				    (distance ((alt (far near))))))
			   (semantics ((describer none))))
			  ((cat relative-pronoun)
			   (np-type relative-pronoun)
			   (syntax ((person ((opt third)))))
			   (head ((pronoun-type relative))))
			  ((cat question-pronoun)
			   (np-type question-pronoun)
			   (head ((pronoun-type question)))
			   (syntax ((person ((opt third)))))         ;; ---> when we question a first-person
			   (semantics ((describer none))))
			  ;; - describers come after head: "something green"
			  ((cat quantified-pronoun)
			   (np-type quantified-pronoun)
			   (head ((pronoun-type quantified)))
			   (syntax ((person third) (definite no)))
			   (alt (((semantics ((describer none))))
				 ((pattern (head describer dots))))))
			  ))
		    ;; Case: subjective, objective, possessive, reflexive
		    ;; - - - - - - - - - - - - - - - - - - - - - - - - -
		    (syntax 
		     ((alt pronoun-case 
			   (demo-message "Is the pronoun subject, object, possessive ~
                            or reflexive?")
			   (((case given)
			     (case ((alt (subjective objective possessive reflexive)))))
			    ((case subjective)))))))

		   ;; Proper nouns -------------------------------------------
		   ((cat proper)
		    (np-type proper)
		    (head ((cat noun)
			   (lex given)
			   ;; Only the ones accepted by morphology + the generic
			   (fset (cat generic-cat lex number a-an feature))
			   (number {^ ^ syntax number})
			   (a-an {^ ^ syntax a-an})))
		    (pattern (head))
		    (syntax ((person third)
			     (definite yes)))
		    (semantics ((describer none)
				(classifier none)
				(qualifier none))))))

	     ;; NUMBER, CARDINAL and ORDINAL ==============================

	     ;; Only add cardinal to determiner if det is not none
	     (opt ((cardinal given)
		   (determiner any) ;; for not-none
		   (determiner ((cardinal {^ ^ cardinal})))))

	     ;; Only add ordinal to determiner if det is not none
	     (opt ((ordinal given)
		   (determiner any) ;; for not-none
		   (determiner ((ordinal {^ ^ ordinal})))))

	     ;; If cardinal is given, use its value to determine number.
	     (alt cardinal-number
		  (((cardinal ((value given)))
		    (control (and (numberp #@{^ cardinal value})
				  (= #@{^ cardinal value} 1)))
		    (syntax ((number singular))))
		   ;; any other number is plural
		   ((cardinal ((value given)))
		    (control (numberp #@{^ cardinal value}))
		    (syntax ((number plural))))
		   ((syntax ((number singular))))
		   ((syntax ((number plural))))))


	     ;; DETERMINER ================================================
	     ;; Check if possessor is defined here - it becomes the determiner
	     (opt ((possessor given)
		   (possessor ((syntax ((case possessive)))))
		   (determiner ((possessor {^ ^ possessor})
				(cat possessive-det)))))


	     ;; DESCRIBER =================================================
	     ;; Now allow for multiple describers (cat list)
	     ;; ***** "Soon to be released book" "man eating tiger"
	     (semantics
	      ((alt describer
		    (demo-message "Are there describers?")
		    (((describer none))
		     ((describer given)
		      ({^ pattern} (dots describer dots head dots))
		      (describer
		       ((alt describer-cat (:index cat)
			     (((cat adj)) ;; either a single adj
			      ((cat ap)	;; or an ap with no modifiers
			       (describer none)
			       (qualifier none))
			      ((cat list)) ;; no check on lists
			      ((cat verb)
			       (ending past-participle)
			       (modifier-type objective))
			      ((cat verb)
			       (ending present-participle)
			       (modifier-type subjective)))))))))))

	     ;; CLASSIFIER =================================================
	     ;; Allow for multiple classifiers (complex nouns)
	     (semantics
	      ((alt classifier 
		    (demo-message "Is there a classifier?")
		    (((classifier none))
		     ((classifier given)
		      ({^ pattern} (dots classifier head dots))
		      (classifier
		       ((alt (:index cat)
			     (((cat noun))
			      ((cat list)) ;; no check on lists
			      ((cat verb)
			       (ending present-participle)
			       (modifier-type subjective)))))))))))

	     ;; QUALIFIER ==================================================
	     (semantics
	      ((alt qualifier
		    (demo-message "Is there a qualifier? Is it a PP or ~
                         a relative clause?")
		    (((qualifier none))
		     ((qualifier given)
		      ({^ pattern} (dots head qualifier))
		      (qualifier
		       ((alt (:index cat)
			     (((cat pp)
			       (restrictive yes)
			       ;; send features of qualifier just to np of pp.
			       ;; This is messy - should be a different constituent.
			       ;; default prep is "of".
			       (opt ((prep ((lex "of")))))
			       (np ((syntax {^ ^ syntax})
				    (semantics {^ ^ semantics})
				    (lex {^ ^ lex}))))
			      ((cat list)) ;; an heterogeneous list of qualifiers
			      ;; "The elephant that came to brunch"
			      ;; "The game he played so wonderfully"
			      ((cat clause)
			       (mood relative))
			      ;; "The time remaining before the buzzer"
			      ((cat clause)
			       (subject none)
			       (mood present-participle))
			      ;; "The fare specified on the ticket"
			      ;; Debatable whether these are adjectives or
			      ;; passive-participles - tests in general are:
			      ;; If can add very - is an adjective
			      ;; If can change "is" by "seems" or "remains" - is an adj
			      ;; In general, want the ap to have a complement except if
			      ;; there is a reason why not.
			      ((cat ap))
			      ;; "The game to be played tomorrow"
			      ((cat clause)
			       (subject none)
			       (mood infinitive)))))))))))
		  

	     ;; POSSESSIVE MARKER ==========================================
	     (alt np-case
		  (:index (syntax case))
		  (demo-message "Is this a possessive NP?")
		  (((syntax ((case subjective))))
		   ((syntax ((case given)))
		    (opt np-case-possessive
			 ((syntax ((case possessive)))
			  (semantics ((qualifier none)))
			  (head ((feature possessive))))))))
	     )

	    ;; ==============================================================
	    ;; 031 CAT NOUN
	    ;; ==============================================================
	    ((cat noun)
	     (generic-cat noun)
	     (complex none))

	    ((cat adj)
	     (generic-cat adj)
	     (complex none))

	    ;; ==============================================================
	    ;; 04 CAT AP : for adjectival phrases ---------------------------
	    ;; ==============================================================
	    ;; ***** Do comparative constructs here
	    ((cat simple-ap)
	     (complex none)
	     (generic-cat ap)
	     (head ((cat adj)
		    (lex {^ ^ lex})))
	     ;; "light blue" (light is the describer)
	     (classifier ((alt (none ((cat adj)) ((cat noun))))))
	     ;; "visible in the cutaway view" (qualifier)
	     (qualifier ((alt (none ((cat pp))))))
	     (pattern (describer head qualifier)))
      
	    ;; ==============================================================
	    ;; 05 CAT PP : for prepositional phrases ------------------------
	    ;; ==============================================================
	    ((cat simple-pp)
	     %pp%
	     (complex none)
	     (generic-cat pp)
	     (pattern (prep np))
	     (prep ((cat prep) (lex given)))
	     (np ((cat np)))
	     %pp%
	     )

	    ;;==============================================================
	    ;; 06 CAT DET : for articles -----------------------------------
	    ;;==============================================================
	    ;; ***** No treatment of quantification, pre-det
	    ((cat det)
	     (pattern (head ordinal cardinal)) ;; the actual word sequence.
	     (complex none)
	     (generic-cat det) ;; for conjunctions

	     ;; Cardinal must be a simple number - don't check it
	     (alt (((cardinal none))
		   ((cardinal ((cat cardinal))))))

	     (alt (((ordinal none))
		   ((ordinal ((cat ordinal))))))

	     ;; Make implication distance ==> demonstrative
	     (opt DISTANCE-DET
		  ((distance {^ syntax distance})
		   (distance given)
		   (cat demonstrative-det)))

	     (alt det
		  (:index cat)
		  (demo-message 
		   "Is this a demonstrative, possessive or regular determiner?")
		  (((cat #(under demonstrative-det))
		    (syntax ((fset (definite countable number distance))
			     (definite yes)))
		    (head ((cat article))) ;; don't do anything on it at morphology.
		    (alt distance 
			 (:index (syntax distance))
			 (demo-message "Is this a reference to a near or to a far object?")
			 (((syntax ((distance near)))
			   (alt (:index (syntax number))
				(((syntax ((number singular)))
				  (head ((lex "this"))))
				 ((syntax ((number plural)))
				  (head ((lex "these")))))))
			  ((syntax ((distance far)))
			   (alt (:index (syntax number))
				(((syntax ((number singular)))
				  (head ((lex "that"))))
				 ((syntax ((number plural)))
				  (head ((lex "those"))))))))))
     
		   ;; Careful here on the meaning of number:
		   ;; distinguish between number of the possessive det and of the
		   ;; head of the np.  In [my first two years] my is singular and
		   ;; years is plural...  
		   ;; Features of the possessor det are under possessor (which is
		   ;; conflated with head).
		   ((cat #(under possessive-det))
		    (syntax ((definite yes)))
		    (possessor {^ head})
		    (alt possessive-det 
			 (:index (possessor cat))
			 (((cset ()) ;; don't recurse on possessor - it's taken
			   ;; care of already here
			   (possessor 
			    ((cat #(under pronp))
			     (alt (:index person)
				  (((person first)
				    (alt (:index number)
					 (((number singular)
					   (lex "my"))
					  ((number plural)
					   (lex "our")))))
				   ((person second)
				    (lex "your"))
				   ((person third)
				    (alt (:index number)
					 (((number singular)
					   (alt (:index gender)
						(((gender masculine)
						  (lex "his"))
						 ((gender feminine)
						  (lex "her"))
						 ((gender neuter)
						  (lex "its")))))
					  ((number plural)
					   (lex "their"))))))))))
			  ;; Regular NP will get an 's
			  ((possessor ((cat np)
				       (syntax ((case possessive)))))))))


		   ((cat regular-det)
		    (head ((cat article))) ;; don't do anything on it more.
		    (alt determiner 
			 (demo-message "Choose between A, THE and nothing")
			 (((syntax ((countable yes)
				    (definite yes)))
			   (head ((lex "the"))))
			  ((syntax ((countable no))) (gap yes))
			  ((syntax ((definite no)
				    (countable yes)))
			   (alt (((syntax ((number singular)))
				  (head ((lex "a"))))
				 ((syntax ((number plural)))
				  (head ((lex "")))))))))))))

	    ;; ==============================================================
	    ;; 07 CAT LIST : for agglutinated constituents ------------------
	    ;; ==============================================================
	    ;; List is for an agglutination of heterogeneous constituents all
	    ;; playing together the same syntactic function in a larger
	    ;; constituent.
	    ;; For example, a list of describers or qualifiers in an NP.
	    ;; Each element of the list can be of a different cat (unlike
	    ;; conjunction). 
	    ;; Lists have one main feature:
	    ;; elements: a list of features in car/cdr form (~ macro is useful).
	    ;; Just recurse on all elements of the list with no additional processing
	    ((cat list)
	     (elements {^ distinct}) ;; for compatibility with complex
	     (alt list 
		  (demo-message "How many elements are there in the list?")
		  (((elements none)) ;; 0 elements
		   ((elements ((cdr none))) ;; 1 elements 
		    (first {^  elements car})
		    (cset (first)) ;; to eliminate any others
		    (pattern (first)))
		   ((first {^  elements car}) ;; more 
		    (rest ((cat list)
			   (elements {^ ^ elements cdr})))
		    (cset (first rest))  
		    (pattern (first rest))))))


	    ;; ==============================================================
	    ;; 08 CAT COMPLEX : for complex constituents --------------------
	    ;; ==============================================================
	    ;; Complex constituents are syntactic complexes - either conjunction or
	    ;; apposition. The type is specified as (complex T).
	    ;; All conjuncts in a complex must be of the same cat.
	    ;; All existing cats can be part of a complex (cf Winograd AppB).
	    ;; The length of a complex in not limited by syntax (but obviously it
	    ;; is by psychological factors).
	    ;; The main features in a complex are:
	    ;; common: all the features that MUST be common to all conjuncts.
	    ;; distinct: the list of constituents in car/cdr form (~ macro is useful).
	    ;; For appositions: restrictive yes/no
	    ;; For conjunctions: conjunction (a conj).
	    ;; NOTE: this is the generic handler for complex constructs, when no
	    ;; more than just checking that common is indeed common and putting
	    ;; commas and conjunctions is involved.
	    ;; For handling cat specific phenomena (in particular ellipsis), a
	    ;; special branch must be added for complex-clause etc...
	    ((cat #(under complex))
	     (distinct given) ;; fail if nothing specified
	     (alt (:index apposition)
		  (((complex conjunction)
		    (conjunction ((cat conj))))
		   ((complex apposition)
		    (restrictive ((alt (yes no)))))))

	     ;; Enforce a common cat for all elements of the list
	     ;; Do NOT DO  (common ((cat {^ ^ cat}))) alone.
	     ;; because this will fail on a conj proper + common
	     ;; To avoid that: first thought of some hack...
	     ;; Solution would involve defining a configuration like:
	     ;; Instead, map from top-level cat to lowest level category for each top 
	     ;; which is a specialization of all.
	     ;; Example:    NP
	     ;;            /  \
	     ;;        common proper
	     ;;           \    /
	     ;;          BOTTOM-NP
	     ;; But then, you loose the information specific to each constituent
	     ;; (common becomes bottom-np) and when you recurse on it, it will
	     ;; do the wrong thing (lost info. that was common).  To avoid that
	     ;; you would need to manually copy existing cats of constituents to
	     ;; another feature (NOT through conflation)
	     ;; something like (alt (((cat common) (np-type common)) ...))
	     ;; and then do (cat {^ ^ common cat}) in the constituent.
	     ;; This is really ugly... and really destroys the consistency of
	     ;; the typing scheme (that is, you can loose information through
	     ;; unification instead of always gaining some).
	     ;; Instead, here is what I do here:
	     ;; If a cat is given in a constituent, use it.
	     ;; otherwise inherit it from common.
	     ;; NOTE: this does not enforce the constraint of common cat for all
	     ;; conjoints as the hack would do.
	     ;; SO: how do you enforce the constraint?
	     ;; SOLUTION (credit to Frank Smadja): you must realize that the
	     ;; categories NP, AP, PP, CLAUSE have something special - they are
	     ;; the real generic grammatical categories.  All specializations
	     ;; are just defined for implementation reasons.  The constraint on
	     ;; conjunction is that the generic cats are the same - not the
	     ;; specializations.  So let's define a new feature called
	     ;; generic-cat which will be added by each branch of the grammar.
	     ;; And each time, we match the generic cats of all conjoints together.

	     ;; Note: we assume this cat is instantiated (use #(under complex)).
	     (common ((cat {^ ^ cat})))

	     ;; Recurse through elements of list
	     (alt list
		  (:demo "How many conjuncts are there: 0, 1, 2, or more?")
		  (((distinct ((car none)))) ;; the list is empty
		   ((distinct ((cdr ((car none))))) ;; the list has only 1 elt
		    (constituent ((cat ((alt (given {^ ^ common cat}))))))
		    (constituent ((generic-cat {^ ^ common cat}))) 
		    (constituent 
		     ((alt (:index cat)
			   (((cat clause)
			     (mood {^ ^ common mood}) ;; all conjuncts have same mood.
			     (scope {^ ^ common scope}))
			    ((cat np)
			     (syntax {^ ^ common syntax}))
			    ((cat verb-group))
			    ((cat ap))
			    ((cat pp))
			    ((cat noun))
			    ((cat adj))))))
		    (constituent {^ distinct car})
		    (cset (constituent)) ;; to eliminate common as a const.
		    (pattern (constituent)))
	  
		   ((distinct ((cdr ((cdr ((car none))))))) ;; list has only 2 elts
		    (opt ((conjunction ((cat conj) (lex "and"))))) ;; default
		    (constituent1 {^ distinct car})
		    (constituent2 {^ distinct cdr car})
		    ;; hack to avoid the common/proper problem
		    (constituent1 ((cat ((alt (given {^ ^ common cat}))))))
		    (constituent2 ((cat ((alt (given {^ ^ common cat}))))))
		    (constituent1 ((generic-cat {^ ^ common cat}))) 
		    (constituent2 ((generic-cat {^ ^ common cat}))) 
		    (constituent1 
		     ((alt const1-cat (:index cat)
			   (((cat clause)
			     (mood {^ ^ common mood})
			     (scope {^ ^ common scope}))
			    ((cat np)
			     (syntax {^ ^ common syntax}))
			    ;; CANNOT HAVE JUST ((cat complex)) to
			    ;; catch all other cats, because this would
			    ;; also work for clause and nps and remove
			    ;; the constraint...
			    ((cat verb-group))
			    ((cat ap))
			    ((cat pp))
			    ((cat noun))
			    ((cat adj))))))
		    (constituent2 
		     ((alt const2-cat (:index cat)
			   (((cat clause)
			     (mood {^ ^ common mood})
			     (scope {^ ^ common scope}))
			    ((cat np)
			     (syntax {^ ^ common syntax}))
			    ;; catch all for all other cats with no constraint
			    ((cat verb-group))
			    ((cat ap))
			    ((cat pp))
			    ((cat noun))
			    ((cat adj))))))
		    (alt complex-type (:index complex)
			 (((complex conjunction)
			   (constituent1 ((punctuation ((after none)))))
			   (pattern (constituent1 conjunction constituent2)))
			  ((complex apposition)
			   (alt (((restrictive no)
				  (constituent1 ((punctuation ((after ","))))))
				 ((restrictive yes)
				  (constituent1 ((punctuation ((after none))))))))
			   (pattern (constituent1 constituent2)))))
		    (cset (constituent1 constituent2)) ;; to eliminate all others

		    ;; Add a special treatment for clauses
		    ;; Would do similar for other cases of ellipsis *****
		    ;; Ellipsis for more than 2 conjuncts looks hard to do *****
		    (alt verb-ellipsis (:wait process)
			 (:ignore-unless ((cat clause)))
			 (((cat clause)
			   ;; Do ellipsis of verb?
			   ;; Could look at ellipsis of subject - but that's tricky
			   ;; all we have in the input is participants (agent etc.) (use :wait)
			   ;; Want to test on subject not agent. *****
			   ;; How do you do ellipsis of VP without a VP in the grammar *****
			   ({^ constituent1 process} {^ ^ ^ constituent2 process})
			   ({^ constituent2 process gap} yes)
			   (verbal-ellipsis yes))
			  ((verbal-ellipsis no)))))

		   ((distinct ((cdr ((cdr ((car given))))))) ;; list w/more than 3 elts
		    (constituent {^ distinct car})
		    (constituent ((cat ((alt (given {^ ^ common cat}))))))
		    (constituent 
		     ((alt (:index cat)
			   (((cat clause)
			     (mood {^ ^ common mood})
			     (scope {^ ^ common scope}))
			    ((cat ap))
			    ((cat noun))
			    ((cat adj))
			    ((cat pp))
			    ((cat np)
			     (syntax {^ ^ common syntax}))))
		      (punctuation ((after ",")))))
		    (rest ((cat {^ ^ cat})
			   (complex {^ ^ complex})
			   (common {^ ^ common})
			   (restrictive {^ ^ restrictive})
			   (conjunction {^ ^ conjunction})
			   (distinct {^ ^ distinct cdr})))
		    (cset (constituent rest))
		    (pattern (constituent rest))))))

	    ;; ==============================================================
	    ;; Misc categories ignored by the grammar and recognized by the
	    ;; morphology component.
	    ((cat phrase))
	    ((cat article))
	    ((cat pronoun))
	    ((cat cardinal))
	    ((cat ordinal))


	    ))))

  (format t "~%gr9 installed.~%")
  (values))


