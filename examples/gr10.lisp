;;; -*- Mode:Lisp; Syntax:Common-Lisp; -*-
;;; ------------------------------------------------------------
;;; File        : GR10.LISP
;;; Description : Introduce oblique hierarchy and distinguish voice/trans.
;;;               Uses wait.
;;; Author      : Michael Elhadad
;;; Created     : 17 Aug 91 
;;; Modified    : 17 Sep 91
;;; Language    : FUF
;;; ------------------------------------------------------------

;; NOTE: all non-implemented features, or things to do are marked in
;; comments with a ***** sign.

(in-package "FUG5")

  ;; ------------------------------------------------------------------
  ;; External functions 
  ;; ------------------------------------------------------------------

(defun aspect-choice-1 (path)
  (let ((alt-name (car (path-last path))))
    `((alt
       (((aspect event)
	 (tpattern ((:rt0 :equals :et))))
	((aspect given)
	 (aspect ((alt (stative process))))
	 (tpattern ((:rt0 :during :et))))))))
  )


(defun gsetup10 ()

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

  (define-feature-type simple-clause (nominalized-ing verbal-clause))

  ;; A single complex construct handles all the similarities between complex
  ;; constructs and handles the recursion.
  (define-feature-type complex (clause verb-group np ap pp noun adj))

  ;; The NP hierarchy
  (define-feature-type simple-np (pronp common proper nominalized-ing))
  (define-feature-type complex-np (pronp common proper))
  (define-feature-type pronp (personal-pronoun question-pronoun
					       relative-pronoun 
					       quantified-pronoun demonstrative-pronoun))

  (define-feature-type det (possessive-det demonstrative-det regular-det))
  (define-feature-type adv (intensifier detensifier))


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
  ;; cardinal:             an fd with ((value n) (digit yes/no))
  ;; ordinal:              digit default is yes.
  ;;                       distinguishes bet. "1st" and "first".
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

  ;; These are completely determined by the morphology features
  ;; they are not unified with the grammar
  (register-categories-not-unified 'cat
				   '(verb punctuation prep conj relpro modal ordinal cardinal))

  (define-bk-class 'transitive-class 'transitivity)
  (define-bk-class 'voice-class 'voice-class)
  (define-bk-class 'dative-move 'dative-move)
  (define-bk-class 'manner-conveyed 'manner)
  ;; (define-bk-class 'manner 'manner)
  ;; (define-bk-class 'lexical-verb '(ao manner lex-verb))
  (define-bk-class 'ao-conveyed 'ao)
  (define-bk-class 'ao 'ao)
  ;; (define-bk-class 'semantics 'ao)

  (setf *any-at-unification* nil)

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
	     (process ((type {^ process-type})))
	     (process-type {^ process process-type})
	     (lex-roles {^ lexical-roles})

	     ;; General structure of a clause
	     ;; lexical-roles: semantic roles specific to a lexical item
	     ;; participants: the semantic roles
	     ;; oblique:      the obliqueness hierarchy of semantic roles
	     ;; synt-roles:   the syntactic complements
	     ;; circumstances: optional semantic roles (circumstantial)
	     ;; adjuncts:      optional syntactic complements
	     ;; Processing of nuclear participants:
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
	     (alt (((lexical-roles none)
		    (partic given))
		   ((lexical-roles given)
		    (partic none)
		    (process-type lexical))
		   ((lexical-roles none)
		    (partic none)
		    (process-type natural-phenom))))
	     (oblique ((fset (1 2 3 4 5 6 7 8))))
	     (synt-roles ((fset (subject object iobject 
					 subj-comp obj-comp 
					 dative by-obj))))

	     (innermost-role {^ oblique 1})
      
	     ;; MOOD SYSTEM
	     ;; For relative and interrogative, the mapping scope to role is
	     ;; delayed after the transitivity system (same time as the voice
	     ;; system does the mapping semantic/syntactic roles).
	     ;; In this first system, all pre-selections that can be derived from
	     ;; the mood are set.
	     (alt mood (:index mood) 
		  (:demo "Deciding between mood finite and non-finite")

		  (((mood finite)
		    (cat verbal-clause)
		    (alt finite (:index mood)
			 (:demo 
			  "Is the clause simple declarative, interrogative, relative or subordinate?")
			 (((mood declarative)
			   ;; start is a dummy constituent used only in the patterns.
			   (pattern (dots start dots)))

			  ((mood interrogative)
			   ;; for both yes-no and wh questions, front the tensed part
			   ;; of the verb group and the not particle.
			   ;; copy everything from tensed-feature except the gap
			   ;; (therefore cannot just conflate them).
			   ;; Note: these are all the features known to morphology.
			   (scope ((gap yes)))
			   ;; For wh questions don't use dative-move
			   (opt ((mood wh)
				 (dative-move no)))
			   (process ((interrogative {^ ^ mood})))
			   ;; @@Seems like a FUF bug - to check (when cat is a path at recursion time)
			   (cset ((- fronted-aux)))
			   (alt AUX (:wait {^ verb tensed-feature cat})
				(((fronted-aux 
				   ((person {^ ^ verb tensed-feature person})
				    (number {^ ^ verb tensed-feature number})
				    (ending {^ ^ verb tensed-feature ending})
				    (tense  {^ ^ verb tensed-feature tense })
				    (cat    {^ ^ verb tensed-feature cat})
				    (lex    {^ ^ verb tensed-feature lex})))))))

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
			 (:demo "Is the clause imperative, present-participle or infinitive?")
			 (((mood imperative)
			   (cat verbal-clause)
			   (modality none)
			   (epistemic-modality none)
			   (deontic-modality none)
			   (process ((ending root)
				     (voice active)))
			   (innermost-role ((gap yes))))

			  ((mood present-participle)
			   (cat nominalized-ing)
			   (process ((ending present-participle)))
			   (modality none)
			   (epistemic-modality none)
			   (deontic-modality none))

			  ((mood infinitive)
			   (cat verbal-clause)
			   (modality none)
			   (epistemic-modality none)
			   (deontic-modality none)
			   (process ((ending infinitive)))))))))

	     ;; DISPLACED CONSTITUENT PROCESSING
	     ;; for interrogative and relative mood, the constituent under scope
	     ;; is displaced.  We handle it in this alternation.
	     (alt scope-place (:index mood)
		  (((mood yes-no)
		    ;; No displaced component except inversion of verb/subject
		    (pattern (dots fronted-aux fronted-not start dots)))
		   ((mood wh)
		    (scope given)
		    ;; scope is like in relative clauses.  Contains the name
		    ;; of a top-level role on which the question is asked
		    ;; as in: (scope ((role carrier)))
		    ;; NOTE: no need to put participants in there, just the name.
		    ;; Two cases handled: corresponding comp is an NP or a PP.
		    ;; ***** Should check for other cats (adverb, ap).
		    ;; ***** Should check features of verb to decide whether
		    ;; ***** to use a pp or an np as question element
		    ;; ***** Should do questions like "to which house did you
		    ;; ***** go?" (scope is sub-constituent) PRETTY HARD.
		    (alt question-elt-cat (:index (scope cat)) 
			 (((scope ((cat np)))
			   (question 
			    ((cat question-pronoun)
			     (syntax {^ ^ scope syntax})
			     (semantics ((index {^ ^ ^ scope semantics index})))))
			   (alt question-elt (:index (scope role))
				(:demo "Choosing a question element.")
				(((scope ((role at-loc))) 
				  (scope {^ adjuncts at-loc np})
				  (question ((cat question-pronoun) (lex "where"))))
				 ((scope ((role location)))
				  (scope {^ participants location})
				  (alt (((process ((mode equative)
						   (copula no)))
					 (question ((restrictive no))))
					((process ((mode attributive)
						   (type spatial)
						   (circumstance-as participant)))
					 (question ((cat question-pronoun) (lex "where")))))))
				 ((scope ((role to-loc))) 
				  (scope {^ adjuncts to-loc np})
				  (question ((cat question-pronoun) (lex "where"))))
				 ((scope ((role time)))
				  (alt (((process ((type #(under temporal))
						   (mode equative)
						   (copula no)))
					 (question ((restrictive yes)))
					 (scope {^ participants time}))
					((process ((type #(under temporal))
						   (circumstance-as participant)
						   (mode attributive)))
					 (question ((cat question-pronoun) (lex "when")))
					 (scope {^ participants time}))
					((question ((cat question-pronoun) (lex "when")))
					 (scope {^ adjuncts time np})))))
				 ((scope ((role reason)))
				  (question ((cat question-pronoun) (lex "why")))
				  (scope {^ adjuncts reason}))
				 ((scope ((role manner)))
				  (question ((cat question-pronoun) (lex "how")))
				  (scope {^ adjuncts manner}))
				 ((scope ((role agent)))
				  (scope {^ participants agent}))
				 ((scope ((role processor)))
				  (scope {^ participants processor}))
				 ((scope ((role phenomenon)))
				  (scope {^ participants phenomenon}))
				 ((scope ((role carrier)))
				  (process ((type simple-process)))
				  (scope {^ participants carrier}))
				 ((scope ((role located)))
				  (scope {^ participants located}))
				 ((scope ((role possessor)))
				  (process ((type simple-process)))
				  (scope {^ participants possessor}))
				 ((scope ((role possessed)))
				  (scope {^ participants possessed})
				  (opt ((process ((mode equative)))
					(question ((restrictive yes))))))
				 ((scope ((role affected)))
				  (scope {^ participants affected}))
				 ((scope ((role created)))
				  (scope {^ participants created}))
				 ((scope ((role attribute)))
				  (question ((cat question-pronoun) (lex "how")))
				  (scope {^ participants attribute}))
				 ((scope ((role identified)))
				  (scope {^ participants identified}))
				 ((scope ((role identifier)))
				  (question ((restrictive yes)))
				  (scope {^ participants identifier}))))
			   (opt ((scope ((cat question-pronoun))))))

			  ;; Scope of question in a PP (embedded alt)
			  ((scope ((cat pp)))
			   (opt ((scope ((np ((cat question-pronoun)))))))
			   (alt embedded-question-elt (:index  (scope role))
				(:demo "This is an embedded question.  What ~
                                preposition must be used?")
				(((scope ((role instrument)))
				  (opt ((prep ((lex "with")))))
				  (scope {^ adjuncts instrument}))
				 ((scope ((role location)))
				  (process ((type accompaniment)))
				  (scope {^ participants location})
				  (opt ((prep ((lex "with"))))))
				 ((scope ((role possessor)))
				  (process ((type composite)))
				  (dative-move no)
				  (scope {^ synt-roles dative})
				  ({^ synt-roles dative gap} yes)
				  (prep ((lex {^ ^ synt-roles dative prep lex}))))
				 ((scope ((role accompaniment)))
				  (opt ((prep ((lex "with")))))
				  (scope {^ adjuncts accompaniment}))
				 ((scope ((role to-loc)))
				  (opt ((prep ((lex "to")))))
				  (question ((np ((lex "where")))))
				  (scope {^ adjuncts to-loc}))
				 ((scope ((role from-loc)))
				  (question ((np ((lex "where")))))
				  (opt ((prep ((lex "from")))))
				  (scope {^ adjuncts from-loc}))
				 ((scope ((role on-loc)))
				  (question ((np ((lex "where")))))
				  (opt ((prep ((lex "on")))))
				  (scope {^ adjuncts on-loc}))
				 ((scope ((role in-loc)))
				  (question ((np ((lex "where")))))
				  (opt ((prep ((lex "in")))))
				  (scope {^ adjuncts in-loc}))
				 ((scope ((role purpose)))
				  (opt ((prep ((lex "for")))))
				  (scope {^ adjuncts purpose}))
				 ((scope ((role behalf)))
				  (opt ((prep ((lex "for")))))
				  (scope {^ adjuncts comp}))))
			   ;; Since scope is a pp it will not put neatly all
			   ;; features under semantics as nps do.  So do it here
			   (scope ((prep {^ ^ prep})
				   (np ((semantics 
					 ((index ((concept {^ ^ ^ ^ concept})
						  (animate {^ ^ ^ ^ animate})
						  (gender  {^ ^ ^ ^ gender})
						  (person  {^ ^ ^ ^ person})
						  (countable {^ ^ ^ ^ countable})
						  (number  {^ ^ ^ ^ number})))))))))
			   (question ((cat pp)
				      (prep {^ ^ prep})
				      (np ((cat question-pronoun)
					   (syntax ((case objective)))
					   (semantics {^ ^ ^ scope np semantics})
					   (syntax {^ ^ ^ scope np syntax}))))))

			  ;; Scope of question is a clause
			  ((scope ((cat clause)))
			   (question 
			    ((cat question-pronoun)
			     (syntax {^ ^ scope syntax})
			     (semantics ((index {^ ^ ^ scope semantics index})))))
			   (alt question-elt (:index (scope role))
				(:demo "Choosing a question element.")
				(((scope ((role purpose)))
				  (question ((cat question-pronoun) (lex "why")))
				  (scope {^ adjuncts reason-cl})))))))

		    ;; Add the question element in front.
		    (pattern (dots question fronted-aux fronted-not start dots)))

		   ;; MOOD RELATIVE
		   ((mood relative)
		    (alt relative (:index mood) 
			 (:demo "Is the relative clause simple or embedded in a PP?")
			 (((mood simple-relative)
			   ;; Example: the woman who lives there
			   ;;          the man whom I know
			   ;;          the reason why I came
			   ;; Simple relative is the qualifier of an NP. The NP
			   ;; is a constituent of the relative clause, as indicated
			   ;; by the scope constituent:
			   ;; if NP is medium, do (scope ((role medium))) in the relative
			   ;; clause. Scope inherits the relevant features from the 
			   ;; head of the enclosing NP.
			   (pattern (relative-marker start dots))
			   (scope ((gap yes)
				   (cat simple-np)
				   (lex {^ ^ ^ head lex})
				   (semantics ((index {^ ^ ^ ^ semantics index})))))
			   (alt relative-marker (:index (scope role))
				(:demo "Choosing a relative pronoun.")
				(((scope ((role at-loc))) 
				  (scope {^ adjuncts at-loc np})
				  (relative-marker ((cat relpro) (lex "where"))))
				 ((scope ((role location)))
				  (scope {^ participants location})
				  (alt (((process ((mode equative)
						   (copula no)))
					 (relative-marker ((restrictive no))))
					((process ((mode attributive)
						   (type spatial)
						   (circumstance-as participant)))
					 (relative-marker
					  ((cat question-pronoun) (lex "where")))))))
				 ((scope ((role at-time)))
				  (relative-marker ((cat relpro) (lex "when")))
				  (scope {^ adjuncts at-time np}))
				 ((scope ((role time)))
				  (alt (((process ((type #(under temporal))
						   (mode equative)
						   (copula no)))
					 (relative-marker ((restrictive yes)))
					 (scope {^ participants time}))
					((process ((type #(under temporal))
						   (circumstance-as participant)
						   (mode attributive)))
					 (relative-marker
					  ((cat question-pronoun) (lex "when")))
					 (scope {^ participants time}))
					((relative-marker
					  ((cat question-pronoun) (lex "when")))
					 (scope {^ adjuncts time np})))))
				 ((scope ((role reason)))
				  (relative-marker ((cat relpro) (lex "why")))
				  (scope {^ adjuncts reason}))
				 ((scope ((role manner)))
				  (relative-marker ((cat relpro) (lex "how")))
				  (scope {^ adjuncts manner}))
				 ((scope ((role agent)))
				  (scope {^ participants agent}))
				 ((scope ((role medium)))
				  (scope {^ participants medium}))
				 ((scope ((role processor)))
				  (scope {^ participants processor}))
				 ((scope ((role phenomenon)))
				  (scope {^ participants phenomenon}))
				 ((scope ((role carrier)))
				  (process ((type simple-process)))
				  (scope {^ participants carrier}))
				 ((scope ((role located)))
				  (scope {^ participants located}))
				 ((scope ((role possessor)))
				  (process ((type simple-process)))
				  (dative-move no)
				  (scope {^ participants possessor}))
				 ((scope ((role possessed)))
				  (dative-move no)
				  (scope {^ participants possessed}))
				 ((scope ((role affected)))
				  (scope {^ participants affected}))
				 ((scope ((role created)))
				  (scope {^ participants created}))
				 ((scope ((role attribute)))
				  (scope {^ participants attribute}))
				 ((scope ((role identified)))
				  (scope {^ participants identified}))
				 ((scope ((role identifier)))
				  (scope {^ participants identifier}))))
			   (alt relative-marker2
				(((relative-marker ((lex given))))
				 ((alt restrictive-relative (:index restrictive)
				       (((restrictive no)
					 (relative-marker 
					  ((cat relative-pronoun)
					   (semantics {^ ^ scope semantics})
					   (syntax {^ ^ scope syntax}))))
					((restrictive yes)
					 (relative-marker ((cat relpro) (lex "that"))))))))))

			  ;; EMBEDDED RELATIVE - SCOPE is within a PP
			  ((mood embedded-relative)
			   ;; Example: the box in which we store the elixir
			   ;;          an experience the likes of which you have never seen
			   (pattern (relative-marker start dots))
			   (prep ((cat prep)))
			   ;; scope is a pp 
			   ;; does not put features under semantics as nps do.  So do it here
			   (scope ((prep {^ ^ prep})
				   (np ((semantics 
					 ((index ((concept {^ ^ ^ ^ concept})
						  (animate {^ ^ ^ ^ animate})
						  (gender  {^ ^ ^ ^ gender})
						  (person  {^ ^ ^ ^ person})
						  (countable {^ ^ ^ ^ countable})
						  (number  {^ ^ ^ ^ number})))))))))
			   (relative-marker
			    ((cat pp)
			     (prep {^ ^ prep})
			     (np ((cat relative-pronoun)
				  (syntax ((case objective)))
				  (semantics {^ ^ ^ scope np semantics})
				  (syntax {^ ^ ^ scope np syntax})))))
			   (alt scope-embedded (:index (scope role))
				(:demo "This is an embedded relative.  What ~
                                preposition must be used?")
				(((scope ((role instrument)))
				  (opt ((prep ((lex "with")))))
				  (scope {^ adjuncts instrument}))
				 ((scope ((role location)))
				  (process ((type accompaniment)))
				  (scope {^ participants location})
				  (opt ((prep ((lex "with"))))))
				 ((scope ((role location)))
				  (scope {^ participants location}))		  
				 ((scope ((role possessor)))
				  (process ((type composite)))
				  (dative-move no)
				  (scope {^ synt-roles dative})
				  (prep ((lex {^ ^ synt-roles dative prep lex}))))
				 ((scope ((role accompaniment)))
				  (opt ((prep ((lex "with")))))
				  (scope {^ adjuncts accompaniment}))
				 ((scope ((role to-loc)))
				  (opt ((prep ((lex "to")))))
				  (scope {^ adjuncts to-loc}))
				 ((scope ((role from-loc)))
				  (opt ((prep ((lex "from")))))
				  (scope {^ adjuncts from-loc}))
				 ((scope ((role on-loc)))
				  (opt ((prep ((lex "on")))))
				  (scope {^ adjuncts on-loc}))
				 ((scope ((role in-loc)))
				  (opt ((prep ((lex "in")))))
				  (scope {^ adjuncts in-loc}))
				 ((scope ((role purpose)))
				  (opt ((prep ((lex "for")))))
				  (scope {^ adjuncts purpose}))
				 ((scope ((role behalf)))
				  (opt ((prep ((lex "for")))))
				  (scope {^ adjuncts behalf})))))

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
	     ;; Transitivity system determines what configuration of participants
	     ;; can be used for the current process and maps the participants to a
	     ;; position in the obliqueness hierarchy.
	     ;; The voice system does the mapping semantic -> syntactic roles
	     ;; Things done:
	     ;; - Determine configuration of participants
	     ;; - Map participants to obliqueness level
	     ;; - Provides default verbs for processes
	     ;; There are 35 kernel templates, with 35 active and 18 passive forms
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

		  (((process-type lexical)
		    ;; Need to have the mapping lexical-roles -> oblique in lexicon
		    ;; The mapping is under the subcat feature of the process.
		    (process ((subcat given)))
		    (oblique {^ process subcat}))
	 
		   ;; Following are the general classes of verbs
		   ;; using Fawcett's transitivity system.
		   ((process-type simple-process)
		    (alt simple-process (:index process-type)
			 (
			  ;; Material: Ag+Af or Ag+Cr or Ag+Rg
			  ((process-type material)
			   (participants ((fset (agent affected created range))))
			   (alt agentive (:index (process agentive))
				(((process ((agentive yes)))
				  (oblique ((fset (1 2))
					    (1 {^ ^ participants agent})))
				  (alt effective (:index (process effective))
				       (((process ((effective yes)))
					 (alt effect-type (:index (process effect-type))
					      (((process ((effect-type dispositive)))
						(participants ((affected given)
							       (fset (agent affected))))
						(oblique ((2 {^ ^ participants affected}))))
					       ((process ((effect-type creative)))
						(participants ((created given)
							       (fset (agent created))))
						(oblique ((2 {^ ^ participants created}))))
					       ;; end of effective yes
					       )))

					((process ((effective no)
						   (agentive yes)))
					 (participants ((fset (agent range))))
					 (alt range-transitive (:index (participants range))
					      (((participants ((range given)))
						(oblique ((2 {^ ^ participants range}))))
					       ((participants ((range none)))
						(oblique ((fset (1))))))))

					;; end of agentive yes
					)))

				 ((process ((agentive no)
					    (effective yes)))
				  (oblique ((fset (1))))
				  (alt af-type (:index (process effect-type))
				       (((process ((effect-type dispositive)))
					 (participants ((fset (affected))))
					 (oblique ((1 {^ ^ participants affected}))))
					((process ((effect-type creative)))
					 (participants ((fset (created))))
					 (oblique ((1 {^ ^ participants created})))))))

				 ;; end of material
				 )))

			  ;; Mental processes:
			  ((process-type mental)
			   (participants ((fset (processor phenomenon))))
			   (oblique ((fset (1 2))
				     (1 {^ ^ participants processor})
				     (2 {^ ^ participants phenomenon})))
			   (alt mental-transitive (:index (process transitive))
				(((process ((transitive yes)))
				  (participants ((phenomenon any))))
				 ((process ((transitive no)))
				  (participants ((phenomenon none)))))))

	       
			  ;; Verbal processes:  *****
			  ((process-type verbal)
			   (participants ((fset (sayer addressee verbalization))))
			   (oblique ((fset (1 2 3))
				     (1 {^ ^ participants sayer})
				     (2 {^ ^ participants verbalization})
				     (3 {^ ^ participants addressee}))))
	       
			  ;; Relational processes:
			  ((process-type relation)

			   ;; General things on Mode 
			   (alt mode (:index (process mode))
				(((process ((mode attributive)
					    (voice active)))
				  (oblique ((1 {^ ^ participants carrier}))))

				 ((process ((mode equative)))
				  (oblique ((1 {^ ^ participants identified}))))))

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
					 (participants ((fset (carrier attribute))))
					 (oblique ((fset (1 4))
						   (4 {^ ^ participants attribute})))
					 (alt verb-be-attributive 
					      (:demo "What verb for ascriptive attributive?")
					      (((process ((lex "be")
							  (subject-clause infinitive)
							  (object-clause none))))
					       ((process ((lex given)))))))
					((process ((mode equative)))
					 (oblique ((fset (1 2))
						   (2 {^ ^ participants identifier})))
					 (alt verb-be-equative 
					      (:demo "What verb for ascriptive equative?")
					      (((process ((lex "be")
							  (copula yes)
							  (subject-clause that)
							  (object-clause present-participle))))
					       ((process ((lex given))))))))))

				 ;; locative can have 0, 1 or 2 participants
				 ;; 0 and 1 also have a dummy constituent (it or there).
				 ;; We note it here for voice to choose it.
				 ((process-type locative)
				  (alt loc-mode (:index (process mode))
				       (((process ((mode attributive)))
					 (alt loc-arity (:index process-type)
					      (
					       ;; NATURAL-PHENOMENON
					       ((process-type #(under natural-phenom))
						(participants none)
						(dummy-constituent yes)
						(oblique none))

					       ;; EXISTENTIAL
					       ((process-type #(under existential))
						(participants ((fset (carrier located))
							       (located {^ carrier})
							       (carrier given)))
						(dummy-constituent yes)
						(oblique ((fset (1)))))

					       ;; TEMPORAL
					       ((process-type #(under temporal))
						(participants 
						 ((fset (carrier attribute located time))
						  (located {^ carrier})
						  (time {^ attribute})))
						(oblique ((fset (1 4))
							  (4 {^ ^ participants time}))))

					       ;; ANY OTHER LOCATIVE
					       ((process-type locative)
						(participants 
						 ((fset (carrier attribute located location))
						  (located {^ carrier})
						  (location {^ attribute})))
						(oblique ((fset (1 4))
							  (4 {^ ^ participants location})))))))

					((process ((mode equative)))
					 (alt (((process-type #(under temporal))
						(participants 
						 ((fset (identifier identified
								    located location time))
						  (time {^ location}))))
					       ((process-type locative)
						(participants 
						 ((fset (identifier identified 
								    located location)))))))
					 (participants 
					  ((located {^ identified})
					   (location {^ identifier})))
					 (oblique ((fset (1 2))
						   (2 {^ ^ participants identifier}))))))

				  ;; DEFAULT VERB LOCATIVE (both equative and attrib)
				  (process ((alt (((lex "be")
						   (copula yes)
						   (subject-clause none)
						   (object-clause none))
						  ((lex given)))))))

				 ;; POSSESSIVE
				 ((process-type possessive)
				  (alt pos-mode (:index (process mode))
				       (((process ((mode attributive)))
					 (participants 
					  ((fset (carrier attribute possessor possessed))
					   (possessor {^ carrier})
					   (possessed {^ attribute})))
					 (oblique ((fset (1 4))
						   (4 {^ ^ participants possessed})))
					 ;; Default verb is "have"
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
					 (oblique ((fset (1 2))
						   (2 {^ ^ participants possessed})))
					 ;; Default verb is "own"
					 (alt verb-own-possessive
					      (:demo "What verb for possessive?")
					      (((process ((lex "own")
							  (subject-clause none)
							  (object-clause none))))
					       ((process ((lex given))))))))))
		 
				 ;; end of process-type relation
				 )))

			  ;; end of process-type simple
			  )))


		   ;; COMPOSITE PROCESSES:
		   ;; compose only material + relation
		   ;; Specify relation type under relation-type. 
		   ;; Index on agentive/effective/effect-type
		   ;; Best to view composition as adding a result relation to an action
		   ;; Table of possible combinations:
		   ;; event = Ag, Ag+Af, Ag+Cr, Af, Cr 
		   ;; relation = Ca+Att, Ca+Pos, Ca+Loc, Id+Ir, [Id+Loc, Id+Pos]
		   ;; The following embedded alts identify only the permissible 
		   ;; combinations [think of it as a matrix event/rel]
		   ;; Ag+Af/Ca+At:  they made him rich
		   ;; Ag+Af/Id+Ir:  they made him the boss *****
		   ;; Ag/Ca+Af+At:  she made him a good wife
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
			   (participants ((agent ((function agent)))))
			   (oblique ((1 {^ ^ participants agent})))
			   (alt composite-effective (:index (process effective))
				(((process ((effective yes)))
				  (participants ((agent ((function third-party-agent)))))
				  (alt composite-effect-type (:index (process effect-type))
				       (((process ((effect-type dispositive)))
					 ;; Structure is Ag+Af
					 (oblique ((2 {^ ^ participants affected})))

					 ;; Enumerate acceptable relation types
					 (alt ag-af-relation (:index (process relation-type))
					      (
					       ;; Ag+Af/Ca+At: they made him rich
					       ((process ((relation-type ascriptive)
							  (mode attributive)))
						(participants 
						 ((attribute any)
						  (carrier ((function carrier)))
						  (affected {^ carrier})
						  (fset (agent affected carrier attribute))))
						(oblique ((fset (1 2 4))
							  (4 {^ ^ participants attribute})))
						;; Default verb is "make"
						(process
						 ((alt (((lex "make"))
							((lex given)))))))

					       ;; Ag+Af/Id+Ir: they elected him the boss
					       ((process ((relation-type ascriptive)
							  (mode equative)))
						(participants 
						 ((identifier any)
						  (identified ((function identified)))
						  (affected {^ identified})
						  (fset (agent affected identified identifier))))
						(oblique ((fset (1 2 4))
							  (4 {^ ^ participants identifier})))
						;; Default verb is "make"
						(process
						 ((alt (((lex "make"))
							((lex given)))))))
			 
					       ;; Ag+Af/Ca+Pos: the babe gave the boss cold cash
					       ((process ((relation-type possessive)
							  (mode attributive)))
						(participants
						 ((fset (agent affected carrier attribute
							       possessor possessed))
						  (possessed any)
						  (carrier ((function carrier)))
						  (affected {^ carrier})
						  (possessor {^ carrier})
						  (possessed {^ attribute})))
						(oblique ((fset (1 2 3))
							  (3 {^ ^ participants possessed})))
						;; Default verb is "give"
						(process
						 ((alt (((lex "give"))
							((lex given)))))))

					       ;; Ag+Af/Ca+Loc: I push the box to the left
					       ((process ((relation-type locative)
							  (mode attributive)))
						(participants
						 ((location any)
						  (carrier ((function carrier)))
						  (located {^ carrier})
						  (location {^ attribute})
						  (affected {^ carrier})
						  (fset (agent affected carrier attribute
							       located location))))
						(oblique ((fset (1 2 4))
							  (4 {^ ^ participants location})))
						;; Default verb is "move"
						(process
						 ((alt (((lex "move"))
							((lex given)))))))
					       ))
					 ;; Check now that affected or any synonym is indeed given
					 (participants ((affected any)
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
						 ((attribute any)
						  (fset (agent created carrier attribute)))))

					       ;; Ag+Cr/Ca+Loc: The prg popped the wnd on the screen
					       ((process ((relation-type locative)
							  (mode attributive)))
						(participants
						 ((location any)
						  (located {^ carrier})
						  (attribute {^ location})
						  (fset (agent created carrier attribute
							       located location)))))
					       ))
					 ;; Check now that created or any synonym is given
					 (participants ((created any)
							(carrier {^ created})
							(created ((function created)))))
					 (oblique ((fset (1 2 4))
						   (2 {^ ^ participants created})
						   (4 {^ ^ participants attribute}))))
		     
					;; end of effective yes
					)))

				 ;; Agent only
				 ((process ((effective no)))
				  ;; Enumerate permissible relation types
				  (alt ag-relation (:index (process relation-type))
				       (
					;; Ag/Ca+At:     he became rich
					((process ((relation-type ascriptive)
						   (mode attributive)))
					 (participants ((carrier ((function carrier)))
							(carrier {^ agent})
							(attribute any)
							(fset (agent carrier attribute))))
					 (oblique ((fset (1 4))
						   (4 {^ ^ participants attribute})))
					 ;; Default verb is "become"
					 (process ((alt (((lex "become")
							  (subject-clause none)
							  (object-clause none)
							  (change-mode change))
							 ((lex given)))))))

					;; Ag/Id+Ir:     he became the boss
					((process ((relation-type ascriptive)
						   (voice active)
						   (mode equative)))
					 (participants ((identifier any)
							(identified ((function identified)))
							(identified {^ agent})
							(fset (agent identified identifier))))
					 (oblique ((fset (1 4))
						   (4 {^ ^ participants identifier})))
					 ;; Default verb is "become"
					 (process ((alt (((lex "become")
							  (subject-clause none)
							  (object-clause none)
							  (change-mode change))
							 ((lex given)))))))
			 
					;; Ag/Ca+Pos:    the boss bought a Rolls
					((process ((relation-type possessive)
						   (mode attributive)))
					 (participants
					  ((possessed any)
					   (carrier ((function carrier)))
					   (carrier {^ agent})
					   (attribute {^ possessed})
					   (possessor {^ carrier})
					   (fset (agent carrier attribute
							possessor possessed))))
					 (oblique ((fset (1 3))
						   (3 {^ ^ participants possessed})))
					 ;; Default verb is "get"
					 (process ((alt (((lex "get"))
							 ((lex given)))))))
			 
					;; Ag/Ca+Loc:    he went home
					((process ((relation-type locative)
						   (mode attributive)))
					 (participants
					  ((location any)
					   (attribute {^ location})
					   (carrier {^ located})
					   (carrier {^ agent})
					   (carrier ((function carrier)))
					   (fset (agent carrier attribute
							located location))))
					 (oblique ((fset (1 4))
						   (4 {^ ^ participants location})))
					 ;; Default verb is "go"
					 (process ((alt (((lex "go")
							  (object-clause none))
							 ((lex given)))))))

					;; end of effective-no
					))) 

				 ;; end of agentive-yes
				 )))

			  ((process ((agentive no)
				     (effective yes)
				     (mode attributive))) ;; no equative allowed 
			   (participants ((agent none)
					  (carrier ((function carrier)))))
			   (alt composite-effect-type2 (:index (process effect-type))
				(((process ((effect-type dispositive)))
				  ;; Structure is Af
				  (participants ((carrier {^ affected})
						 (affected any)
						 (affected ((function affected)))))
				  (oblique ((1 {^ ^ participants affected})))

				  ;; Enumerate acceptable relation types
				  (alt af-relation (:index (process relation-type))
				       (
					;; Af/Ca+At:     the kettle boiled dry
					((process ((relation-type ascriptive)))
					 (participants 
					  ((attribute any)
					   (fset (affected carrier attribute))))
					 (oblique ((fset (1 4))
						   (4 {^ ^ participants attribute})))
					 ;; Default verb is "turn"
					 (process ((alt (((lex "turn"))
							 ((lex given)))))))

					;; Af/Ca+Pos:    the boss received cash
					((process ((relation-type possessive)))
					 (participants
					  ((possessed any)
					   (possessed {^ attribute})
					   (possessor {^ carrier})
					   (fset (affected carrier attribute
							   possessor possessed))))
					 (oblique ((fset (1 3))
						   (3 {^ ^ participants possessed})))
					 ;; Default verb is "get"
					 (process ((alt (((lex "get"))
							 ((lex given)))))))
			 
					;; Af/Ca+Loc:    the box fell on the floor
					((process ((relation-type locative)
						   (voice active)))
					 (participants
					  ((location any)
					   (location {^ attribute})
					   (located {^ carrier})
					   (fset (affected carrier attribute
							   located location))))
					 (oblique ((fset (1 4))
						   (4 {^ ^ participants location})))
					 ;; Default verb is "move"
					 (process ((alt (((lex "move")
							  (subject-clause none))
							 ((lex given)))))))

					;; end of dispositive
					)))

				 ((process ((effect-type creative)))
				  ;; Structure is Cr
				  (participants ((carrier {^ created})
						 (created any)
						 (created ((function created)))))
				  (oblique ((1 {^ ^ participants created})))

				  ;; Enumerate acceptable relation types
				  (alt cr-relation (:index (process relation-type))
				       (
					;; Cr/Ca+At:     The window popped wide
					((process ((relation-type ascriptive)))
					 (participants 
					  ((attribute any)
					   (fset (created carrier attribute))))
					 (oblique ((fset (1 4))
						   (4 {^ ^ participants attribute})))
					 (process ((lex given))))

					;; Cr/Ca+Loc:    the window popped on the screen
					((process ((relation-type locative)
						   (voice active)))
					 (participants
					  ((location any)
					   (location {^ attribute})
					   (located {^ carrier})
					   (fset (created carrier attribute 
							  located location))))
					 (oblique ((fset (1 4))
						   (4 {^ ^ participants attribute})))
					 (process ((lex given))))
		     
					;; end of creative
					)))

				 ;; end of agentive-no
				 )))

			  ;; end of composite
			  )))

		   ;; end of transitivity system
		   ))

	     ;; VOICE SYSTEM:
	     ;; Map from obliqueness to syntactic roles
	     ;; When more than 1 participant:
	     ;; Choice receptive/operative should be based on focus *****
	     ;; Nuclear Syntactic functions mapped to are: 
	     ;; subject, object, iobject, dative, by-obj, subj-comp, obj-comp
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
	     (alt voice1 (:index process-type)
		  (
		   ;; NATURAL-PHENOMENON
		   ((process-type #(under natural-phenom))
		    (process ((voice active)))
		    (synt-roles ((fset (subject))
				 (subject ((lex "it")
					   (number singular)
					   (cat personal-pronoun))))))
	 
		   ;; EXISTENTIAL
		   ((process-type #(under existential))
		    (synt-roles ((fset (subject subj-comp))
				 (subject ((lex "there")
					   (number {^ ^ ^ oblique 1 number})
					   (cat demonstrative-pronoun)))
				 (subj-comp {^ ^ oblique 1}))))

		   ;; ASCRIPTIVE EQUATIVE: passive with copula is special
		   ;; passive is just a swap of the order of arguments around the copula.
		   ((process-type #(under ascriptive))
		    (process ((mode equative)))
		    (alt equative-voice (:index (process voice))
			 (((process ((voice active)))
			   (synt-roles ((fset (subject subj-comp))
					(subject {^ ^ oblique 1})
					(subj-comp {^ ^ oblique 2}))))
			  ((process ((voice passive)))
			   (alt equative-passive (:index (process voice))
				(((process ((copula no)))
				  (synt-roles ((fset (subject by-obj))
					       (synt-roles ((fset (subject by-obj))
							    (subject {^ ^ oblique 2}))))))
				 ;; This is the real special case
				 ((process ((copula yes)))
				  (synt-roles ((fset (subject subj-comp))
					       (subject {^ ^ oblique 2})
					       (subj-comp {^ ^ oblique 1}))))))))))

		   ;; ANY OTHER PROCESS
		   ((dummy-constituent none)
		    ;; All patterns of obliqueness are:
		    ;; 1, 12, 123, 124, 13, 14
		    ;; For all passives, the decision with/wo by-obj is made in the
		    ;; agent-less alt.
		    (alt voice2 (:index (oblique 2))
			 (((oblique ((2 any)
				     (fset (1 2))))
			   (alt voice21 (:index (process voice))
				(((process ((voice active)))
				  (synt-roles ((fset (subject object))
					       (subject {^ ^ oblique 1})
					       (object  {^ ^ oblique 2}))))
				 ((process ((voice passive)
					    (copula no)))
				  (synt-roles ((fset (subject by-obj))
					       (subject {^ ^ oblique 2})))))))

			  ((oblique ((2 any)
				     (3 any)
				     (fset (1 2 3))))
			   (alt voice22 (:index dative-move) (:bk-class dative-move)
				(((dative-move no)
				  (alt voice222 (:index (process voice))
				       (((process ((voice active)))
					 (synt-roles ((fset (subject object dative))
						      (subject {^ ^ oblique 1})
						      (dative ((np {^ ^ ^ oblique 2})))
						      (object  {^ ^ oblique 3}))))
					((process ((voice passive)
						   (copula no)))
					 (synt-roles ((fset (subject dative by-obj))
						      (subject {^ ^ oblique 3})
						      (dative ((np {^ ^ ^ oblique 2})))))))))
				 ((dative-move yes)
				  (alt voice221 (:index (process voice))
				       (((process ((voice active)))
					 (synt-roles ((fset (subject object iobject))
						      (subject {^ ^ oblique 1})
						      (iobject {^ ^ oblique 2})
						      (object  {^ ^ oblique 3}))))
					((process ((voice passive)
						   (copula no)))
					 (synt-roles ((fset (subject object by-obj))
						      (subject {^ ^ oblique 2})
						      (object  {^ ^ oblique 3}))))))))))

			  ((oblique ((2 any)
				     (4 any)
				     (fset (1 2 4))))
			   (alt voice23 (:index (process voice))
				(((process ((voice active)))
				  (synt-roles ((fset (subject object obj-comp))
					       (subject {^ ^ oblique 1})
					       (object  {^ ^ oblique 2})
					       (obj-comp {^ ^ oblique 4}))))
				 ((process ((voice passive)
					    (copula no)))
				  (synt-roles ((fset (subject by-obj subj-comp))
					       (subject {^ ^ oblique 2})
					       (subj-comp {^ ^ oblique 4})))))))

			  ((oblique ((2 none)
				     (4 any)
				     (fset (1 4))))
			   (process ((voice active)))
			   (synt-roles ((fset (subject subj-comp))
					(subject {^ ^ oblique 1})
					(subj-comp {^ ^ oblique 4}))))

			  ((oblique ((2 none)
				     (fset (1))))
			   (process ((voice active)))
			   (synt-roles ((fset (subject))
					(subject {^ ^ oblique 1}))))

			  ((oblique ((2 none)
				     (3 any)
				     (fset (1 3))))
			   (alt voice26 (:index (process voice))
				(((process ((voice active)))
				  (synt-roles ((fset (subject object))
					       (subject {^ ^ oblique 1})
					       (object  {^ ^ oblique 3}))))
				 ((process ((voice passive)
					    (copula no)))
				  (synt-roles ((fset (subject by-obj))
					       (subject {^ ^ oblique 3}))))))))))))
      
	     ;; Decide whether by-obj is included in passive (non ascriptive equative)
	     (alt agentless1 (:index (process voice))
		  (((process ((voice active))))
		   ((process ((voice passive)))
		    (alt agentless2 (:index agentless)
			 (((agentless no)
			   (synt-roles ((by-obj ((np {^ ^ ^ oblique 1}))))))
			  ((agentless yes)
			   (synt-roles ((by-obj none)))))))))


	     ;; END OF TREATMENT OF INHERENT PARTICIPANTS


	     ;; START OF TREATMENT OF CIRCUMSTANCIAL PARTICIPANTS
	     ;; OPTIONAL CASES:
	     ;; These cases can occur with all process-types.
	     ;; They handle "circumstances".
	     ;; All have the same structure.
	     ;; Order in pattern should be studied with care. I have now a
	     ;; standard order. 
	     ;; All roles are mapped to corresponding syntactic complements from
	     ;; circumstances to adjuncts.
	     ;; CAREFUL that most are inherent participants in the context of a
	     ;; relational clause (eg, "The meeting is at 9" - "at 9" is not an
	     ;; at-time circumstance, it is an attribute of the temporal
	     ;; clause.)
	     ;; ***** Should list criteria for each role and work on a more
	     ;; exhaustive list of roles.  Relate this list to relational
	     ;; processes. 

	     ;; Some synonyms first to make life easier
	     (circum {^ circumstances})

	     ;; List here all known circumstances
	     (circum ((fset (at-loc to-loc from-loc on-loc in-loc
				    instrument accompaniment manner
				    purpose reason behalf
				    at-time))))
      
	     (alt at-loc (:demo "Is there an at-loc role?")
		  (((circum ((at-loc none))))
		   ((circum ((at-loc given)))
		    (adjuncts ((at-loc ((synt-funct at-loc)))))
		    ;; get prep from role if given, otw from verb, otw default.
		    (opt ((circum ((at-loc ((prep given)))))
			  (adjuncts 
			   ((at-loc ((prep ((lex {^ ^ ^ ^ circum at-loc prep})))))))))
		    (opt ((process ((at-loc-prep given)))
			  (adjunts 
			   ((at-loc ((prep ((lex {^ ^ ^ ^ process at-loc-prep})))))))))
		    (adjuncts ((at-loc ((cat pp)
					(opt ((prep ((lex "at")))))
					(np {^ ^ ^ circum at-loc}))))))))

	     (alt to-loc (:demo "Is there a to-loc role?")
		  (((circum ((to-loc none))))
		   ((circum ((to-loc given)))
		    (adjuncts ((to-loc ((synt-funct to-loc)))))
		    (opt ((circum ((to-loc ((prep given)))))
			  (adjuncts 
			   ((to-loc ((prep ((lex {^ ^ ^ ^ circum to-loc prep})))))))))
		    (opt ((process ((to-loc-prep given)))
			  (adjunts 
			   ((to-loc ((prep ((lex {^ ^ ^ ^ process to-loc-prep})))))))))
		    (adjuncts ((to-loc ((cat pp)
					(opt ((prep ((lex "to")))))
					(np {^ ^ ^ circum at-loc}))))))))

	     (alt from-loc (:demo "Is there a from-loc role?")
		  (((circum ((from-loc none))))
		   ((circum ((from-loc given)))
		    (adjuncts ((from-loc ((synt-funct from-loc)))))
		    (opt ((circum ((from-loc ((prep given)))))
			  (adjuncts 
			   ((from-loc 
			     ((prep ((lex {^ ^ ^ ^ circum from-loc prep})))))))))
		    (opt ((process ((from-loc-prep given)))
			  (adjunts 
			   ((from-loc 
			     ((prep ((lex {^ ^ ^ ^ process from-loc-prep})))))))))
		    (adjuncts ((from-loc ((cat pp)
					  (opt ((prep ((lex "from")))))
					  (np {^ ^ ^ circum from-loc}))))))))

	     (alt in-loc (:demo "Is there an in-loc role?")
		  (((circum ((in-loc none))))
		   ((circum ((in-loc given)))
		    (adjuncts ((in-loc ((synt-funct in-loc)))))
		    (opt ((circum ((in-loc ((prep given)))))
			  (adjuncts 
			   ((in-loc ((prep ((lex {^ ^ ^ ^ circum in-loc prep})))))))))
		    (opt ((process ((in-loc-prep given)))
			  (adjunts 
			   ((in-loc ((prep ((lex {^ ^ ^ ^ process in-loc-prep})))))))))
		    (adjuncts ((in-loc ((cat pp)
					(opt ((prep ((lex "in")))))
					(np {^ ^ ^ circum in-loc}))))))))

	     (alt on-loc (:demo "Is there an on-loc role?")
		  (((circum ((on-loc none))))
		   ((circum ((on-loc given)))
		    (adjuncts ((on-loc ((synt-funct on-loc)))))
		    ;; get prep from role if given, otw from verb, otw default.
		    (opt ((circum ((on-loc ((prep given)))))
			  (adjuncts 
			   ((on-loc ((prep ((lex {^ ^ ^ ^ circum on-loc prep})))))))))
		    (opt ((process ((on-loc-prep given)))
			  (adjunts 
			   ((on-loc ((prep ((lex {^ ^ ^ ^ process on-loc-prep})))))))))
		    (adjuncts ((on-loc ((cat pp)
					(opt ((prep ((lex "on")))))
					(np {^ ^ ^ circum on-loc}))))))))

	     (alt instrument (:demo "Is there an instrument role?")
		  (((circum ((instrument none))))
		   ((circum ((instrument given)))
		    (adjuncts ((instrument ((synt-funct instrument)))))
		    (opt ((circum ((instrument ((prep given)))))
			  (adjuncts
			   ((instrument
			     ((prep ((lex {^ ^ ^ ^ circum instrument prep})))))))))
		    (opt ((process ((instrument-prep given)))
			  (adjuncts
			   ((instrument 
			     ((prep ((lex {^ ^ ^ ^ process instrument-prep})))))))))
		    (adjuncts ((instrument ((cat pp)
					    (opt ((prep ((lex "with")))))
					    (np {^ ^ ^ circum instrument}))))))))
      
	     ;; Answer to "who/what with?"
	     (alt accompaniment (:demo "Is there an accompaniment role?")
		  (((circum ((accompaniment none))))
		   ((circum ((accompaniment given)))
		    (adjuncts ((accompaniment ((synt-funct accompaniment)))))
		    (opt ((circum ((accompaniment ((prep given)))))
			  (adjuncts
			   ((accompaniment
			     ((prep ((lex {^ ^ ^ ^ circum accompaniment prep})))))))))
		    (opt ((process ((accompaniment-prep given)))
			  (adjuncts
			   ((accompaniment
			     ((prep ((lex {^ ^ ^ ^ process accompaniment-prep})))))))))
		    (adjuncts ((accompaniment ((cat pp)
					       (opt ((prep ((lex "with")))))
					       (np {^ ^ ^ circum accompaniment}))))))))


	     ;; A special floating constituent: manner can be realized
	     ;; as an adverbial or manner-conveyed by the verb lexically.
	     ;; Manner adjunct
	     (alt manner (:demo "Is there a manner role?")
		  (:bk-class (ao manner))
		  (:wait ({^ circum manner manner-conveyed}))
		  (((circum ((manner none))))
		   ;; Can it be realized by other means? delay
		   ((circum ((manner given)))
		    (circum ((manner ((manner-conveyed any))))))
		   ;; If cannot be realized any other way, resort to an adverb
		   ((circum ((manner given)))
		    (adjuncts ((manner ((cat adv)
					(concept {^ ^ ^ circum manner concept})
					(lex {^ ^ ^ circum manner lex})))
			       (ao none)))
		    (circum ((manner ((manner-conveyed adverb)))))
		    (pattern (dots {^ adjuncts manner} process dots)))
		   ;; or to a pp
		   ((circum ((manner given)))
		    (adjuncts ((manner ((cat pp)
					(np ((concept {^ ^ ^ ^ circum manner
						      concept})))
					(opt ((prep ((lex "with")))))))
			       (ao none)))
		    (circum ((manner ((manner-conveyed pp)))))
		    (pattern (dots process dots {^ adjuncts manner} dots)))))


	     ;; A special floating constituent: ao can be realized by an adverb
	     ;; ao competes with manner for the adjuncts manner adverbial position
	     (ao 
	      ((alt ao-adverbial (:bk-class ao)
		    (none
		     (({^ ao} given)
		      (alt ao-adverbial-present (:bk-class ao)
			   (:wait {^ ao-conveyed})
			   (:demo "Can AO be conveyed by an adverbial")
			   (((ao-conveyed any))
			    (({^ adjuncts} ((ao any)
					    (ao ((cat adv)
						 (concept {^ ^ ^ ao concept})
						 (lex {^ ^ ^ ao lex})))
					    (manner none)))
			     (ao-conveyed adverb)
			     ({^} ((pattern (dots {^ adjuncts ao} process dots))))))))))))


	     ;; THREE CAUSE COMPLEMENTS (as by Hallyday): reason, purpose, behalf
	     ;; purpose: answer to "what for?"
	     (alt purpose (:demo "Is there a purpose role?")
		  (((circum ((purpose none))))
		   ((circum ((purpose given)))
		    (adjuncts ((purpose-cl {^ ^ circum purpose})
			       (purpose-cl ((synt-funct purpose)))))
		    (circum ((purpose ((cat clause)
				       (mood infinitive)
				       (syntax ((case purposive)))
				       (opt ((in-order ((lex "in order") 
							(cat conj)))))))))
		    (alt (((pattern (dots {^ adjuncts purpose-cl} start dots))
			   (adjuncts ((purpose-cl ((punctuation ((after ","))))))))
			  ((pattern (dots {^ adjuncts purpose-cl}))))))
		   ((circum ((purpose given)))
		    (adjuncts ((purpose ((synt-funct purpose)))))
		    (circum ((purpose 
			      ((cat np)
			       (opt ((prep given)
				     (prep {^ ^ ^ adjuncts purpose prep lex})))))))
		    (opt ((process ((purpose-prep given)))
			  (adjuncts
			   ((purpose
			     ((prep ((lex {^ ^ ^ ^ process purpose-prep})))))))))
		    (adjuncts ((purpose ((cat pp)
					 (opt ((prep ((lex "for")))))
					 (np {^ ^ ^ circum purpose}))))))))
      
	     ;; reason: answer to "why? How?"
	     (alt reason (:demo "Is there a reason role?")
		  (((circum ((reason none))))
		   ((circum ((reason given)))
		    (adjuncts ((reason {^ ^ circum reason})
			       (reason ((cat clause)
					(mood bound)
					(binder ((lex "because"))))))))
		   ((circum ((reason given)))       
		    (circum ((reason ((cat np)))))
		    (adjuncts ((reason ((synt-func reason)))))
		    (opt ((circum ((reason ((prep given)))))
			  (adjuncts
			   ((reason ((prep ((lex {^ ^ ^ ^ circum reason prep})))))))))
		    (opt ((process ((reason-prep given)))
			  (adjuncts
			   ((reason ((prep ((lex {^ ^ ^ ^ process reason-prep})))))))))
		    (adjuncts 
		     ((reason ((cat pp)
			       (opt ((prep ((lex "because of")))))
			       (np {^ ^ ^ circum reason}))))))))

	     ;; behalf: answer to "who for?"
	     (alt behalf (:demo "Is there a behalf role?")
		  (((circum ((behalf none))))
		   ;; behalf as a for-to infinitive clause
		   ;; Note: subject must be given and is actually the behalf
		   ;; "You have to do it for John to read" (Winograd p.472)
		   ((circum ((behalf given)))
		    (adjuncts ((behalf ((synt-func behalf)))))
		    (adjuncts ((behalf ((cat clause)
					(mood infinitive)))
			       (behalf {^ ^ circum behalf}))))
		   ((circum ((behalf given)))
		    (circum ((behalf ((cat np)))))
		    (adjuncts ((behalf ((synt-func behalf)))))
		    (opt ((circum ((behalf ((prep given)))))
			  (adjuncts 
			   ((behalf ((prep ((lex {^ ^ ^ ^ circum behalf prep})))))))))
		    (opt ((process ((behalf-prep given)))
			  (adjuncts
			   ((behalf ((prep ((lex {^ ^ ^ ^ process behalf-prep})))))))))
		    (adjuncts
		     ((behalf ((cat pp)
			       (opt ((prep ((lex "for")))))
			       (np {^ ^ ^ circum behalf}))))))))
      

	     ;; All the possible time roles under one time plus a time-type
	     ;; feature specializing it.  
	     ;; Can be a (cat list) for agglutination of time complements but then
	     ;; the list should contain complete syntactic fds.
	     ;; The list of time-type is given in Quirk 11.27
	     ;; Ex: in the afternoon, later, when I have time, last Thursday
	     ;; ***** Should implement the semantics of time-type with tpattern.
	     (alt time	(:demo "Is there a time role?")
		  (((circum ((time none))))
		   ((circum ((time given)))
		    (adjuncts ((time ((synt-funct time)))))
		    (alt time-adjunct (:index (circum time cat))
			 (((circum ((time ((cat adv)))))
			   (adjuncts ((time {^ ^ circum time}))))
			  ((circum ((time ((cat clause)
					   (mood bound)
					   (binder ((lex {^ ^ time-type})))
					   (time-type 
					    ((alt ("after" "as" "before" "once" "since"
							   "until" "when" "while" "now that"))))))))
			   (adjuncts ((time {^ ^ circum time}))))
			  ((circum ((time ((cat list)))))
			   (adjuncts ((time {^ ^ circum time}))))
			  ((circum ((time ((cat np)
					   (time-type 
					    ((alt ("at" "on" "in" "for" "before" "after"
							"since" "until"))))))))
			   (adjuncts ((time ((cat pp)
					     (prep ((lex {^ ^ ^ ^ circum time time-type})))
					     (np {^ ^ ^ circum  time})))))))))))

	     ;; time-relater are "first", "next", "then" occuring first in the
	     ;; clause.  Note that they are not movable when used in this relater
	     ;; sense.  So they are not just simply adverbials.
	     ;; ***** To re-do when complex clauses are implemented.
	     (alt time-relater (:demo "Is there a time-relater?")
		  (((time-relater none))
		   ((time-relater given)
		    (time-relater ((cat adv)
				   (punctuation ((after ",")))))
		    (pattern (time-relater dots)))))

	     ;; cond-relater is "if", "then" or "else" - this is used until we
	     ;; come up with a better treatment of complex clauses. *****
	     (alt cond-relater (:demo "Is there a cond-relater?")
		  (((cond-relater none))
		   ((cond-relater given)
		    (cond-relater ((cat adv)))
		    (pattern (time-relater cond-relater dots)))))
      
	     ;; END OF CIRCUMSTANTIAL ROLES TREATMENT

	     ;; CHECK CAT OF EACH SYNTACTIC ROLE
	     ;; SUBCATEGORIZATION SYSTEM
	     ;; Get lexical information on government pattern
	     (process ((subcat {^ ^ oblique})))

	     ;; special treatment of subject clauses (this is the right time
	     ;; because the subject is now bound). 
	     (alt subject-mood	(:index mood) 
		  (:demo "Is a subject required or does it need a special treatment?")
		  (((mood finite)
		    (synt-roles ((subject given))))
		   ((mood wh)
		    (scope {^ synt-roles subject}))
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
				 ((keep-for no)))))
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
	     ;; SUBJECT CAT = NP, CLAUSE.
	     (alt subject-subcat (:index (synt-roles subject cat))
		  (((synt-roles ((subject none))))
		   ((synt-roles ((subject ((cat np))))))
		   ((synt-roles ((subject ((cat #(under clause))))))
		    (alt subject-clause	(:index (process subject-clause))
			 (:demo "For clausal subjects, what type of clause ~
                     must be used?")
			 (((process ((subject-clause infinitive)))
			   (synt-roles ((subject ((mood {^ ^ ^ process subject-clause}))))))
			  ((process ((subject-clause #(under present-participle))))
			   (synt-roles ((subject ((mood {^ ^ ^ process subject-clause}))))))
			  ((process ((subject-clause that)))
			   (synt-roles ((subject ((mood bound)
						  (binder ((lex "that")))))))))))
		   ((synt-roles ((subject ((cat #(under list)))))))))
	     (opt ((synt-roles ((subject ((synt-funct subject)))))))
      
	     ;; OBJECT CAT = NP, CLAUSE
	     (alt object-subcat (:index  (synt-roles object cat))
		  (((synt-roles ((object none))))
		   ((synt-roles ((object ((cat np))))))
		   ((synt-roles ((object ((cat #(under clause))))))
		    (alt object-clause (:index (process object-clause))
			 (:demo "For clausal objects, what type of clause ~
                    must be used?")	  
			 (((process ((object-clause infinitive)))
			   (synt-roles ((object ((mood infinitive))))))
			  ((process ((object-clause #(under present-participle))))
			   (synt-roles ((object ((mood present-participle))))))
			  ((process ((object-clause that)))
			   (synt-roles ((object ((mood bound)
						 (binder ((lex "that")))))))))))
		   ((synt-roles ((object ((cat #(under list)))))))))
	     (opt ((synt-roles ((object ((synt-funct object)))))))

	     ;; SUBJ-COMP CAT = NP, AP, PP, ADV
	     (synt-roles ((subj-comp ((alt subj-comp-cat (:index (subj-comp cat))
					   (none
					    ((cat ap))
					    ((cat given))
					    ((cat #(under np)))
					    ((cat #(under pp)))
					    ((cat #(under list)))
					    ((cat #(under adv)))))))))
	     (opt ((synt-roles ((subj-comp ((synt-funct subj-comp)))))))

	     ;; OBJ-COMP CAT = NP, AP, PP, ADV
	     (synt-roles ((obj-comp ((alt obj-comp-cat (:index (obj-comp cat))
					  (none
					   ((cat ap))
					   ((cat given))
					   ((cat #(under np)))
					   ((cat #(under pp)))
					   ((cat #(under list)))
					   ((cat #(under adv)))))))))
	     (opt ((synt-roles ((obj-comp ((synt-funct obj-comp)))))))

	     ;; BY-OBJ CAT = PP, set prep
	     (synt-roles ((alt by-obj-cat (:index (by-obj cat))
			       (((by-obj none))
				((by-obj given)
				 (by-obj ((cat pp)
					  (prep ((lex "by")))
					  (np ((cat np)
					       (syntax ((case objective))))))))))))
	     (opt ((synt-roles ((by-obj ((np ((synt-funct subj-comp)))))))))

	     ;; DATIVE CAT = PP, set prep
	     (synt-roles 
	      ((alt dative-cat (:index (dative cat))
		    (((dative none))
		     ((dative given)
		      (dative ((cat pp)
			       ({^ ^ process dative-prep} given)
			       (prep ((lex {^ ^ ^ ^ process dative-prep})))
			       (np ((cat np) 
				    (syntax ((case objective))))))))
		     ((dative ((cat pp)
			       (prep ((lex "to")))
			       (np ((cat np) 
				    (syntax ((case objective))))))))))))
	     (opt ((synt-roles ((dative ((np ((synt-funct dative)))))))))


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
	     (alt particle (:demo "Does the verb have a particle?")
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
	     (pattern (dots start dots process dots 
			    {^ adjuncts accompaniment} 
			    {^ adjuncts behalf}  
			    {^ adjuncts reason}
			    {^ adjuncts purpose}
			    {^ adjuncts time} 
			    {^ adjuncts at-loc}
			    {^ adjuncts from-loc}
			    {^ adjuncts to-loc}
			    {^ adjuncts on-loc}
			    {^ adjuncts in-loc} 
			    {^ adjuncts instrument} 
			    dots))

	     )

	    ;;==============================================================
	    ;; 02 CAT VERB-GROUP -------------------------------------------
	    ;;==============================================================
	    ;; Overall structure of verb sequence:
	    ;;     aux have-1 beg going-to have-2 be-1 be-2 event
	    ;; with the not marker placed at the first non-empty slot (after modal if there is modal, else after aux etc)
	    ;; be-2 is only used for the passive voice
	    ;; aux is either the modal if present, or will, or do.
	    ;; Other elements:
	    ;; notf
	    ;; tensed-feature: the element that carries the agreement features (person, number, ending)
	    ;; verb-aspect:    root, past-participle, present-participle (ending of the inflected element - event or be-2)
	    ;; time-frame:     present, future, past
	    ;; tense:          tense-1 ... tense-36 (with nicknames)
	    ;; polarity:       positive, negative
	    ;; event:          the main verb with lex
	    ;;
	    ;; Example: must have been going to have been taken
	    ;; 
	    ((cat simple-verb-group)
	     (generic-cat verb-group)
	     (complex none)
	     (interrogative ((alt (none #(under yes-no) #(under wh))))) ;; default is not interrogative
	     (insistence ((alt (no #(under yes))))) ;; default is not insistent

	     (alt verb-lexicalization (:demo "Lexicalization of verb")
		  (((concept given)
		    (lexical-verb ((concept {^ ^ concept})
				   (subcat {^ ^ subcat})
				   (lex {^ ^ lex})
				   (cat lex-verb))))
		   ((lex given))))

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

	     ;; there is always an event verb -- it is also called the main verb.
	     (event ((cat verb)
		     (lex { ^ ^ lex})))

	     ;; deal with modality: allow only one type or no modality at all
	     ;; default is no modality
	     (alt only-one-modal
		  (((epistemic-modality none)
		    (deontic-modality none)
		    (modal-present no))
		   ((epistemic-modality given)
		    (deontic-modality none)
		    (modal-present yes))
		   ((deontic-modality given)
		    (epistemic-modality none)
		    (modal-present yes))))

	     ;; now, code the tense-specific stuff
	     ;; ***** Done only for case of non-modal finite tense
	     (alt tense-selection (:index tense)
		  (:demo "what is the tense?")
	

		  ;; SIMPLE TENSES
		  ;; Cannot say if simple until we look at voice, interrogative and polarity
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
		    (simple yes)
		    (time-frame present)
		    (verb-aspect nil)       ;; inflected based on tense / depends on aux decision
		    (first-verb {^ event})
		    (tensed-feature nil)    ;; depends on later decision: voice, interrogative, polarity
		    (tensed-feature ((tense present))))

		   ;; Tense 1: past
		   ;; I took the bus.[a]
		   ;; The bus was taken by me.[p]
		   ((tense past)
		    (tpattern ((:rt0 :precedes :st)
			       (:st  :none     :rt1)
			       (:et  :none     :rt1)
			       (:rt0 :none     :rt1)))
		    ({^ verb} #(external aspect-choice-1))
		    (simple yes)
		    (time-frame past)
		    (verb-aspect nil)       ;; inflected based on tense / depends on aux decision
		    (first-verb {^ event})
		    (tensed-feature nil)    ;; depends on later decision: voice, interrogative, polarity
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
		    (verb-aspect root)
		    (first-verb {^ aux})
		    (tensed-feature {^ aux})
		    (aux ((lex "will")
			  (cat modal))))

		   ;; tense 4
		   ;; I had taken the bus.(a)
		   ;; The bus had been taken by me.(p)
		   ((tense past-perfect)
		    (tpattern ((:rt1 :precedes :st)
			       (:rt0 :precedes :rt1)
			       (:st :none :rt2)
			       (:rt1 :none :rt2)
			       (:rt0 :none :rt2)
			       (:et :none :rt2)))
		    ({^ verb} #(external aspect-choice-1))
		    (simple no)
		    (time-frame past)
		    (verb-aspect past-participle)
		    (first-verb {^ have-1})
		    (tensed-feature ((tense past))))

		   ;; tense 5
		   ;; I have taken the bus
		   ;; The bus has been taken by me.
		   ((tense  present-perfect)
		    (tpattern ((:rt0 :precedes :rt1)
			       (:rt1 :equals :st)
			       (:st :none :rt2)
			       (:rt1 :none :rt2)
			       (:et :none :rt2)
			       (:rt0 :none :rt2)))
		    ({^ verb} #(external aspect-choice-1))
		    (simple no)
		    (time-frame present)
		    (verb-aspect past-participle)
		    (first-verb {^ have-1})
		    (tensed-feature ((tense present))))

		   ;; tense 6
		   ;; I will have taken the bus.[a]
		   ;; The bus will have been taken by me.[p]
		   ((tense future-perfect)
		    (tpattern ((:st :precedes :rt0)
			       (:rt0 :precedes :rt1)
			       (:st :none :rt2)
			       (:rt1 :none :rt2)
			       (:et :none :rt2)
			       (:rt0 :none :rt2)))
		    ({^ verb} #(external aspect-choice-1))
		    (simple no)
		    (time-frame future)
		    (verb-aspect past-participle)
		    (first-verb {^ aux})
		    (tensed-feature {^ aux})
		    (aux ((lex "will")
			  (cat modal)))
		    (have-1 ((ending root))))

		   ;; tense 7
		   ;; I was taking the bus.(a)
		   ;; The bus was being taken by me.(p)
		   ((tense past-progressive)
		    (tpattern ((:rt0 :precedes :st)
			       (:rt0 :includes :et)
			       (:st :none :rt1)
			       (:et :none :rt1)
			       (:rt0 :none :rt1)))
		    (aspect ((alt aspect-7 (event process))))
		    (simple no)
		    (time-frame past)
		    (verb-aspect present-participle)
		    (first-verb {^ be-1})
		    (tensed-feature {^ be-1})
		    (be-1 ((tense past))))

		   ;; tense 8
		   ;; I am taking the bus.(a)
		   ;; The bus is being taken by me.(p)
		   ((tense present-progressive)
		    (tpattern ((:rt0 :precedes :st)
			       (:rt0 :includes :et)
			       (:st :none :rt1)
			       (:et :none :rt1)
			       (:rt0 :none :rt1)))
		    (aspect ((alt aspect-8 (event process))))
		    (simple no)
		    (time-frame present)
		    (verb-aspect present-participle)
		    (first-verb {^ be-1})
		    (tensed-feature {^ be-1})
		    (be-1 ((tense present))))

		   ;; tense 9
		   ;; I will be taking the bus.(a)
		   ;; The bus will be being taken by me.(p)
		   ((tense future-progressive)
		    (tpattern ((:st :precedes :rt0)
			       (:rt0 :includes :et)
			       (:st :none :rt1)
			       (:et :none :rt1)
			       (:rt0 :none :rt1)))
		    (aspect ((alt aspect-9 (event process))))
		    (simple no)
		    (time-frame future)
		    (verb-aspect present-participle)
		    (first-verb {^ aux})
		    (tensed-feature {^ aux})
		    (aux ((lex "will")
			  (cat modal)))
		    (be-1 ((ending root))))
	 
		   ;; tense 10
		   ;; I was going to take the bus.[a]
		   ;; The bus was going to be taken by me.[p]
		   ((tense tense-10)
		    (tpattern ((:rt1 :precedes :st) (:rt1 :precedes :rt0)
			       (:rt1 :none :rt2) (:st :none :rt2)
			       (:et :none :rt2)))
		    ({^ verb} #(external aspect-choice-1))
		    (simple no)
		    (time-frame past)
		    (verb-aspect root)
		    (first-verb {^ beg})
		    (tensed-feature {^ beg})
		    (beg ((tense past))))
	 
		   ;; tense 11
		   ;; I am going to take the bus.[a]
		   ;; The bus is going to be taken by me.[p]
		   ((tense tense-11)
		    (tpattern ((:st :equals :rt1) (:rt1 :precedes :rt0)
			       (:st :none :rt2) (:et :none :rt2)
			       (:rt1 :none :rt2) (:rt0 :none :rt2)))
		    ({^ verb} #(external aspect-choice-1))
		    (simple no)
		    (time-frame present)
		    (verb-aspect root)
		    (first-verb {^ beg})
		    (tensed-feature {^ beg})
		    (beg ((tense present))))

		   ;; tense 12
		   ;; I will be going to take.[a]
		   ;; The bus will be going to be taken.[b]
		   ((tense tense-12)
		    (tpattern ((:st :precedes :rt1)
			       (:rt1 :precedes :rt0)
			       (:st :none :rt2) (:et :none :rt2)
			       (:rt1 :none :rt2) (:rt0 :none :rt2)))
		    ({^ verb} #(external aspect-choice-1))
		    (simple no)
		    (time-frame future)
		    (verb-aspect root)
		    (first-verb {^ aux})
		    (tensed-feature {^ aux})
		    (beg ((ending root)))
		    (aux ((lex "will")
			  (cat modal))))

		   ;; tense 13
		   ;; I was going to have taken the bus.[a]
		   ;; The bus was going to have been taken by me.[p]
		   ((tense tense-13)
		    (tpattern ((:rt1 :precedes :st) (:rt1 :precedes :rt0)
			       (:rt0 :precedes :rt2) (:st :none :rt3)
			       (:rt0 :none :rt3) (:rt1 :none :rt3)
			       (:rt2 :none :rt3) (:et :none :rt3)))
		    ({^ verb} #(external aspect-choice-1))
		    (simple no)
		    (time-frame past)
		    (verb-aspect past-participle)
		    (first-verb {^ beg})
		    (tensed-feature { ^ beg})
		    (beg ((tense past)))
		    (have-2 ((ending root))))

		   ;; tense 14
		   ;; I am going to have taken the bus.[a]
		   ;; The bus is going to have been taken by me.[p]
		   ((tense tense-14)
		    (tpattern ((:st :equals :rt1) (:rt1 :precedes :rt0)
			       (:rt0 :precedes :rt2) (:st :none :rt3)
			       (:rt0 :none :rt3) (:rt1 :none :rt3)
			       (:rt2 :none :rt3)))
		    ({^ verb} #(external aspect-choice-1))
		    (simple no)
		    (time-frame present)
		    (verb-aspect past-participle)
		    (first-verb {^ beg})
		    (tensed-feature { ^ beg})
		    (beg ((tense present)))
		    (have-2 ((ending root))))
	  
		   ;; tense 15
		   ;; I will be going to have taken the bus.[a]
		   ;; The bus will be going to have been taken by me.[p]
		   ((tense tense-15)
		    (tpattern ((:st :precedes :rt1) (:rt1 :precedes :rt0)
			       (:rt0 :precedes :rt2) (:st :none :rt3)
			       (:rt0 :none :rt3) (:rt1 :none :rt3)
			       (:rt2 :none :rt3)))
		    ({^ verb} #(external aspect-choice-1))
		    (simple no)
		    (time-frame future)
		    (verb-aspect past-participle)
		    (first-verb {^ aux})
		    (tensed-feature {^ aux})
		    (aux ((lex "will") (cat modal)))
		    (beg ((ending root)))
		    (have-2 ((ending root))))

		   ;; tense 16
		   ;; I had been taking the bus.[a]
		   ;; The bus had been being taken by me.[p]
		   ((tense tense-16)
		    (tpattern ((:rt1 :precedes :st) (:rt0 :precedes :rt1)
			       (:rt0 :during :et) (:st :none :rt2)
			       (:rt0 :none :rt2) (:rt1 :none :rt2)))
		    (aspect ((alt (process event))))
		    (simple no)
		    (time-frame past)
		    (verb-aspect present-participle)
		    (first-verb {^ have-1})
		    (tensed-feature {^ have-1})
		    (have-1 ((tense past)))
		    (be-1 ((ending past-participle))))

		   ;; tense 17
		   ;; I have been taking the bus.[a]
		   ;; The bus has been being taken by me.[p]
		   ((tense tense-17)
		    (tpattern ((:rt1 :equals :st) (:rt0 :precedes :rt1)
			       (:rt0 :during :et) (:st :none :rt2)
			       (:rt0 :none :rt2) (:rt1 :none :rt2)))
		    (aspect ((alt (process event))))
		    (simple no)
		    (time-frame present)
		    (verb-aspect present-participle)
		    (first-verb {^ have-1})
		    (tensed-feature {^ have-1})
		    (have-1 ((tense present)))
		    (be-1 ((ending past-participle))))

		   ;; tense 18
		   ;; I will have been taking the bus.[a]
		   ;; The bus will have been being taken by me.[p]
		   ((tense tense-18)
		    (tpattern ((:rt1 :precedes :st) (:rt0 :precedes :rt1)
			       (:rt0 :during :et) (:st :none :rt2)
			       (:rt0 :none :rt2) (:rt1 :none :rt2)))
		    (aspect ((alt (process event))))
		    (simple no)
		    (time-frame future)
		    (verb-aspect present-participle)
		    (first-verb {^ aux})
		    (tensed-feature {^ aux})
		    (aux ((lex "will") (cat modal)))
		    (have-1 ((ending root)))
		    (be-1 ((ending past-participle))))

		   ;; tense 19
		   ;; I was going to be taking the bus.[a]
		   ;; The bus was going to be being taking the bus.[p]
		   ((tense tense-19)
		    (tpattern ((:rt1 :precedes :st) (:rt1 :precedes :rt0)
			       (:rt0 :during :et) (:st :none :rt2)
			       (:rt0 :none :rt2) (:rt1 :none :rt2)
			       (:et :none :rt2)))
		    (aspect ((alt aspect-19 (process event))))
		    (simple no)
		    (time-frame past)
		    (verb-aspect present-participle)
		    (first-verb {^ beg})
		    (tensed-feature {^ beg})
		    (beg ((tense past)))
		    (be-1 ((ending root))))

		   ;; tense 20
		   ;; I am going to be taking the bus.[a]
		   ;; The bus is going to be being taken by me.[p]
		   ((tense tense-20)
		    (tpattern ((:rt1 :equals :st) (:rt1 :precedes :rt0)
			       (:rt0 :during :et) (:st :none :rt2)
			       (:rt0 :none :rt2) (:rt1 :none :rt2)
			       (:et :none :rt2)))
		    (aspect ((alt aspect-20 (process event))))
		    (simple no)
		    (time-frame present)
		    (verb-aspect present-participle)
		    (first-verb {^ beg})
		    (tensed-feature {^ beg})
		    (beg ((tense present)))
		    (be-1 ((ending root))))

		   ;; tense 21
		   ;; I will be going to be taking the bus.[a]
		   ;; The bus will be going to be being taken by me.[p]
		   ((tense tense-21)
		    (tpattern ((:st :precedes :rt1) (:rt1 :precedes :rt0)
			       (:rt0 :during :et) (:st :none :rt2)
			       (:rt0 :none :rt2) (:rt1 :none :rt2)
			       (:et :none :rt2)))
		    (aspect ((alt aspect-21 (process event))))
		    (simple no)
		    (time-frame future)
		    (verb-aspect present-participle)
		    (first-verb {^ aux})
		    (tensed-feature {^ aux})
		    (aux ((lex "will") (cat modal)))
		    (beg ((ending root)))
		    (be-1 ((ending root))))

		   ;; ***** NO SEMANTICS
		   ;; tense 22
		   ;; I had been going to take the bus.[a]
		   ;; The bus had been going to be taken by me.[p]
		   ((tense tense-22)
		    (aspect ((alt aspect-22 (process event))))
		    (simple no)
		    (time-frame past)
		    (verb-aspect root)
		    (first-verb {^ have-1})
		    (tensed-feature {^ have-1})
		    (have-1 ((tense past)))
		    (beg ((ending past-participle))))

		   ;; ***** NO SEMANTICS
		   ;; tense 23
		   ;; I have been going to take the bus.[a]
		   ;; The bus has been going to be taken by me.[p]
		   ((tense tense-23)
		    (aspect ((alt aspect-23 (process event))))
		    (simple no)
		    (time-frame present)
		    (verb-aspect root)
		    (first-verb {^ have-1})
		    (tensed-feature {^ have-1})
		    (have-1 ((tense present)))
		    (beg ((ending past-participle))))

		   ;; ***** NO SEMANTICS
		   ;; tense 24
		   ;; I will have been going to take the bus.[a]
		   ;; The bus will have been going to be taken by me.[p]
		   ((tense tense-24)
		    (aspect ((alt aspect-22 (process event))))
		    (simple no)
		    (time-frame future)
		    (verb-aspect root)
		    (first-verb {^ aux})
		    (tensed-feature {^ aux})
		    (aux ((lex "will") (cat modal)))
		    (have-1 ((ending root)))
		    (beg ((ending past-participle))))

		   ;; tense 25
		   ;; I had been going to have taken the bus.[a]
		   ;; The bus had been going to have been taken by me.[p]???
		   ((tense tense-25)
		    (tpattern ((:rt2 :precedes :st)
			       (:rt1 :precedes :rt2)
			       (:rt1 :precedes :rt0)
			       (:rt0 :precedes :rt3)))
		    ({^ verb} #(external aspect-choice-1))
		    (simple no)
		    (time-frame past)
		    (verb-aspect past-participle)
		    (first-verb {^ have-1})
		    (tensed-feature {^ have-1})
		    (have-1 ((tense past)))
		    (beg ((ending past-participle)))
		    (have-2 ((ending root))))

		   ;; tense 26
		   ;; I have been going to have taken the bus.[a]
		   ;; The bus has been going to have been taken by me.[p]???
		   ((tense tense-26)
		    (tpattern ((:rt2 :equals :st)
			       (:rt1 :precedes :rt2)
			       (:rt1 :precedes :rt0)
			       (:rt0 :precedes :rt3)))
		    ({^ verb} #(external aspect-choice-1))
		    (simple no)
		    (time-frame present)
		    (verb-aspect past-participle)
		    (first-verb {^ have-1})
		    (tensed-feature {^ have-1})
		    (have-1 ((tense present)))
		    (beg ((ending past-participle)))
		    (have-2 ((ending root))))

		   ;; tense 27
		   ;; I will have been going to have taken the bus.[a]
		   ;; The bus will have been going to have been taken by me.[p]???
		   ((tense tense-27)
		    (tpattern ((:st :precedes  :rt2)
			       (:rt1 :precedes :rt2)
			       (:rt1 :precedes :rt0)
			       (:rt0 :precedes :rt3)))
		    ({^ verb} #(external aspect-choice-1))
		    (simple no)
		    (time-frame future)
		    (verb-aspect past-participle)
		    (first-verb {^ aux})
		    (tensed-feature {^ aux})
		    (aux ((lex "will") (cat modal)))
		    (have-1 ((ending root)))
		    (beg ((ending past-participle)))
		    (have-2 ((ending root))))

		   ;; tense 28
		   ;; I was going to have been taking the bus.[a]
		   ;; The bus was going to have been being taken by me.[p]
		   ((tense tense-28)
		    (tpattern ((:rt1 :precedes :st) (:rt1 :precedes :rt0)
			       (:rt0 :precedes :rt2) (:rt0 :during :et)))
		    (aspect ((alt aspect-28 (event process))))
		    (simple no)
		    (time-frame past)
		    (verb-aspect present-participle)
		    (first-verb {^ beg})
		    (tensed-feature {^ beg}) 
		    (be-1 ((ending past-participle)))
		    (beg ((tense past)))
		    (have-2 ((ending root))))

		   ;; tense 29
		   ;; I am going to have been taking the bus.[a]
		   ;; The bus is going to have been being taken by me.[p]
		   ((tense tense-29)
		    (tpattern ((:rt1 :equals :st) (:rt1 :precedes :rt0)
			       (:rt0 :precedes :rt2) (:rt0 :during :et)))
		    (aspect ((alt aspect-29 (event process))))
		    (simple no)
		    (time-frame present)
		    (verb-aspect present-participle)
		    (first-verb {^ beg})
		    (tensed-feature { ^ beg})
		    (beg ((tense present)))
		    (have-2 ((ending root)))
		    (be-1 ((ending past-participle))))

		   ;; tense 30
		   ;; I will be going to have been taking the bus.[a]
		   ;; The bus will be going to have been being taken by me.[p]
		   ((tense tense-30)
		    (tpattern ((:st :precedes :rt1) (:rt1 :precedes :rt0)
			       (:rt0 :precedes :rt2) (:rt0 :during :et)))
		    (aspect ((alt aspect-30 (event process))))
		    (simple no)
		    (time-frame future)
		    (verb-aspect present-participle)
		    (first-verb {^ aux})
		    (tensed-feature {^ aux})
		    (aux ((lex "will") (cat modal)))
		    (beg ((ending root)))
		    (have-2 ((ending root)))
		    (be-1 ((ending past-participle))))

		   ;; tense 31
		   ;; I had been going to be taking the bus.[a]
		   ;; The bus had been going to be being taken by me.[p]
		   ((tense tense-31)
		    (tpattern ((:rt2 :precedes :st) (:rt1 :precedes :rt2)
			       (:rt1 :precedes :rt0) (:rt0 :during :et)
			       (:st :none :rt3) (:rt0 :none :rt3) (:rt1 :none :rt3)
			       (:rt2 :none :rt3) (:et :none :rt3)))
		    (aspect ((alt (process event))))
		    (simple no)
		    (time-frame past)
		    (verb-aspect present-participle)
		    (first-verb {^ have-1})
		    (tensed-feature { ^ have-1})
		    (have-1 ((tense past)))
		    (beg ((ending past-participle)))
		    (be-1 ((ending root))))

		   ;; tense 32
		   ;; I have been going to be taking the bus.[a]
		   ;; The bus has been going to be being taken by me.[p]
		   ((tense tense-32)
		    (tpattern ((:rt2 :equals :st) (:rt1 :precedes :rt2)
			       (:rt1 :precedes :rt0) (:rt0 :during :et)
			       (:st :none :rt3) (:rt0 :none :rt3) (:rt1 :none :rt3)
			       (:rt2 :none :rt3) (:et :none :rt3)))
		    (aspect ((alt (process event))))
		    (simple no)
		    (time-frame present)
		    (verb-aspect present-participle)
		    (first-verb {^ have-1})
		    (tensed-feature { ^ have-1})
		    (have-1 ((tense present)))
		    (beg ((ending past-participle)))
		    (be-1 ((ending root))))

		   ;; tense 33
		   ;; I will have been going to be taking the bus.[a]
		   ;; The bus will have been going to be being taken by me.[p]
		   ((tense tense-33)
		    (tpattern ((:st :precedes :rt1) (:rt1 :precedes :rt2)
			       (:rt1 :precedes :rt0) (:rt0 :during :et)
			       (:st :none :rt3) (:rt0 :none :rt3) (:rt1 :none :rt3)
			       (:rt2 :none :rt3) (:et :none :rt3)))
		    (aspect ((alt (process event))))
		    (simple no)
		    (time-frame future)
		    (verb-aspect present-participle)
		    (first-verb {^ aux})
		    (tensed-feature {^ aux})
		    (aux ((lex "will") (cat modal)))
		    (have-1 ((ending root)))
		    (beg ((ending past-participle)))
		    (be-1 ((ending root))))
	 
		   ;; tense 34
		   ;; I had been going to have been taking the bus.[a]
		   ;; The bus had been going to have been being taken by me.[p]
		   ((tense tense-34)
		    (tpattern ((:rt2 :precedes :st)
			       (:rt1 :precedes :rt2)
			       (:rt1 :precedes :rt0)
			       (:rt0 :precedes :rt3)
			       (:rt0 :during :et)))
		    (aspect ((alt (process event))))
		    (simple no)
		    (time-frame past)
		    (verb-aspect present-participle)
		    (first-verb {^ have-1})
		    (tensed-feature { ^ have-1})
		    (have-1 ((tense past)))
		    (beg ((ending past-participle)))
		    (have-2 ((ending root)))
		    (be-1 ((ending past-participle))))

		   ;; tense 35
		   ;; I have been going to have been taking the bus.[a]
		   ;; The bus has been going to have been being taken by me.[p]
		   ((tense tense-35)
		    (tpattern ((:st :equals :rt2)
			       (:rt1 :precedes :rt2)
			       (:rt1 :precedes :rt0)
			       (:rt0 :precedes :rt3)
			       (:rt0 :during :et)))
		    (aspect ((alt (process event))))
		    (simple no)
		    (time-frame present)
		    (verb-aspect present-participle)
		    (first-verb {^ have-1})
		    (tensed-feature {^ have-1})
		    (have-1 ((tense present)))
		    (beg ((ending past-participle)))
		    (have-2 ((ending root)))
		    (be-1 ((ending past-participle))))

		   ;; tense 36
		   ;; I will have been going to have been taking the bus.[a]
		   ;; The bus will have been going to have been being taken by me.[p]
		   ((tense tense-36)
		    (tpattern ((:st :precedes :rt1)
			       (:rt1 :precedes :rt2)
			       (:rt1 :precedes :rt0)
			       (:rt0 :precedes :rt3)
			       (:rt0 :during :et)))
		    (aspect ((alt (process event))))
		    (simple no)
		    (time-frame future)
		    (verb-aspect present-participle)
		    (first-verb {^ aux})
		    (tensed-feature {^ aux})
		    (aux ((lex "will") (cat modal)))
		    (have-1 ((ending root)))
		    (beg ((ending past-participle)))
		    (have-2 ((ending root)))
		    (be-1 ((ending past-participle))))
		   ))

	     (alt modality
		  (:demo "what modality is used w/ this verb?")
		  (
		   ((modal-present no)
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
		    (tensed-feature {^ aux})
		    (first-verb ((ending root)))
		    (alt inference-time (:index time-frame)
			 (((time-frame present)
			   (aux ((lex "must") (cat modal))))
			  ((time-frame future)
			   ;; there is already a will 
			   )
			  ((time-frame past)
			   (aux ((lex "must've") (cat modal))))
			  )))
		   ((epistemic-modality possible)
		    (tensed-feature {^ aux})
		    (first-verb ((ending root)))
		    (alt (:index time-frame)
			 (((time-frame present)
			   (aux ((lex "can") (cat modal))))
			  ((time-frame future)
			   (aux ((lex "can") (cat modal))))
			  ((time-frame past)
			   (aux ((lex "could have")))))))
		   ((epistemic-modality given)
		    (tensed-feature {^ aux})
		    (first-verb ((ending root)))
		    (aux ((cat modal)
			  (lex {^ ^ epistemic-modality}))))
		   ((deontic-modality duty)
		    (tensed-feature {^ aux})
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
		    (tensed-feature {^ aux})
		    (first-verb ((ending root)))
		    (aux ((cat modal)
			  (lex {^ ^ deontic-modality}))))
		   ((modality given)
		    (tensed-feature {^ aux})
		    (first-verb ((ending root)))
		    (aux ((cat modal)
			  (lex {^ ^ modality}))))
		   ))
	
	     ;; Now, the tense feature should be selected.  Deal
	     ;; with voice, interrogative and polarity.
	     (alt voice-1 (:index voice) 
		  (:wait {^ event lex})
		  (
		   ;; First: very special case of "to be"
		   ;; passive is "to be" - no auxiliary for negation and question
		   ((event ((lex "be")))
		    (alt simple-be (:index simple)
			 ;; for simple tenses, don't add "do"
			 (((simple yes)
			   ;; (event {^ first-verb})
			   (event ((cat verb)))
			   (event {^ tensed-feature}))
			  ((simple no)
			   (event ((ending {^ ^ verb-aspect})))))))
		   ;; then special case of all copulae: passive is themselves
		   ;; and all other verbs at active
		   ((alt (((voice active))
			  ((copula #(under yes)))))
		    (alt simple-do (:index simple) (:wait {^ simple})
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
					 ;; @@TODO: not good with modals
					 (interrogative wh)
					 ({^ scope} ((synt-funct #(under subject))))
					 (aux none)
					 (event { ^ tensed-feature}))
					((interrogative interrogative)
					 (aux ((lex "do") (cat verb)))
					 (aux {^ tensed-feature})
					 (event ((ending root))))
					((alt simple-insistence (:index insistence)
					      (((insistence no))
					       ((insistence #(under yes))
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
		    (alt passive-simple (:index simple) (:wait {^ simple})
			 ( ;; no auxilliary necessarily
			  ((simple yes)
			   (event ((ending past-participle)))
			   (tensed-feature {^ be-2}))
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
	     (alt be-1 (((be-1 none))
			((be-1 given)
			 (be-1 ((lex "be")
				(cat verb))))))

	     (alt be-2 (((be-2 none))
			((be-2 given)
			 (be-2 ((lex "be")
				(cat verb))))))
      
	     (alt have-1 (((have-1 none))
			  ((have-1 given)
			   (have-1 ((lex "have")
				    (cat verb))))))

	     (alt have-2 (((have-2 none))
			  ((have-2 given)
			   (have-2 ((lex "have")
				    (cat verb))))))

	     (alt beg (((beg none))
		       ((beg given)
			(beg ((lex "be")
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
		    ;; (tensed-feature ((cat {^ ^ ^ fronted-aux cat})))
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
	     %np%
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
			 (index ((fset (concept animate gender person number
						countable))
				 (concept {^ ^ ^ concept})
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
		  (:demo "Is this a common noun, a pronoun or a proper noun?")
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
		    (definite ((alt (yes #(under no)))))
		    (countable ((alt (yes #(under no)))))
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
			 (:demo "Is this a personal, demonstrative, question, relative ~
                          or quantified pronoun?")
			 (((cat personal-pronoun)
			   (np-type personal-pronoun)
			   ;; are gender and person specific, default is third masculine.
			   (head ((pronoun-type personal))) ;; arg to morph.
			   (syntax
			    ((alt (:index person)
				  (((person third))
				   ((person #(under first)) (animate yes))
				   ((person #(under second)) (animate yes))))
			     (alt gender
				  (:index gender)
				  (((gender #(under neuter)) (animate no))
				   ((gender #(under masculine)) (animate yes))
				   ((gender #(under feminine)) (animate yes))
				   ((gender nil))))))
			   (definite yes)
			   (semantics ((describer none))))
			  ((cat demonstrative-pronoun)
			   (np-type demonstrative-pronoun)
			   (head ((pronoun-type demonstrative)))
			   (syntax ((definite yes)
				    (person third)
				    (distance ((alt (far #(under near)))))))
			   (semantics ((describer none))))
			  ((cat relative-pronoun)
			   (np-type relative-pronoun)
			   (head ((pronoun-type relative)))
			   (syntax ((person third))))
			  ((cat question-pronoun)
			   (np-type question-pronoun)
			   (head ((pronoun-type question)))
			   (syntax ((person third)))
			   (semantics ((describer none))))
			  ;; - describers come after head: "something green"
			  ((cat quantified-pronoun)
			   (np-type quantified-pronoun)
			   (head ((pronoun-type quantified)))
			   (syntax ((person third) (definite no)))
			   (alt (((semantics ((describer none))))
				 ((semantics ((describer given)))
				  (pattern (head describer dots))))))
			  ))
		    ;; Case: subjective, objective, possessive, reflexive
		    ;; - - - - - - - - - - - - - - - - - - - - - - - - -
		    (syntax 
		     ((alt pronoun-case 
			   (:demo "Is the pronoun subject, object, possessive ~
                            or reflexive?")
			   (((case subjective))
			    ((case given)
			     (case ((alt (objective possessive reflexive))))))))))

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
		   ((syntax ((number #(under plural)))))))


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
	      ((alt describer (:bk-class ao)
		    (:demo "Are there describers?")
		    (((describer none))
		     ((describer given)
		      ({^ pattern} (dots describer dots head dots))
		      (describer
		       ((alt describer-cat (:index cat)
			     (((cat adj)) ;; either a single adj
			      ((cat ap) ;; or an ap with no modifiers
			       (describer none)
			       (qualifier none))
			      ((cat list)) ;; no check on lists
			      ((cat verb)
			       (ending past-participle)
			       (modifier-type objective))
			      ((cat verb)
			       (ending present-participle)
			       (modifier-type subjective)))))))
		     ;; Use a describer to bring an AO modification on head
		     (({ao} given)
		      (index ((concept {ao partic carrier concept})))
		      (alt describer-ao (:bk-class ao)
			   (:wait {ao ao-conveyed})
			   ((({ao ao-conveyed} given))
			    ((describer ((ao {ao})
					 (concept {ao concept})
					 (cat ap)))
			     ({^ pattern} (dots describer dots head dots))
			     ({ao ao-conveyed} describer)))))))))
	  

	     ;; CLASSIFIER =================================================
	     ;; Allow for multiple classifiers (complex nouns)
	     (semantics
	      ((alt classifier 
		    (:demo "Is there a classifier?")
		    (((classifier none))
		     ((classifier given)
		      ({^ pattern} (dots classifier head dots))
		      (classifier
		       ((alt (:index cat)
			     (((cat noun))
			      ((cat #(under list))) ;; no check on lists
			      ((cat #(under verb))
			       (ending present-participle)
			       (modifier-type subjective)))))))))))

	     ;; QUALIFIER ==================================================
	     (semantics
	      ((alt qualifier
		    (:demo "Is there a qualifier? Is it a PP or ~
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
			      ((cat #(under list))) ;; an heterogeneous list of qualifiers
			      ;; "The elephant that came to brunch"
			      ;; "The game he played so wonderfully"
			      ((cat #(under clause))
			       (mood relative))
			      ;; "The time remaining before the buzzer"
			      ((cat #(under clause))
			       (subject none)
			       (mood present-participle))
			      ;; "The fare specified on the ticket"
			      ;; Debatable whether these are adjectives or
			      ;; passive-participles - tests in general are:
			      ;; If can add very - is an adjective
			      ;; If can change "is" by "seems" or "remains" - is an adj
			      ;; In general, want the ap to have a complement except if
			      ;; there is a reason why not.
			      ((cat #(under ap)))
			      ;; "The game to be played tomorrow"
			      ((cat #(under clause))
			       (subject none)
			       (mood infinitive)))))))))))
		  

	     ;; POSSESSIVE MARKER ==========================================
	     (alt np-case
		  (:index (syntax case))
		  (:demo "Is this a possessive NP?")
		  (((syntax ((case subjective))))
		   ((syntax ((case given)))
		    (syntax ((case ((alt (objective possessive reflexive))))))
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

	    ;; ==============================================================
	    ;; 04 CAT AP : for adjectival phrases ---------------------------
	    ;; ==============================================================
	    ;; ***** Do comparative constructs here
	    ((cat simple-ap)
	     (complex none)
	     (generic-cat ap)
	     (head ((cat adj)
		    (concept {^ ^ concept})
		    (polarity {^ ^ polarity})
		    (lex {^ ^ lex})
		    (ao {^ ^ ao})))
	     ;; "light blue" (light is the classifier)
	     (alt (((classifier none))
		   ((classifier given)
		    (classifier ((cat ((alt (adj #(under noun))))))))))
	     ;; "visible in the cutaway view" (qualifier)
	     (alt (((qualifier none))
		   ((qualifier given)
		    (qualifier ((cat pp))))))
	     ;; modifier is an adverb: can be intensifier or detensifier
	     (alt (((modifier none))
		   ((modifier given)
		    (modifier ((cat adv))))))

	     (pattern (modifier classifier head qualifier)))
      
	    ;; ==============================================================
	    ;; 05 CAT PP : for prepositional phrases ------------------------
	    ;; ==============================================================
	    ((cat simple-pp)
	     (complex none)
	     (generic-cat pp)
	     (pattern (prep np))
	     (prep ((cat prep) (lex given)))
	     (np ((alt pp-np-cat 
		       (((cat #(under relpro)))
			((cat np))))))
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
		   ((cardinal given)
		    (cardinal ((cat cardinal))))))

	     (alt (((ordinal none))
		   ((ordinal given)
		    (ordinal ((cat ordinal))))))

	     ;; Make implication distance ==> demonstrative
	     (opt DISTANCE-DET
		  ((distance {^ syntax distance})
		   (distance given)
		   (cat demonstrative-det)))

	     (alt det
		  (:index cat)
		  (:demo 
		   "Is this a demonstrative, possessive or regular determiner?")
		  (((cat #(under demonstrative-det))
		    (syntax ((fset (definite countable number distance))
			     (definite yes)))
		    (head ((cat article))) ;; don't do anything on it at morphology.
		    (alt distance 
			 (:index (syntax distance))
			 (:demo "Is this a reference to a near or to a far object?")
			 (((syntax ((distance near)))
			   (alt (:index (syntax number))
				(((syntax ((number singular)))
				  (head ((lex "this"))))
				 ((syntax ((number plural)))
				  (head ((lex "these")))))))
			  ((syntax ((distance #(under far))))
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
				  (((person #(under first))
				    (alt (:index number)
					 (((number singular)
					   (lex "my"))
					  ((number plural)
					   (lex "our")))))
				   ((person #(under second))
				    (lex "your"))
				   ((person third)
				    (alt (:index number)
					 (((number singular)
					   (alt (:index gender)
						(((gender #(under masculine))
						  (lex "his"))
						 ((gender #(under feminine))
						  (lex "her"))
						 ((gender neuter)
						  (lex "its")))))
					  ((number #(under plural))
					   (lex "their"))))))))))
			  ;; Regular NP will get an 's
			  ((possessor ((cat np)
				       (syntax ((case possessive)))))))))


		   ((cat regular-det)
		    (head ((cat article))) ;; don't do anything on it more.
		    (alt determiner 
			 (:demo "Choose between A, THE and nothing")
			 (((syntax ((countable yes)
				    (definite yes)))
			   (head ((lex "the"))))
			  ((syntax ((countable #(under no)))) (gap yes))
			  ((syntax ((definite #(under no))
				    (countable yes)))
			   (alt (((syntax ((number singular)))
				  (head ((lex "a"))))
				 ((syntax ((number #(under plural))))
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
		  (:demo"How many elements are there in the list?")
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
			   ;; all we have in the input is agent/medium etc. (use :wait)
			   ;; Want to test on subject not agent. *****
			   ;; How do you do ellipsis of VP without a VP in the grammar *****
			   ({^ constituent1 process lex} {^ ^ ^ ^ constituent2 process lex})
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
	    ;; CAT SEGMENT
	    ;; ==============================================================
	    ;; A segment is a paragraph-length unit where clauses can be
	    ;; combined by:
	    ;; - Sequence
	    ;; - Coordination
	    ;; - Subordination
	    ;; Input is of the form:
	    ;; 

	    ;; ==============================================================
	    ;; LEXICON ======================================================
	    ;; ==============================================================

	    ;; ==============================================================
	    ;; Examples of lexical entries for adjs
	    ;; ==============================================================
	    ;; AO is composed by the NP grammar to the particular entity modified
	    ;; by the adjective.
	    ((cat adj)
	     (alt adj-concept (:index concept)
		  (((concept none)
		    (lex given))
		   ((concept none)
		    (lex ""))
		   ((concept c-difficulty)
		    (alt (:bk-class ao)
			 (:wait {ao ao-conveyed})
			 (((ao ((orientation +)))
			   (polarity +)
			   (lex ((ralt ("hard" "difficult" "tough")))))
			  ((ao ((orientation +)))
			   (polarity -)
			   (lex "easy")))))
		   ((concept c-team-rating)
		    (ao ((scale s-team-rating)
			 (alt (:bk-class ao)
			      (:wait {ao ao-conveyed})
			      (((orientation +)
				({^ lex} ((ralt ("mighty" "top-notch")))))
			       ((orientation -)
				({^ lex} ((ralt ("hapless" "rock-bottom"))))))))))
		   ((concept c-full)
		    (alt (:bk-class ao)
			 (:wait {ao ao-conveyed})	     
			 (((ao ((orientation +)))
			   (polarity +)
			   (lex "full"))
			  ((ao ((orientation +)))
			   (polarity -)
			   (lex "empty"))))))))

	    ;; ==============================================================
	    ;; Examples of verbs
	    ;; ==============================================================
	    ;; Lexicon for verbs: mapping concept - lex + connotations
	    ((cat lex-verb)
	     (alt verbal-lexicon (:index concept) 
		  (((concept c-game-result)
		    (alt game-result-lex (:bk-class (ao manner))
			 (:wait ({ao ao-conveyed} {manner manner-conveyed}))
			 ((({AO} ((concept c-team-rating)
				  ({^ ao partic carrier concept}
				      ((alt ({partic agent concept}
						     {lex-roles winner concept}))))
				  (orientation -)
				  (ao-conveyed verb)))
			   (lex ((ralt ("stun" "surprise")))))
	      
			  (({circum manner} ((concept c-narrow)
					     (manner-conveyed verb)))
			   (lex ((ralt ("edge" "nip")))))
	      
			  ;; Neutral verbs
			  ((lex ((ralt ("beat" "defeat" "down"))))))))

		   ((concept c-move)
		    (lex ((ralt ("walk" "run"))))))))

	    ;; ==============================================================
	    ;; Examples of adverbs
	    ;; ==============================================================
	    ((cat adv)
	     (generic-cat adv)
	     (complex none)
	     (alt adv-type (:index cat)
		  (((cat #(under detensifier))
		    (lex ((ralt ("quite" "pretty" "rather" "somehow")))))
		   ((cat #(under intensifier))
		    (lex ((ralt ("very" "extremely")))))
		   ((cat adv)
		    (alt adv-concept (:index concept)
			 (((concept c-narrow)
			   (lex "narrowly"))
			  ((concept c-team-rating)
			   ;; ***** Express that surprise inverts the AO of verb
			   (lex "surprisingly"))
			  ((lex given))
			  ((lex ""))))))))

    
	    ;; ==============================================================
	    ;; Misc categories ignored by the grammar and recognized by the
	    ;; morphology component.
	    ((cat phrase))
	    ((cat article))
	    ((cat pronoun))
	    ((cat cardinal))
	    ((cat ordinal))
	    ((cat score)
	     (hi ((cat cardinal)
		  (value {^ ^ win})))
	     (lo ((cat cardinal)
		  (value {^ ^ lose})))
	     (to ((cat phrase)
		  (lex "-")))
	     (pattern (hi to lo)))
	    ))))
	    
  (format t "~%gr10 installed.~%")
  (values))
