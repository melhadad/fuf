;;; -*- Mode:Lisp; Syntax:Common-Lisp; -*-
;;; ------------------------------------------------------------
;;; File        : GR8.L
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

;; Utilities for temporal patterns  (tpattern unifier)
;; (require "tpat")

(defun aspect-choice-1 (path)
  (let ((alt-name (car (path-last path))))
    `((alt ,alt-name
       (((aspect event)
	 (tpattern ((:rt0 :equals :et))))
	((aspect ((alt (stative process))))
	 (tpattern ((:rt0 :during :et))))))))
  )

;; This function is used in #external constructs to check if a certain
;; construct is specified in the input or not.
;; ***** Re-do role-exists based on each process type
(defun role-exists (path)
  "An external function to test that the role of a clause is given in
  input.
  External functions return a piece of grammar."
  ;; All roles can be nps
  (let* ((role (car (path-last path)))  ;; the role being inspected
	 (msg  (format nil "Is there a role ~s in the input" role))
	 (branches '(((cat #(under list))) ;; a list is opaque to the grammar
		     ((cat np)
		      (alt (((lex given))
			    ((head given))
			    ((cat #(under pronp)))
			    ((complex given)
			     (distinct given))
			    ((gap given)))))
		     nil)))                   ;; all optional roles (fix it ****)

    ;; Attributes can be pps too:
    (when (member role '(attribute))
      (setf branches (cons '((cat pp)) branches)))

    ;; These roles can be adjectives (this is the default):
    (when (member role '(attribute identifier identified))
      (setf branches (cons '((cat ap)) branches)))

    ;; These roles can be clauses:
    (when (member role '(agent medium phenomenon carrier identifier
			       identified))
      (setf branches (append branches '( ((cat clause)) ))))

    `((alt role-exists (:index cat) (:bk-class role-cat)
	,branches))))
  

;; NOTE: all non-implemented features, or things to do are marked in
;; comments with a ***** sign.

(defun gsetup8 ()
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

  ;; TRANSITIVITY SYSTEM:
  ;; Process-type     | Nuclear Semantic roles (specified with fset)
  ;; -----------------+-------------------------------------------------
  ;; - material       | agent, medium, benef
  ;; - mental         | processor, phenomenon
  ;; - relational     | comes as relation-mode attributive or equative.
  ;;                  | when attributive    | carrier, attribute
  ;;                  | when equative       | identified, identifier

  (define-feature-type process-class (material mental relational))
  (define-feature-type material (action event natural-phenomenon))
  (define-feature-type mental   (perception verbal thinking))
  (define-feature-type relational (ascriptive possessive circumstantial))
  (define-feature-type circumstantial (locative temporal))
  (define-feature-type ascriptive (attributive equative))
  (define-feature-type possessive (possessive-attributive
				   possessive-equative))
  (define-feature-type locative (locative-attributive locative-equative))
  (define-feature-type temporal (temporal-attributive temporal-equative))

  ;; Notes on treatment of relational processes:
  ;; relation-mode determines whether relation is symmetrical or not (can it
  ;; be passivized).
  ;; For circumstantial processes, relation can be expressed in the verb or
  ;; in the participant (in which case it is a PP with the prep expressing
  ;; the relation).
  ;; relation-mode   | attributive, equative
  ;; circumstance-as | process, participant

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
  ;; process-class    :    action, mental, attributive, equative           
  ;; voice-class      : 	 middle, non-middle                              
  ;; transitive-class : 	 neutral, intransitive, transitive, bitransitive 
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

	     (verb {^ process})

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
			   (verb ((ending root))))

			  ((mood present-participle)
			   (verb ((ending present-participle)))
			   (modality none)
			   (epistemic-modality none)
			   (deontic-modality none))

			  ((mood infinitive)
			   (modality none)
			   (epistemic-modality none)
			   (deontic-modality none)
			   (verb ((ending infinitive)))))))))

      
	     ;; TRANSITIVITY SYSTEM
	     (alt process (:index process-type)
		  (:demo "Is the process material, mental or relational?")

		  ;; Process-type: material, mental, or relation
		  ;; -------------------------------------------
	
		  ;; Process 1: material --> actions, events, natural phenomena
		  ;; inherent cases      --> agent, medium, benef.
		  ;; all are optional, but at least one of medium or agent
		  ;; must be present.
	
		  (((process-type material)
		    (agent #(external role-exists))
		    (medium #(external role-exists))
		    (benef  #(external role-exists))
		    (process ((process-class material)
			      (lex given)))) ;;there must be a verb given

		   ;; Process 2: mental --> perception, reaction, cognition, verbalization
		   ;; inherent cases    --> processor, phenomenon
		   ;; processor is required, phenomenon is optional.
		   ((process-type mental)
		    (process ((process-class mental)
			      (lex given)))
		    (processor #(external role-exists))
		    (phenomenon #(external role-exists)))

		   ;; Process 3: relational
		   ;; This one is specialized into ascriptive, possessive and
		   ;; circumstantial. 
		   ;; inherent cases depend on relation-mode:
		   ;; If attributive: inherent cases --> carrier, attribute
		   ;; If equative inherent cases     --> identified, identifier
		   ;; (mode is equative if clause can be passivized)
		   ((process-type relational)
		    (process ((process-class relational)))
		    (alt relational-subtype (:index process-type)

			 ;; ascriptive: assign a property to an object
			 (((process-type ascriptive)
			   (alt relation-mode (:index relation-mode)
				(((relation-mode attributive)
				  ;; In general can have a full verb otw just be
				  (alt verb-be-attributive
				       (((verb ((lex "be")
						(subject-clause infinitive)
						(object-clause none)
						(voice-class non-middle)
						(transitive-class neutral))))
					((verb ((process-class attributive)
						(attributive yes)
						(lex given))))))
				  (carrier #(external role-exists))
				  (attribute #(external role-exists)))
				 ((relation-mode equative)
				  ;; "A book is an object" or "The president is the chief"
				  (alt verb-be-equative
				       (((verb ((lex "be")
						(voice-class non-middle)
						(subject-clause that)
						(object-clause present-participle)
						(transitive-class neutral))))
					((verb ((lex given)
						(attributive no)
						(process-class equative))))))
				  (identified #(external role-exists))
				  (identifier #(external role-exists))))))

			  ;; possessive relation
			  ((process-type possessive)
			   (alt relation-mode (:index relation-mode)
				(((relation-mode attributive)
				  ;; Default verb is "has"
				  (alt verb-have-possessive
				       (((verb ((lex "have")
						(subject-clause infinitive)
						(object-clause none)
						(voice-class non-middle)
						(transitive-class neutral))))
					((verb ((lex given)
						(attributive yes)
						(process-class possessive-attributive))))))
				  (owner {^ carrier})
				  (possessor {^ carrier})
				  (owned {^ attribute})
				  (possessed {^ attribute})
				  (carrier #(external role-exists))
				  (attribute #(external role-exists)))
				 ;; equative can be passivized
				 ((relation-mode equative)
				  ;; Default verb is "own"
				  (alt verb-own-possessive
				       (((verb ((lex "own")
						(subject-clause none)
						(object-clause none)
						(voice-class non-middle)
						(transitive-class transitive))))
					((verb ((lex given)
						(attributive no)
						(process-class possessive-equative))))))
				  (owner {^ identified})
				  (possessor {^ identified})
				  (owned {^ identifier})
				  (possessed {^ identifier})
				  (identified #(external role-exists))
				  (identifier #(external role-exists))))))

			  ;; circumstantial relation
			  ((process-type circumstantial)
			   (alt relation-mode (:index relation-mode)
				(((relation-mode attributive)
				  ;; verb is "be" by default or a given verb with
				  ;; attributive meaning (eg "start")
				  ;; "The meeting starts at 9" or "The book is in the box"
				  (opt location-synonyms
				       ((process-type locative)
					(located {^ carrier})
					(location {^ attribute})))
				  (opt temporal-synonyms
				       ((process-type temporal)
					(located {^ carrier})
					(time {^ attribute})))
				  (alt verb-circumstantial (:index {verb lex})
				       (((verb ((lex given)
						(process-class circumstantial)
						(attributive yes))))
					((verb ((lex "be")
						(attributive yes)
						(subject-clause infinitive)
						(object-clause none)
						(voice-class non-middle)
						(transitive-class neutral))))))
				  (carrier #(external role-exists))
				  (attribute #(external role-exists)))
				 ((relation-mode equative)
				  ;; "The box contains a book" or "The lecture ends the
				  ;; meeting" 
				  (opt location-synonyms
				       ((process-type locative)
					(located {^ identified})
					(location {^ identifier})))
				  (opt temporal-synonyms
				       ((process-type temporal)
					(located {^ identified})
					(time {^ identifier})))
				  (alt verb-be-equative
				       (((verb ((lex "be")
						(voice-class non-middle)
						(subject-clause that)
						(object-clause present-participle)
						(transitive-class neutral))))
					((verb ((lex given)
						(process-class circumstantial)
						(attributive no))))))
				  (identified #(external role-exists))
				  (identifier #(external role-exists)))))))))))
		 
 

	     ;; VOICE CHOICE --> operative, middle, receptive.
	     ;; Operative =~ active
	     ;; Receptive =~ passive
	     ;; Middle    = sentences with only one participant ("the sun shines")
	     ;; Choice middle/non-middle is based on verb classification
	     ;; it is also based on the interaction verb/participant but we don't
	     ;; do that yet.
	     ;; Choice receptive/operative is based on focus (using pattern).
	     ;; The voice alternation does the mapping semantic -> syntactic roles
	     ;; The next alternation does the mapping scope -> syntactic role
	     ;; for interrogative and relative clauses.
	     (alt voice 
		  (:index (verb voice-class))
		  (:demo "Is the verb middle or non-middle?")
		  (:bk-class voice-class)
		  (((verb ((voice-class non-middle)))
		    (alt non-middle-voice 
			 (:index voice)
			 (:demo "Is the clause passive or active? This ~
                           determines the mapping deep to surface roles")
			 (((voice operative)
			   (verb ((voice active)))
			   (alt (:index process-type)
				(((process-type material)
				  (subject {^ agent})
				  (object {^ medium})
				  (iobject {^ benef}))
				 ((process-type mental)
				  (subject {^ processor})
				  (object  {^ phenomenon})
				  (iobject none))
				 ((process-type relational)
				  (relation-mode equative)
				  (subject {^ identified})
				  (object {^ identifier})
				  (object ((syntax ((case subjective)))))
				  (iobject none))
				 ((process-type relational)
				  (relation-mode attributive)
				  (subject {^ carrier})
				  (object  {^ attribute})
				  (iobject none)))))
			  ((voice receptive)
			   ;; Warning: a receptive is not always translated into a
			   ;; passive verb!!!: "this string won't tie" (no actor).
			   ;; ignore that in this grammar
			   (alt receptive-mapping (index process-type)
				(((process-type material)
				  (verb ((voice passive)))
				  (alt explicit-agent
				       ;; Is there an explicit agent?
				       ;; well, actually should distinguish between
				       ;; "the door opened" and "the door was opened".
				       (((agent none)
					 (by-obj none))
					((agent ((gap given) (gap yes)))
					 (by-obj none))
					((agent given)
					 (by-obj ((np {^ ^ agent}))))))
				  (alt passive-subject
				       ;; subject is either benef or medium
				       ;; "A book is given to mary by john"
				       ;; "Mary is given a book by John"
				       (((subject {^ medium})
					 (iobject {^ benef})
					 (object none))
					((subject {^ benef})
					 (object  {^ medium})
					 (iobject none)))))
				 ((process-type mental)
				  (verb ((voice passive)))
				  (subject {^ phenomenon})
				  (alt explicit-processor
				       ;; is there an explicit processor?
				       (((processor ((lex none))))
					((processor ((lex given)))
					 (by-obj ((np {^ ^ processor}))))))
				  (iobject none))
				 ;; cannot have an attributive process in receptive voice.
				 ((process-type relational)
				  (relation-mode equative)
				  (subject {^ identifier})
				  ;; "be" is symmetrical - don't need to passivize
				  ;; other verbs have a passive
				  (alt (((verb ((lex "be")
						(voice active)))
					 (object ((syntax ((case subjective)))))
					 (object  {^ identified}))
					((verb ((voice passive)))
					 (by-obj ((np {^ ^ identified})))
					 (object none))))
				  (iobject none))))))))

		   ((verb ((voice-class middle)))
		    (voice middle)
		    ;; We must have only one participant, the medium that is the subject.
		    (alt (:index process-type) 
			 ;; main case is the subject.
			 ;; cannot have a middle verb with process-type relation.
			 (((process-type material)
			   ;; (agent none)            ;; ***** Ergative construct to do
			   ;; agent make medium verb.
			   (benef none)
			   (subject {^ medium}))
			  ((process-type mental) ;; ??? Are there mental/middle
			   (subject {^ processor}))))
		    ;; cannot have a relational with only one participant...
		    (object none)
		    (iobject none))))

	     ;; DISPLACED CONSTITUENT PROCESSING
	     ;; for interrogative and relative mood, the constituent under scope
	     ;; is displaced.  We handle it in this alternation.
	     (alt scope-place (:index mood)
		  (((mood yes-no)
		    ;; No displaced component except inversion of verb/subject
		    (cset ((- fronted-aux)))
		    (pattern (dots fronted-aux fronted-not start dots)))
		   ((mood wh)
		    ;; scope is like in relative clauses.  Contains the name
		    ;; of a top-level role on which the question is asked
		    ;; as in: (scope ((role carrier)))
		    ;; Two cases handled: corresponding comp is an NP or a PP.
		    ;; ***** Should check for other cats (adverb, ap).
		    ;; ***** Should check features of verb to decide whether
		    ;; to use a pp or an np as question element
		    ;; ***** Should do questions like "to which house did you
		    ;; go?" (scope is sub-constituent) VERY HARD.
		    (alt question-elt-cat (:index (scope cat))
			 (((scope ((cat np)))
			   (question 
			    ((cat question-pronoun)
			     (restrictive {^ ^ scope restrictive})
			     (syntax {^ ^ scope syntax})
			     (semantics ((index {^ ^ ^ scope semantics index})))))
			   (alt question-elt (:index (scope role))
				(:demo "Choosing a question element.")
				(((scope ((role at-loc))) 
				  (scope {^ at-loc-comp np})
				  (question ((cat question-pronoun) (lex "where"))))
				 ((scope ((role location)))
				  (scope {^ location})
				  (alt (((circumstance-as participant)
					 (relation-mode attributive)
					 (question ((cat question-pronoun) (lex "where"))))
					((relation-mode equative)
					 ;; Can be either restrictive or not
					 (question ((restrictive ((alt (no yes))))))))))
				 ((scope ((role to-loc))) 
				  (scope {^ to-loc-comp np})
				  (question ((cat question-pronoun) (lex "where"))))
				 ((scope ((role at-time)))
				  (question ((cat question-pronoun) (lex "when")))
				  (scope {^ at-time-comp np}))
				 ((scope ((role time)))
				  (alt (((circumstance-as participant)
					 (relation-mode attributive)
					 (question ((cat question-pronoun) (lex "when"))))
					((relation-mode equative)
					 (question ((restrictive ((alt (no yes)))))))))
				  (scope {^ time}))
				 ((scope ((role reason)))
				  (question ((cat question-pronoun) (lex "why")))
				  (scope {^ reason-comp}))
				 ((scope ((role manner)))
				  (question ((cat question-pronoun) (lex "how")))
				  (scope {^ manner-comp}))
				 ((scope ((role agent)))
				  (scope {^ agent}))
				 ((scope ((role medium)))
				  (scope {^ medium}))
				 ((scope ((role processor)))
				  (scope {^ processor}))
				 ((scope ((role phenomenon)))
				  (scope {^ phenomenon}))
				 ((scope ((role carrier)))
				  (scope {^ carrier}))
				 ((scope ((role located)))
				  (scope {^ located}))
				 ((scope ((role owner)))
				  (scope {^ owner}))
				 ((scope ((role owned)))
				  (scope {^ owned})
				  (opt ((relation-mode equative)
					(question ((restrictive ((alt (no yes)))))))))
				 ((scope ((role possessor)))
				  (scope {^ possessor}))
				 ((scope ((role possessed)))
				  (scope {^ possessed})
				  (opt ((relation-mode equative)
					(question ((restrictive ((alt (no yes)))))))))
				 ((scope ((role attribute)))
				  (alt (((circumstance-as participant)
					 (relation-mode attributive)
					 (question ((cat question-pronoun) (lex "how"))))
					((circumstance-as process))))
				  (scope {^ attribute}))
				 ((scope ((role identified)))
				  (scope {^ identified}))
				 ((scope ((role identifier)))
				  (question ((restrictive ((alt (no yes))))))
				  (scope {^ identifier}))))
			   (opt ((scope ((cat question-pronoun))))))

			  ;; Scope of question in a PP (embedded alt)
			  ((scope ((cat pp)))
			   (opt ((scope ((np ((cat question-pronoun)))))))
			   (alt embedded-question-elt (:index (scope role))
				(:demo "This is an embedded question.  What ~
                                preposition must be used?")
				(((scope ((role instrument)))
				  (opt ((prep ((lex "with")))))
				  (scope {^ instrument-comp}))
				 ((scope ((role benef)))
				  (scope {^ dative})
				  ({^ dative gap} yes)
				  ({^ benef gap} yes)
				  (prep ((lex {^ ^ verb dative-prep}))))
				 ((scope ((role accompaniment)))
				  (opt ((prep ((lex "with")))))
				  (scope {^ accompaniment-comp}))
				 ((scope ((role to-loc)))
				  (opt ((prep ((lex "to")))))
				  (question ((np ((lex "where")))))
				  (scope {^ to-loc-comp}))
				 ((scope ((role from-loc)))
				  (question ((np ((lex "where")))))
				  (opt ((prep ((lex "from")))))
				  (scope {^ from-loc-comp}))
				 ((scope ((role on-loc)))
				  (question ((np ((lex "where")))))
				  (opt ((prep ((lex "on")))))
				  (scope {^ on-loc-comp}))
				 ((scope ((role in-loc)))
				  (question ((np ((lex "where")))))
				  (opt ((prep ((lex "in")))))
				  (scope {^ in-loc-comp}))
				 ((scope ((role purpose)))
				  (opt ((prep ((lex "for")))))
				  (scope {^ purpose-comp}))
				 ((scope ((role behalf)))
				  (opt ((prep ((lex "for")))))
				  (scope {^ behalf-comp}))))
			   ;; Since scope is a pp it will not put neatly all
			   ;; features under semantics as nps do.  So do it here
			   (scope ((prep {^ ^ prep})
				   (np ((semantics 
					 ((index ((animate {^ ^ ^ ^ animate})
						  (gender  {^ ^ ^ ^ gender})
						  (person  {^ ^ ^ ^ person})
						  (countable {^ ^ ^ ^ countable})
						  (number  {^ ^ ^ ^ number})))))))))
			   (question ((cat pp)
				      (prep {^ ^ prep})
				      (np ((cat question-pronoun)
					   (syntax ((case objective)))
					   (semantics {^ ^ ^ scope np semantics})
					   (syntax {^ ^ ^ scope np syntax}))))))))
		    (pattern (dots question fronted-aux fronted-not start dots)))

		   ;; MOOD RELATIVE
		   ((mood relative)
		    (alt (trace relative) (:index mood) 
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
				  (scope {^ at-loc-comp np})
				  (relative-marker ((cat relpro) (lex "where"))))
				 ((scope ((role at-time)))
				  (relative-marker ((cat rel-pro) (lex "when")))
				  (scope {^ at-time-comp np}))
				 ((scope ((role reason)))
				  (relative-marker ((cat relpro) (lex "why")))
				  (scope {^ reason-comp}))
				 ((scope ((role manner)))
				  (relative-marker ((cat relpro) (lex "how")))
				  (scope {^ manner-comp}))
				 ((scope ((role agent)))
				  (scope {^ agent}))
				 ((scope ((role medium)))
				  (scope {^ medium}))
				 ((scope ((role benef)))
				  (scope {^ benef}))
				 ((scope ((role processor)))
				  (scope {^ processor}))
				 ((scope ((role phenomenon)))
				  (scope {^ phenomenon}))
				 ((scope ((role carrier)))
				  (scope {^ carrier}))
				 ((scope ((role attribute)))
				  (scope {^ attribute}))
				 ((scope ((role identified)))
				  (scope {^ identified}))
				 ((scope ((role identifier)))
				  (scope {^ identifier}))))
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
			   (relative-marker ((cat relative-pronoun)
					     (semantics {^ ^ scope semantics})
					     (syntax {^ ^ scope syntax})))
			   (alt scope-embedded (:index (scope role))
				(:demo "This is an embedded relative.  What ~
                                preposition must be used?")
				(((scope ((role instrument)))
				  (opt ((prep ((lex "with")))))
				  (scope {^ instrument-comp}))
				 ((scope ((role accompaniment)))
				  (opt ((prep ((lex "with")))))
				  (scope {^ accompaniment-comp}))
				 ((scope ((role to-loc)))
				  (opt ((prep ((lex "to")))))
				  (scope {^ to-loc-comp}))
				 ((scope ((role from-loc)))
				  (opt ((prep ((lex "from")))))
				  (scope {^ from-loc-comp}))
				 ((scope ((role on-loc)))
				  (opt ((prep ((lex "on")))))
				  (scope {^ on-loc-comp}))
				 ((scope ((role in-loc)))
				  (opt ((prep ((lex "in")))))
				  (scope {^ in-loc-comp}))
				 ((scope ((role purpose)))
				  (opt ((prep ((lex "for")))))
				  (scope {^ purpose-comp}))
				 ((scope ((role behalf)))
				  (opt ((prep ((lex "for")))))
				  (scope {^ behalf-comp})))))

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


	     ;; Now take care of special treatment of subject
	     ;; clauses (this is the right time because the subject is now bound).
	     (alt subject-mood	(:index mood) 
		  (:demo "Is a subject required or does it need a special treatment?")
		  (((mood finite)
		    (subject given))
		   ((mood non-finite)
		    (alt (:index mood)
			 (((mood infinitive)
			   (opt INF-SUB 
				((subject given)
				 (subject ((cat np) (syntax ((case objective)))))))
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
				  (subject given)
				  (subject ((gap none)))
				  (pattern (dots for start dots))
				  (for ((cat conj) (lex "for"))))
				 ((keep-for no)))))
			  ((mood present-participle)
			   ;; subject is optional or in possessive form
			   (alt subject-subject
				(((subject none))
				 ((subject given)
				  (subject ((cat np)
					    (syntax ((case possessive))))))
				 ((subject ((gap yes)))))))
			  ((mood past-participle)
			   (subject none))
			  ((mood imperative)
			   ;; subject is optional in realization
			   (alt (((subject none))
				 ((subject ((gap yes))))))))))))


	     ;; Syntactic categories of subject/object are compatible with verb?
	     ;; This depends on particular verb, information we will get from the
	     ;; lexicon. So far, we check whether subject-clause and object-clause
	     ;; are ok. 
	     ;; ***** SHOULD BE EXTENDED TO INCLUDE A FULL SUBCAT TREATMENT
	     ;; ***** or something like Melcuk's government.
	     (alt subject-subcat
		  (:index (subject cat))
		  (((subject none))
		   ((subject ((cat np))))
		   ((subject ((cat clause)))
		    (alt subject-clause
			 (:index (verb subject-clause))
			 (:demo "For clausal subjects, what type of clause ~
                           must be used?")
			 (((verb ((subject-clause infinitive)))
			   (subject ((mood {^ ^ verb subject-clause}))))
			  ((verb ((subject-clause present-participle)))
			   (subject ((mood {^ ^ verb subject-clause}))))
			  ((verb ((subject-clause that)))
			   (subject ((mood bound)
				     (binder ((lex "that")))))))))))
      
	     ;; Now, with the extended relational clauses, "object" can be a PP.
	     ;; In such a case, we should check here for what prep is used and
	     ;; provide defaults. ***** Not done. Should use a
	     ;; subcategorization schema in the verb a la radical lexicalist or
	     ;; HPSG. 
	     (alt object-subcat
		  (:index (object cat))
		  (((object none))
		   ((object ((cat np))))
		   ((object ((cat ap)))) ;; adjectival phrase
		   ((object ((cat pp))))
		   ((object ((cat adv))))
		   ((object ((cat clause)))
		    (alt object-clause
			 (:index (verb object-clause))
			 (:demo "For clausal objects, what type of clause ~
                           must be used?")	  
			 (((verb ((object-clause infinitive)))
			   (object ((mood infinitive))))
			  ((verb ((object-clause present-participle)))
			   (object ((mood present-participle))))
			  ((verb ((object-clause that)))
			   (object ((mood bound)
				    (binder ((lex "that")))))))))))


	     ;; Number of inherent participants to the process besides the subject.
	     ;; Based on verb classification:
	     ;; Neutral: 1 or 2 participants (object is optional)
	     ;; Intransitive: 1 participant
	     ;; Transitive:   2 participants
	     ;; Bitransitive: 3 participants
	     ;; ***** To redo when subcategorization is handled
	     (alt transitivity
		  (:index (verb transitive-class))
		  (:demo "How many roles can be used with this verb?")
		  (:bk-class transitivity)
		  (((verb ((transitive-class transitive)))
		    (iobject none)
		    (dative none))
		   ((verb ((transitive-class intransitive)))
		    (object none)
		    (iobject none)
		    (dative none))
		   ((verb ((transitive-class neutral)))
		    (iobject none)
		    (dative none))
		   ((verb ((transitive-class bitransitive))))))

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

	     (alt at-loc
		  (:demo "Is there an at-loc role?")
		  (((at-loc none))
		   ((at-loc given)
		    ;; get prep from role if given, otw from verb, otw default.
		    (opt ((at-loc ((prep given)))
			  (at-loc-comp ((prep ((lex {^ ^ ^ at-loc prep})))))))
		    (opt ((verb ((at-loc-prep given)))
			  (at-loc-comp ((prep ((lex {^ ^ ^ verb at-loc-prep})))))))
		    (at-loc-comp ((cat pp)
				  (opt ((prep ((lex "to")))))
				  (np {^ ^ at-loc}))))))

	     (alt to-loc
		  (:demo "Is there a to-loc role?")
		  (((to-loc none))
		   ((to-loc given)
		    ;; get prep from role if given, otw from verb, otw default.
		    (opt ((to-loc ((prep given)))
			  (to-loc-comp ((prep ((lex {^ ^ ^ to-loc prep})))))))
		    (opt ((verb ((to-loc-prep given)))
			  (to-loc-comp ((prep ((lex {^ ^ ^ verb to-loc-prep})))))))
		    (to-loc-comp ((cat pp)
				  (opt ((prep ((lex "to")))))
				  (np {^ ^ to-loc}))))))

	     (alt from-loc 
		  (:demo "Is there a from-loc role?")
		  (((from-loc none))
		   ((from-loc given)
		    (opt ((from-loc ((prep given)))
			  (from-loc-comp ((prep ((lex {^ ^ ^ from-loc prep})))))))
		    (opt ((verb ((from-loc-prep given)))
			  (from-loc-comp 
			   ((prep ((lex {^ ^ ^ verb from-loc-prep})))))))
		    (from-loc-comp ((cat pp)
				    (opt ((prep ((lex "from")))))
				    (np {^ ^ from-loc}))))))
      
	     (alt on-loc
		  (:demo "Is there an on-loc role?")
		  (((on-loc none))
		   ((on-loc given)
		    (opt ((on-loc ((prep given)))
			  (on-loc-comp ((prep ((lex {^ ^ ^ on-loc prep})))))))
		    (opt ((verb ((on-loc-prep given)))
			  (on-loc-comp ((prep ((lex {^ ^ ^ verb on-loc-prep})))))))
		    (on-loc-comp ((cat pp)
				  (opt ((prep ((lex "on")))))
				  (np {^ ^ on-loc}))))))
      
	     (alt in-loc 
		  (:demo "Is there an in-loc role?")
		  (((in-loc none))
		   ((in-loc given)
		    (opt ((in-loc ((prep given)))
			  (in-loc-comp ((prep ((lex {^ ^ ^ in-loc prep})))))))
		    (opt ((verb ((in-loc-prep given)))
			  (in-loc-comp ((prep ((lex {^ ^ ^ verb in-loc-prep})))))))
		    (in-loc-comp ((cat pp)
				  (opt ((prep ((lex "in")))))
				  (np {^ ^ in-loc}))))))
      
	     (alt instrument (:demo "Is there an instrument role?")
		  (((instrument none))
		   ((instrument given)
		    (opt ((instrument ((prep given)))
			  (instrument-comp ((prep ((lex {^ ^ ^ instrument prep})))))))
		    (opt 
		     ((verb ((instrument-prep given)))
		      (instrument-comp 
		       ((prep ((lex {^ ^ ^ verb instrument-prep})))))))
		    (instrument-comp
		     ((cat pp)
		      (opt ((prep ((lex "with")))))
		      (np {^ ^ instrument}))))))
      
	     ;; Answer to "who/what with?"
	     (alt accompaniment (:demo "Is there an accompaniment role?")
		  (((accompaniment none))
		   ((accompaniment given)
		    (opt 
		     ((accompaniment ((prep given)))
		      (accompaniment-comp ((prep ((lex {^ ^ ^ accompaniment prep})))))))
		    (opt 
		     ((verb ((accompaniment-prep given)))
		      (accompaniment-comp 
		       ((prep ((lex {^ ^ ^ verb accompaniment-prep})))))))
		    (accompaniment-comp
		     ((cat pp)
		      (opt ((prep ((lex "with")))))
		      (np {^ ^ accompaniment}))))))


	     ;; THREE CAUSE COMPLEMENTS (as by Hallyday): reason, purpose, behalf
	     ;; purpose: answer to "what for?"
	     (alt purpose (:demo "Is there a purpose role?")
		  (((purpose none))
		   ((purpose given)
		    (purpose ((cat clause)
			      (mood infinitive)
			      (syntax ((case purposive)))
			      (opt ((in-order ((lex "in order") (cat conj)))))))
		    (alt (((pattern (dots purpose start dots))
			   (purpose ((punctuation ((after ","))))))
			  ((pattern (dots purpose))
			   (purpose ((punctuation ((before ",")))))))))
		   ((purpose given)
		    (purpose ((cat np)))
		    (opt ((purpose ((prep given)))
			  (purpose-comp ((prep ((lex {^ ^ ^ purpose prep})))))))
		    (opt ((verb ((purpose-prep given)))
			  (purpose-comp 
			   ((prep ((lex {^ ^ ^ verb purpose-prep})))))))
		    (purpose-comp 
		     ((cat pp)
		      (opt ((prep ((lex "for")))))
		      (np {^ ^ purpose}))))))
      
	     ;; reason: answer to "why? How?"
	     (alt reason (:demo "Is there a reason role?")
		  (((reason none))
		   ((reason given)
		    (reason ((cat np)))
		    (opt ((reason ((prep given)))
			  (reason-comp ((prep ((lex {^ ^ ^ reason prep})))))))
		    (opt ((verb ((reason-prep given)))
			  (reason-comp 
			   ((prep ((lex {^ ^ ^ verb reason-prep})))))))
		    (reason-comp 
		     ((cat pp)
		      (opt ((prep ((lex "because of")))))
		      (np {^ ^ reason}))))
		   ((reason given)
		    (reason-comp {^ reason})
		    (reason-comp ((cat clause)
				  (mood bound)
				  (binder ((lex "because"))))))))

	     ;; behalf: answer to "who for?"
	     (alt behalf (:demo "Is there a behalf role?")
		  (((behalf none))
		   ((behalf given)
		    (behalf ((cat np)))
		    (opt ((behalf ((prep given)))
			  (behalf-comp ((prep ((lex {^ ^ ^ behalf prep})))))))
		    (opt ((verb ((behalf-prep given)))
			  (behalf-comp 
			   ((prep ((lex {^ ^ ^ verb behalf-prep})))))))
		    (behalf-comp 
		     ((cat pp)
		      (opt ((prep ((lex "for")))))
		      (np {^ ^ behalf}))))
		   ;; behalf as a for-to infinitive clause
		   ;; Note: subject must be given and is actually the behalf
		   ;; "You have to do it for John to read" (Winograd p.472)
		   ((behalf given)
		    (behalf ((cat clause)
			     (mood infinitive)))
		    (behalf-comp {^ behalf}))))
      

	     ;; All the possible time roles under one at-time plus a time-type
	     ;; feature sub-categorizing it.  This means I can have only one time
	     ;; role per clause.
	     ;; The list of time-type is given in Quirk 11.27
	     ;; Ex: in the afternoon, later, when I have time, last Thursday
	     ;; ***** Should implement the semantics of time-type with tpattern.
	     (alt at-time
		  (:demo "Is there an at-time role?")
		  (((at-time none))
		   ((at-time given)
		    (alt at-time 
			 (:index (at-time cat))
			 (((at-time ((cat adv)))
			   (time-comp {^ at-time}))
			  ((at-time  ((cat clause)
				      (mood bound)
				      (binder ((lex {^ ^ time-type})))
				      (time-type ((alt ("after" "as" "before"
								"once" "since" "until" "when"
								"while" "now that"))))))
			   (time-comp {^ at-time}))
			  ((at-time ((cat np)))
			   (time-type ((alt ("at" "on" "in" "for" "before" "after"
						  "since" "until"))))
			   (time-comp ((cat pp)
				       (prep ((lex {^ ^ ^ at-time time-type})))
				       (np {^ ^ at-time})))))))))

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

	     ;; START OF GENERAL THINGS: ORDERING PLUS AGREEMENT
	     ;; General things: arrange syntactic roles together
	     ;; and do the agreements.
	     ;; The patterns are here of course.
      
	     ;; Focus first (change when add modifiers)
	     ;; This is an example of mergeable pattern - where an ordering
	     ;; constraint can have an effect on voice choice.
	     ;; (pattern ((* focus) dots))  
	     ;; (focus {^ subject})
	     (focus {^ subject})

	     ;; Number and person agreement
	     (verb ((cat verb-group)
		    (modality {^ ^ modality})
		    (epistemic-modality {^ ^ epistemic-modality})
		    (deontic-modality {^ ^ deontic-modality})
		    (tense  {^ ^ tense})
		    (polarity {^ ^ polarity})
		    (insistence {^ ^ insistence})
		    (person {^ ^ subject person})
		    (number {^ ^ subject number})))
      
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
	     (alt verb-voice (:index (verb voice))
		  (:demo "Is the clause active or passive? This will ~
                          determine the form of the verb.")
		  ;; VERB VOICE ACTIVE
		  (((verb ((voice active)))
		    (by-obj none)
		    (alt (:bk-class transitivity)
			 (((verb ((transitive-class bitransitive) 
				  (dative-prep none)))
			   (particle none)
			   (pattern (dots start subject dots verb iobject object dots)))
			  ((verb ((transitive-class bitransitive) 
				  (dative-prep given)))
			   (dative ((cat pp) 
				    (prep ((lex {^ ^ ^ verb dative-prep})))
				    (np {^ ^ iobject})))
			   (pattern (dots start subject dots verb object particle 
					  dative dots)))
			  ((pattern (dots start subject dots verb object particle dots))))))
		   ;; VERB VOICE PASSIVE
		   ((verb ((voice passive)))
		    (pattern (dots start subject dots verb object particle by-obj dative dots))
		    (by-obj ((alt (none
				   ((cat pp)
				    (np given)
				    (np   ((syntax ((case objective)))))
				    (prep ((lex "by"))))))))
		    (opt ((iobject given)
			  (verb ((dative-prep given)))
			  (dative ((cat pp)
				   (prep ((lex {^ ^ ^ verb dative-prep})))
				   (np {^ ^ iobject}))))))))

	     ;; Case assignment
	     (opt ((subject ((syntax ((case subjective)))))))
	     (opt ((object  ((syntax ((case objective)))))))
	     (opt ((iobject ((syntax ((case objective)))))))

	     ;; Place optional roles
	     (pattern (dots start dots verb dots 
			    accompaniment-comp 
			    behalf-comp reason-comp purpose-comp
			    time-comp 
			    at-loc-comp from-loc-comp to-loc-comp
			    on-loc-comp in-loc-comp 
			    instrument-comp 
			    dots))) 

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
		  (:demo "what is the tense?")
	

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
		  (:demo "what modality is used w/ this verb?")
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
		   ;; now all other verbs
		   ((voice active)
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
					 ({^ subject} {^ ^ scope})
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
			 (:demo "Is this a personal, demonstrative, question ~
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
				    (person third)
				    (distance ((alt (far near))))))
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
				 ((pattern (head describer dots))))))
			  ))
		    ;; Case: subjective, objective, possessive, reflexive
		    ;; - - - - - - - - - - - - - - - - - - - - - - - - -
		    (syntax 
		     ((alt pronoun-case 
			   (:demo "Is the pronoun subject, object, possessive ~
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
			   (fset (cat generic-cat lex number a-an feature case))
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
		    (:demo "Are there describers?")
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
		    (:demo "Is there a classifier?")
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
		  (:demo "Is this a possessive NP?")
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
			 (:demo "Choose between A, THE and nothing")
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
		  (:demo "How many elements are there in the list?")
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
		    (constituent ((alt (:index cat)
				       (((cat clause)
					 (mood {^ ^ common mood}) ;; all conjuncts have
					 ;; same mood.
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
		    (constituent1 ((alt const1-cat (:index cat)
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
		    (constituent2 ((alt const2-cat (:index cat)
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
		    (alt (:index complex)
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
		    (opt 
		     ((cat clause)
		      ;; Do ellipsis of verb?
		      ;; Could look at ellipsis of subject - but that's tricky
		      ;; all we have in the input is agent/medium etc. (use bk-class)
		      ;; Want to test on subject not agent. *****
		      ;; How do you do ellipsis of VP without a VP in the grammar *****
		      ({^ constituent1 process} {^ ^ ^ constituent2 process})
		      ({^ constituent2 process gap} yes))))

		   ((distinct ((cdr ((cdr ((car given))))))) ;; list w/more than 3 elts
		    (constituent {^ distinct car})
		    (constituent ((cat ((alt (given {^ ^ common cat}))))))
		    (constituent ((alt (:index cat)
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

  (format t "~%gr8 installed. Samples tpattern procedural type.~%")
  (values))


