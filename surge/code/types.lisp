;;; -*- Mode:Lisp; Syntax:Common-Lisp; Package: -*-
;;; -----------------------------------------------------------------------
;;; File:         types.lisp
;;; Description:  All type definitions used in the grammar
;;; Author:       Michael Elhadad
;;; Created:      19 Dec 1991
;;; Modified:     23 Dec 1991: added "grouping" types to make hierarchy
;;;                            look nicer in the map.
;;;               26 Dec 1991: added question-det and quantifier-det
;;;               28 Dec 1991: changed pronp to pronoun.
;;;               12 May 1992: added collective-set distributive-set
;;;               19 Aug 1992: added person-name, basic-proper, compound-proper
;;;                8 Jul 1993: added addresses & dates (later under np for irs)
;;;                5 Jul 1995: SURGE 2.2 VERSION
;;;                            Removed trivial-name
;;;                            Added adv-p
;;;                            Specialized wh into wh-partial, wh-full, wh-possessive
;;;                            Removed nominalized-ing and verbal-clause
;;;                            Added complex for cardinal and ordinal
;;;                            Added possessive-relative
;;;               19 May 1996: Package all defs in one function
;;;                            (reset-surge-types)
;;; -----------------------------------------------------------------------

(in-package "FUG5")

(defun type-map ()
  "Draw a map of all types segmented in meaningful pages"
  (types-postscript 'tense "tense.map.ps")
  (types-postscript '(mood modality) "mood.map.ps")
  (types-postscript '(participant process-class) "transitivity.map.ps")
  (types-postscript 'semantic-cat "semantic.map.ps")
  (types-postscript '(lexical-cat adv det) "cat.map.ps"))


;; TYPE DECLARATIONS
;; =================

;; CATEGORY HIERARCHY
;; ------------------
;; 
;; 1. SEMANTIC (non-lexical) vs. LEXICAL-CAT
;; Input to grammar can be either lexicalized or non-lexicalized.
;; When it is lexicalized, cat are CLAUSE, VERB-GROUP, NP, PP, AP, NOUN,
;; ADJ. 
;; When it is non-lexicalized, it needs to be lexicalized first (each
;; constituent) before the grammar for syntax can work.  In this case,
;; input contains semantic cats EVENT or THING with a concept
;; specification.  The concept is then lexicalized and the cat is
;; specialized to one of the lexical cats. So hierarchy is top for semantic
;; cats and below lexical cats.
;;
;; 2. SIMPLE - COMPLEX RELATIONS
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

(defun reset-surge-types ()
  (reset-typed-features)

(define-feature-type semantic-cat (lexical-cat event relation set thing))

(define-feature-type thing (zero-article-thing article-thing
					       countable mass))
(define-feature-type set (collective-set distributive-set))
(define-feature-type zero-article-thing (season institution meal illness
						transportation))

(define-feature-type number (singular plural))
(define-feature-type plural (dual not-one))

(define-feature-type lexical-cat (clause verb-group np ap pp np-head adj prep
					 conj phrase address))

(define-feature-type clause (simple-clause complex-clause))
(define-feature-type verb-group (simple-verb-group complex-verb-group))
(define-feature-type np (simple-np complex-np))
(define-feature-type pp (simple-pp complex-pp))
(define-feature-type ap (simple-ap complex-ap))
(define-feature-type np-head (simple-np-head complex-np-head))
(define-feature-type cardinal (simple-cardinal complex-cardinal))
(define-feature-type ordinal (simple-ordinal complex-ordinal))

;; The cats that can serve as np heads
;; JR: added person-name and team-name
(define-feature-type simple-np-head 
  (noun noun-compound measure partitive name))
;; Name is quite domain specific: add your types of names here...
(define-feature-type name (person-name team-name))
(define-feature-type noun (simple-noun complex-noun))
(define-feature-type adj  (simple-adj complex-adj))

;; A single complex construct handles all the similarities between complex
;; constructs and handles the recursion.
(define-feature-type complex (clause verb-group np ap pp np-head adj 
				     cardinal ordinal))

;; The NP hierarchy
(define-feature-type simple-np 
  (pronoun common proper measure partitive date address))
;; JR-added 3/7/95: address to simple-np types

(define-feature-type proper (trivial-proper basic-proper compound-proper))
(define-feature-type common (partitive))
(define-feature-type complex-np (pronoun common proper date partitive))
;; JR-added 3/2/95: partitive in complex-np types

(define-feature-type pronoun (personal-pronoun question-pronoun relative-pronoun
		              quantified-pronoun demonstrative-pronoun))

(define-feature-type det (possessive-det demonstrative-det article-det
					 question-det quantifier-det))
(define-feature-type question-det (question-possessive-det))
(define-feature-type possessive-det (question-possessive-det))

(define-feature-type adv (intensifier detensifier adv-p))

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

(define-feature-type process-class (lexical simple-process composite))
(define-feature-type simple-process (event relation))
(define-feature-type event (material mental verbal))
(define-feature-type mental   (perception thinking))
(define-feature-type relation (ascriptive possessive locative))
(define-feature-type locative (spatial temporal accompaniment
				       existential natural-phenom))

;; Composite functions: only the possible combinations are defined here
(define-feature-type participant
  (agent affected created carrier identified))
(define-feature-type agent (agent-carrier third-party-agent agent-identified))
(define-feature-type affected (affected-carrier affected-identified))
(define-feature-type created (created-carrier))
(define-feature-type carrier (located possessor))
(define-feature-type located (agent-carrier affected-carrier created-carrier))
(define-feature-type possessor 
  (agent-carrier affected-carrier created-carrier))
(define-feature-type identified (agent-identified affected-identified))


;; JR-added-1/19/93
;; Core Syntactic Roles
(define-feature-type synt-role (subject obj complement))
(define-feature-type obj (object iobject dative by-obj))
(define-feature-type complement (subj-comp obj-comp))

;; JR-added-1/19/93
;; Adverbial Syntactic Roles
(define-feature-type synt-adverbial (adjunct disjunct))
(define-feature-type adjunct (pred-adjunct sent-adjunct))

;; JR-added-1/19/93
;; Adverbial Semantic Roles
(define-feature-type sem-adverbial (pred-modif circum))
(define-feature-type pred-modif 
  (location direction origin destination path distance duration manner means 
   instrument comparison score))
(define-feature-type circum
  (location distance origin time duration frequency co-event reason result purpose 
   behalf condition concessive-condition concession contrast exception inclusion 
   substitution addition accompaniment opposition manner means comparison matter 
   standard perspective))
(define-feature-type sem-adverbial (spatial temporal causal conditional
				    determinative how topical))
(define-feature-type spatial (location direction origin destination path distance))
(define-feature-type temporal (time duration frequency co-event))
(define-feature-type causal (reason result purpose behalf co-event))
(define-feature-type conditional (condition concessive-condition concession))
(define-feature-type determinative (contrast opposition exception inclusion
				    substitution addition accompaniment))
(define-feature-type how (manner means instrument comparison))
(define-feature-type topical (matter standard perspective))


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
;; Changed by JR 1-16-92: removed unimplemented stuff
;;                        added verbless, bound-adverbial 
;;                        subdivided infinitive

(define-feature-type mood (finite non-finite))
(define-feature-type finite (declarative interrogative bound relative))
(define-feature-type non-finite (imperative participle infinitive verbless))
(define-feature-type participle (past-participle present-participle))
(define-feature-type infinitive (to-infinitive for-to-infinitive bare-infinitive))
(define-feature-type interrogative (yes-no wh))
(define-feature-type wh (wh-full wh-possessive wh-partial))
(define-feature-type bound (bound-nominal bound-adverbial))
(define-feature-type bound-nominal 
  (bound-nominal-declarative bound-nominal-subjunctive))
(define-feature-type relative (simple-relative embedded-relative possessive-relative))



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

(define-feature-type tense
  (present past future past-perfect present-perfect future-perfect
	   past-progressive present-progressive future-progressive
	   tense-10 tense-11 tense-12 tense-13 tense-14 tense-15
	   tense-16 tense-17 tense-18 tense-19 tense-20 tense-21
	   tense-22 tense-23 tense-24 tense-25 tense-26 tense-27
	   tense-28 tense-29 tense-30 tense-31 tense-32 tense-33
	   tense-34 tense-35 tense-36))

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

) ;; reset-surge-types

(reset-surge-types)


;; ============================================================
(provide "types")
;; ============================================================
