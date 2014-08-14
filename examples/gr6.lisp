;;; -*- Mode:Lisp; Syntax:Common-Lisp; -*-
;;; ------------------------------------------------------------
;;; File        : GR6.L
;;; Description : Grammar with types/fset/external
;;; Author      : Michael Elhadad
;;; Created     : 25 Jun 90
;;; Modified    : 09 Jan 91  (New circ. roles)
;;; Language    : Common Lisp
;;; ------------------------------------------------------------

(in-package "FUG5")

(defun role-exists (path)
  "An external function to test that the role of a clause is given in
  input.
  External functions return a piece of grammar."
  ;; All roles can be nps
  (let* ((role (car (path-last path)))  ;; the role being inspected
	 (msg  (format nil "Is there a role ~s in the input" role))
	 (branches '(((cat np)
		      (alt (((lex given))
			    ((head given))
			    ((cat #(under pronp)))
			    ((gap given))))))))

    ;; These roles can be adjectives:
    (when (member role '(attribute identifier identified))
      (setf branches (cons '((cat adj)) branches)))

    ;; These roles can be clauses:
    (when (member role '(agent medium phenomenon carrier identifier
			       identified))
      (setf branches (append branches '( ((cat clause)) ))))

    ;; Allow for conjunctions in roles:
    (setf branches
	  (append branches
		  (cond ((member role '(identifier identified))
			 '( ((cat list)
			     (common ((cat ((alt (np adj clause))))))) ))
			((member role '(attribute))
			 '( ((cat list)
			     (common ((cat ((alt (np adj))))))) ))
			((member role '(agent medium carrier))
			 '( ((cat list)
			     (common ((cat ((alt (np clause))))))) ))
			(t 
			 '( ((cat list)
			     (common ((cat np)))) )))))

    ;; All roles are optional (fix that)
    (setf branches (cons 'none branches))
    
    `((alt ,role (:index cat) 
	,branches))))
  
(defun gsetup6 ()
  ;; Reset type declaration in effect before new decl.
  (clear-bk-class)
  (reset-typed-features)

  ;; TYPE DECLARATIONS
  ;; =================

  ;; MOOD SYSTEM:
  ;; ------------------------------------------------------------------
  ;; MOOD: finite/non-finite
  ;; FINITE: declarative/interrogative/bound/relative
  ;; NON-FINITE: imperative/present-participle/infinitive
  ;; INTERROGATIVE: yes-no/wh
  ;; RELATIVE: simple-relative/embedded-relative/be-deleted-relative/
  ;;           wh-nominal-relative/wh-ever-nominal-relative

  (define-feature-type mood (finite non-finite))
  (define-feature-type finite (declarative interrogative bound relative))
  (define-feature-type non-finite (imperative present-participle infinitive))
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
  ;; - action         | agent, medium, benef
  ;; - mental         | processor, phenomenon
  ;; - attributive    | carrier, attribute
  ;; - equative       | identified, identifier
  (define-feature-type process-type (action mental attributive equative))

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

  (define-feature-type np (pronp common proper))
  (define-feature-type pronp (personal-pronoun question-pronoun relative-pronoun
					       quantified-pronoun demonstrative-pronoun))

  (define-feature-type det (possessive-det demonstrative-det regular-det))
  ;; This is a bad idea for modeling
  (define-feature-type possessive-det (np)) ;; with a possessive feature.

  ;; CONSTITUENTS OF NPs   relevant features
  ;; ------------------------------------------------------------------
  ;; determiner:           definite/distance/demonstrative/possessive
  ;; describer:            
  ;; head:                 (syntax) lex/animate/person/number/gender/case
  ;; classifier:           
  ;; qualifier:            restrictive [yes/no]
  ;; (possessive determiners are described as NPs, with a possessive case.)

  (setq *u-grammar*'((alt
		      ;;==============================================================
		      ;; 01 CAT CLAUSE : clause --------------------------------------
		      ;;==============================================================
		      (((cat clause)

			;; First easy mapping...
			(verb {^ process})

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
				      (alt (:index mood)
					   (((mood yes-no))
					    ((mood wh)
					     (question ((cat question-pronoun)))
					     (pattern (question start dots))))))
				     ((mood bound)
				      (pattern (binder start dots))
				      (binder ((cat conj)))
				      (opt binder
					   ((binder ((lex ((alt ("that" "whether" "if"))))))))
				      (binder ((lex given))))
				     ((mood relative)
				      (alt (trace relative) (:index mood) 
					   (:demo
					    "Is the relative clause simple or embedded in a PP?")
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
						     (cat np)
						     (lex {^ ^ ^ head lex})
						     (semantics ((index {^ ^ ^ ^ semantics index})))))
					     (alt relative-marker
						  (:index (scope role))
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
						   ((alt restrictive-relative 
							 (:index restrictive)
							 (((restrictive yes)
							   (relative-marker ((cat relpro) (lex "that"))))
							  ((restrictive no)
							   (relative-marker 
							    ((cat pronoun) 
							     (pronoun-type relative)
							     (animate {^ syntax animate})
							     (case {^ syntax case})
							     (semantics {^ ^ scope semantics})
							     (syntax {^ ^ scope syntax}))))))))))

					    ((mood embedded-relative)
					     ;; Example: the box in which we store the elixir
					     ;;          an experience the likes of which you have never seen
					     (pattern (prep relative-marker start dots))
					     (prep ((cat prep)))
					     (scope ((gap yes)))
					     (relative-marker ((cat pronoun)
							       (pronoun-type relative)
							       (animate {^ syntax animate})
							       (case {^ syntax case})
							       (semantics {^ ^ scope semantics})
							       (syntax {^ ^ scope syntax})))
					     (alt scope-embedded 
						  (:index (scope role))
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

					    ((mood be-deleted-relative))
					    ;; Example: Goldwater /who was/ crushed by his defeat
					    ;;          the team /that is/ expected to win
					    ;;          an enchanting island /that is/ waiting to be seen

					    ((mood wh-nominal-relative))
					    ;; Example: They were amazed at which one they chose
					    ;;          I couldn't believe how many people understood
					    ;;          What they did next was surprising

					    ((mood wh-ever-nominal-relative))))))))
			      ;; Example: Whoever did it will be sorry

			      ((mood non-finite)
			       (alt non-finite 
				    (:index mood)
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


			(alt process 
			     (:index process-type)
			     (:demo "Is the process an action, mental, attributive ~
                       or equative?")

			     ;; Process-type: action, mental, or relation
			     ;; -----------------------------------------
	
			     ;; Process 1: Action --> actions, events, natural phenomena
			     ;; inherent cases    --> agent, medium, benef.
			     ;; all are optional, but at least one of medium or agent
			     ;; must be present.
	
			     (((process-type action)
			       (agent #(external role-exists))
			       (medium #(external role-exists))
			       (benef  #(external role-exists))
			       (process ((process-class action)
					 (lex given))))	;there must be a verb given

			      ;; Process 2: mental --> perception, reaction, cognition, verbalization
			      ;; inherent cases    --> processor, phenomenon
			      ;; processor is required, phenomenon is optional.
			      ((process-type mental)
			       (process ((process-class mental)
					 (lex given)))
			       (processor #(external role-exists))
			       (phenomenon #(external role-exists)))

			      ;; Process 3: attributive
			      ;; there need not be a verb, it will be determined by the
			      ;; epistemic modality features among the possible copula.
			      ;; inherent cases     --> carrier, attribute
			      ((process-type attributive)
			       (process ((process-class attributive)))
			       (opt 
				verb-be-attributive
				((verb ((lex "be")
					(subject-clause infinitive)
					(object-clause none)
					(voice-class non-middle)
					(transitive-class neutral)))))
			       (carrier #(external role-exists))
			       (attribute #(external role-exists)))

			      ;; Process 4: equative
			      ;; inherent cases    --> identified, identifier
			      ;; "A book is an object" or "The president is the chief"
			      ((process-type equative)
			       (process ((process-class equative)))
			       (opt 
				verb-be-equative
				((verb ((lex "be")
					(voice-class non-middle)
					(subject-clause that)
					(object-clause present-participle)
					(transitive-class neutral)))))
			       (identified #(external role-exists))
			       (identifier #(external role-exists)))))

			;; Voice choice --> operative, middle, receptive.
			;; Operative =~ active
			;; Receptive =~ passive
			;; Middle    = sentences with only one participant ("the sun shines")
			;; Choice middle/non-middle is based on verb classification
			;; it is also based on the interaction verb/participant but we don't
			;; do that yet.
			;; Choice receptive/operative is based on focus (using pattern).
			;; The voice alternation does the mapping semantic -> syntactic roles
			(alt voice 
			     (:index (verb voice-class))
			     (:demo "Is the verb middle or non-middle?")
			     (((verb ((voice-class middle)))
			       (voice middle)
			       ;; We must have only one participant, the medium that is the subject.
			       (alt (:index process-type) 
				    ;; main case is the subject.
				    ;; cannot have a middle verb with process-type relation.
				    (((process-type action)
				      ;; (agent none)            ;; ERGATIVE CONSTRUCT to work out.
				      ;; agent make medium verb.
				      (benef none)
				      (subject {^ medium}))
				     ((process-type mental) ;; ??? Are there mental/middle
				      (subject {^ processor}))))
			       (object none)
			       (iobject none))

			      ((verb ((voice-class non-middle)))
			       (alt non-middle-voice 
				    (:index voice)
				    (:demo "Is the clause passive or active? This ~
                           determines the mapping deep to surface roles")
				    (((voice operative)
				      (verb ((voice active)))
				      (alt (:index process-type)
					   (((process-type action)
					     (subject {^ agent})
					     (object {^ medium})
					     (iobject {^ benef}))
					    ((process-type mental)
					     (subject {^ processor})
					     (object  {^ phenomenon})
					     (iobject none))
					    ((process-type equative)
					     (subject {^ identified})
					     (object {^ identifier})
					     (iobject none))
					    ((process-type attributive)
					     (subject {^ carrier})
					     (object  {^ attribute})
					     (iobject none)))))
				     ((voice receptive)
				      ;; Warning: a receptive is not always translated into a
				      ;; passive verb!!!: "this string won't tie" (no actor).
				      (alt (index process-type)
					   (((process-type action)
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
					    ((process-type equative)
					     (subject {^ identifier})
					     (object  {^ identified})
					     (verb ((voice active)))
					     (iobject none))))))))))


			;; Now take care of special treatment of subject
			;; clauses (this is the right time because the subject is now bound).
			(alt subject-mood
			     (:index mood) 
			     (:demo "Is a subject required ~
                       or does it need a special treatment?")
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
				      ;; use a FOR-clause as in "for her to do it is a bold statement"
				      (alt keep-for
					   (:demo "Should we use a for in front of the subject?")
					   (((keep-for yes)
					     (syntax ((case ((alt (subjective purposive))))))
					     (subject given)
					     (pattern (dots for start dots))
					     (for ((cat conj) (lex "for"))))
					    ((keep-for no)))))
				     ((mood present-participle)
				      ;; subject is optional or in possessive form
				      (alt subject-subject
					   (((subject given)
					     (subject ((cat np)
						       (syntax ((case possessive))))))
					    ((subject none))
					    ((subject ((gap yes)))))))
				     ((mood imperative)
				      ;; subject is optional in realization
				      (alt (((subject none))
					    ((subject ((gap yes))))))))))))


			;; Syntactic categories of subject/object are compatible with verb?
			;; This depends on particular verb, information we will get from the
			;; lexicon. So far, we check whether subject-clause and object-clause
			;; are ok. 
			(alt subject-subcat
			     (:index (subject cat))
			     (((subject none))
			      ((subject ((cat np))))
			      ((subject ((cat list) (common ((cat np))))))
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
      
			(alt object-subcat
			     (:index (object cat))
			     (((object none))
			      ((object ((cat np))))
			      ((object ((cat adj))))
			      ((object ((cat list) (common ((cat ((alt (np adj)))))))))
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
			;; Neutral: 1 or 2 participants
			;; Intransitive: 1 participant
			;; Transitive:   2 participants
			;; Bitransitive: 3 participants
			(alt transitivity
			     (:index (verb transitive-class))
			     (:demo "How many roles can be used with this verb?")
			     (((verb ((transitive-class intransitive)))
			       (object none)
			       (iobject none)
			       (dative none))
			      ((verb ((transitive-class transitive)))
			       (iobject none)
			       (dative none))
			      ((verb ((transitive-class neutral)))
			       (iobject none)
			       (dative none))
			      ((verb ((transitive-class bitransitive))))))

			;; OPTIONAL CASES:
			;; These cases can occur with all process-types.
			;; They handle "circumstances".
			;; All have the same structure.
			;; Order should be studied with care. I have now a standard order.
			;; All roles are mapped to corresponding syntactic complements.

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
					 (opt ((in-order ((lex "in order") (cat conj)))))
					 (punctuation ((after ",")))))
			       (alt (((pattern (purpose dots)))
				     ((pattern (dots purpose))))))
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
				 (np {^ ^ behalf}))))))
      

			;; All the possible time roles under one at-time plus a time-type
			;; feature sub-categorizing it.  This means I can have only one time
			;; role per clause.
			;; The list of time-type is given in Quirk 11.27
			;; Ex: in the afternoon, later, when I have time, last Thursday
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

			(alt time-relater (:demo "Is there a time-relater?")
			     (((time-relater none))
			      ((time-relater given)
			       (time-relater ((cat adv)
					      (punctuation ((after ",")))))
			       (pattern (time-relater dots)))))

			(alt cond-relater (:demo "Is there a cond-relater?")
			     (((cond-relater none))
			      ((cond-relater given)
			       (cond-relater ((cat adv)))
			       (pattern (time-relater cond-relater dots)))))



			;; General things: arrange syntactic roles together
			;; and do the agreements.
			;; The patterns are here of course.
      
			;; Focus first (change when add modifiers)
			;; (pattern ((* focus) dots))
			(focus {^ subject})
      
			;; Number and person agreement
			(verb ((cat verb-group)
			       (modality {^ ^ modality})
			       (epistemic-modality {^ ^ epistemic-modality})
			       (deontic-modality {^ ^ deontic-modality})
			       (tense  {^ ^ tense})
			       (person {^ ^ subject person})
			       (number {^ ^ subject number})))
      
			;; particle is for verbs like "take off"
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
			       (alt (((verb ((transitive-class bitransitive) 
					     (dative-prep none)))
				      (particle none)
				      (pattern (dots start subject verb iobject object dots)))
				     ((verb ((transitive-class bitransitive) 
					     (dative-prep given)))
				      (dative ((cat pp) 
					       (prep ((lex {^ ^ ^ verb dative-prep})))
					       (np {^ ^ iobject})))
				      (pattern (dots start subject verb object particle 
						     dative dots)))
				     ((pattern (dots start subject verb object particle dots))))))
			      ;; VERB VOICE PASSIVE
			      ((verb ((voice passive)))
			       (pattern (dots start subject verb object particle by-obj dative dots))
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
		       ;; No treatment of auxiliary yet
		       ((cat verb-group)
			(v ((person {^ ^ person})
			    (number {^ ^ number})
			    (tense  {^ ^ tense})
			    (ending {^ ^ ending})))
			;; v carries the agreement features
			;; v1 and v2 are optional parts
			;; for groups like "must be done"

			(alt (((modality none)
			       (epistemic-modality none)
			       (deontic-modality none))
			      ((modality epistemic-modality)
			       (epistemic-modality given)
			       (deontic-modality none))
			      ((modality deontic-modality)
			       (deontic-modality given)
			       (epistemic-modality none))))
			(alt modality
			     (:demo "What modality is used with the verb?")
			     (((epistemic-modality none)
			       (deontic-modality none)
			       %TRACE-OFF%
			       (control (control-demo "No modality in this clause"))
			       %TRACE-ON%
			       (v ((cat verb))))
			      ((epistemic-modality fact)
			       (v ((cat verb))))
			      ((epistemic-modality inference)
			       (v ((lex "must")
				   (cat modal)))
			       (v1 ((ending root))))
			      ((epistemic-modality possible)
			       (v ((lex "can")
				   (cat modal)))
			       (v1 ((ending root))))
			      ((deontic-modality duty)
			       (v ((lex "must")
				   (cat modal)))
			       (v1 ((ending root))))
			      ((deontic-modality authorisation)
			       (v ((lex "may")
				   (cat modal)))
			       (v1 ((ending root))))
			      ((control (string #@{^ epistemic-modality}))
			       (v ((lex {^ ^ epistemic-modality})
				   (cat modal)))
			       (v1 ((ending root))))
			      ((control (string #@{^ deontic-modality}))
			       (v ((lex {^ ^ deontic-modality})
				   (cat modal)))
			       (v1 ((ending root))))))
	    
			(alt voice-verb 
			     (:index voice)
			     (:demo "Is the verb active or passive?")
			     (((voice active)
			       (alt (((epistemic-modality given)
				      (v1 ((cat verb) (lex {^ ^ lex})))
				      (v2 none))
				     ((deontic-modality given)
				      (v1 ((cat verb) (lex {^ ^ lex})))
				      (v2 none))
				     ((epistemic-modality none)
				      (deontic-modality none)
				      (v ((lex {^ ^ lex})))))))
			      ((voice passive)
			       (alt (((modality given)
				      (v1 ((cat verb) (lex "be")))
				      (v2 ((cat verb)
					   (lex {^ ^ lex})
					   (ending past-participle))))
				     ((modality none)
				      (v ((lex "be")))
				      (v1 ((cat verb) 
					   (lex {^ ^ lex}) 
					   (ending past-participle)))))))))
			(pattern (v v1 v2)))
     

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
			;; GENERAL NP =================================================
			;; General structure: head, syntax, semantics, determiner.
			(pattern (dots head dots))
			(is-a-determiner no) ;; a hack to avoid poss. dets.
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

			%focus%
			(control (control-demo "*****I = ~s~%~%" *input*))
			%focus%

			;; NP-TYPE: Pronouns, common, proper =========================
			(alt np-type 
			     (:index cat)
			     (:demo "Is this a common noun, a pronoun or a proper noun?")
			     ;; COMMON NOUNS -------------------------------------------
			     ;; Only common nouns can have determiners.
			     (((cat common)
			       (head ((cat noun)
				      (lex given)
				      (fset (cat lex number a-an feature)) ;; these only accepted
				      ;; by morphology.
				      (number {^ ^ syntax number})
				      (a-an {^ ^ syntax a-an})))
			       (person third)
			       (alt (:index countable)
				    (((countable yes))
				     ((countable no)
				      (definite no))))
			       (pattern (determiner dots))
			       (determiner ((cat det)
					    (is-a-determiner yes)
					    (syntax ((definite  {^ ^ ^ syntax definite})
						     (countable {^ ^ ^ syntax countable})
						     (number    {^ ^ ^ syntax number})
						     (distance  {^ ^ ^ syntax distance}))))))
	
			      ;; PRONOUNS ------------------------------------------------
			      ((cat pronp)
			       ;; pronouns allow no classifier, no determiner, 
			       ;; all except quantified have no describer.
			       ;; can have qualifiers
			       (semantics ((classifier none)))
			       (determiner none)
			       (head ((cat pronoun) ;; known to morphology plus its args.
				      (fset (cat pronoun-type case gender animate feature 
						 syntax lex number distance person))
				      (case {^ ^ syntax case})
				      (gender {^ ^ syntax gender})
				      (number {^ ^ syntax number})
				      (animate {^ ^ syntax animate})
				      (distance {^ ^ syntax distance})
				      (person {^ ^ syntax person})))
			       (pattern (head dots))
			       ;; Pronoun-type: personal, relative, demonstrative, question, quantified
			       ;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
			       (alt pronoun-type 
				    (:index cat)
				    (:demo "Is this a personal, relative, demonstrative, question ~
                          or quantified pronoun?")
				    (((cat personal-pronoun)
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
				     ;; Pass it to morphology
				     ((cat pronoun) (pronoun-type relative))
				     ((cat demonstrative-pronoun)
				      (head ((pronoun-type demonstrative)))
				      (syntax ((definite yes)
					       (person third)
					       (distance ((alt (far near))))))
				      (semantics ((describer none))))
				     ((cat question-pronoun)
				      (head ((pronoun-type question)))
				      (syntax ((person third) (definite no)))
				      (semantics ((describer none))))
				     ;; - describers come after head: "something green"
				     ((cat quantified-pronoun)
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
			       (head ((cat noun)
				      (lex given)
				      (fset (cat lex number a-an feature)) ;; these only accepted
				      ;; by morphology.
				      (number {^ ^ syntax number})
				      (a-an {^ ^ syntax a-an})))
			       (pattern (head))
			       (syntax ((person third)
					(definite yes)))
			       (determiner none)
			       (semantics ((describer none)
					   (classifier none)
					   (qualifier none))))))

			;; NUMBER ====================================================
			(syntax ((number ((alt (singular plural))))))

			;; DESCRIBER =================================================
			(semantics
			 ((alt describer
			       (:demo "Is there a describer?")
			       (((describer none))
				((describer given)
				 ({^ pattern} (dots describer dots head dots))
				 (describer
				  ((alt (:index cat)
					(((cat adj))
					 ((cat verb)
					  (ending past-participle)
					  (modifier-type objective))
					 ((cat verb)
					  (ending present-participle)
					  (modifier-type subjective)))))))))))

			;; CLASSIFIER =================================================
			(semantics
			 ((alt classifier 
			       (:demo "Is there a classifier?")
			       (((classifier none))
				((classifier given)
				 ({^ pattern} (dots classifier head dots))
				 (classifier
				  ((alt (:index cat)
					(((cat noun))
					 ((cat verb)
					  (ending present-participle)
					  (modifier-type subjective)))))))))))

			;; QUALIFIER ==================================================
			(semantics
			 ((alt qualifier
			       (:demo "Is there a qualifier? Is it an apposition, a PP or ~
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
					 ((cat np)
					  (alt (:index restrictive)
					       (((restrictive yes))
						((restrictive no)
						 (punctuation ((before ",")
							       (after ",")))))))
					 ((cat clause)
					  (mood relative)))))))))))
		  

			;; CATAPHORA ==================================================
			;; a hack... "the following: ..."
			(opt 
			 ((ref-type given)
			  (ref-type immediate-cataphora)
			  (pattern (dots referred))
			  (referred ((cat ((alt (list np))))
				     (punctuation ((before ":")))))))


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
		       ;; 04 CAT LIST : for coordinated constituents -------------------
		       ;; ==============================================================
		       ;; Lists have 3 main features:
		       ;; common: all the features common to all conjuncts
		       ;; distinct: a list of features in car/cdr form
		       ;; conjunction: the conjunction of coordination to use.
		       ;; NOTE: don't deal with ellipsis of common parts.
		       ;; NOTE: ONLY ALLOW LISTS OF CLAUSES, NPS and ADJs.
		       ;;       if you want other cats, change the alts here 
		       ;;       (3 identical alts to change)
		       ((cat list)
			(opt ((conjunction ((lex "and")))))
			(conjunction ((cat conj) (lex given)))
			(alt list
			     (:demo "How many conjuncts are there: 0, 1, 2, or more?")
			     (((distinct ((car none))))	;; the list is empty
			      ((distinct ((cdr ((car none))))) ;; the list has only 1 elt
			       (constituent ((alt (:index cat)
						  (((cat clause)
						    (mood {^ ^ common mood})
						    (scope {^ ^ common scope}))
						   ((cat adj))
						   ((cat np)
						    (syntax {^ ^ common syntax}))))))
			       (constituent {^ distinct car})
			       (cset (constituent))
			       (pattern (constituent)))
			      ((distinct ((cdr ((cdr ((car none))))))) ;; list has only 2 elts
			       (constituent1 {^ distinct car})
			       (constituent2 {^ distinct cdr car})
			       (constituent1 ((punctuation ((after none)))
					      (alt (:index cat)
						   (((cat clause)
						     (mood {^ ^ common mood})
						     (scope {^ ^ common scope}))
						    ((cat adj))
						    ((cat np)
						     (syntax {^ ^ common syntax}))))))
			       ;; Do ellipsis of verb?
			       ;; Could look at ellipsis of subject - but that's tricky
			       ;; all we have in the input is agent/medium etc.
			       ;; Want to test on subject not agent.
			       (opt (({^ constituent1 verb lex} {^ ^ ^ ^ constituent2 verb lex})
				     ({^ constituent2 verb gap} yes)))
			       (constituent2 ((alt (:index cat)
						   (((cat clause)
						     (mood {^ ^ common mood})
						     (scope {^ ^ common scope}))
						    ((cat adj))
						    ((cat np)
						     (syntax {^ ^ common syntax}))))))
			       (cset (conjunction constituent1 constituent2))
			       (pattern (constituent1 conjunction constituent2)))
			      ((distinct ((cdr ((cdr ((car given)))))))	;; list w/more than 3 elts
			       (constituent {^ distinct car})
			       (constituent ((alt (:index cat)
						  (((cat clause)
						    (mood {^ ^ common mood})
						    (scope {^ ^ common scope}))
						   ((cat adj))
						   ((cat np)
						    (syntax {^ ^ common syntax}))))
					     (punctuation ((after ",")))))
			       (rest ((cat list)
				      (common {^ ^ common})
				      (conjunction {^ ^ conjunction})
				      (distinct {^ ^ distinct cdr})))
			       (cset (constituent rest))
			       (pattern (constituent rest))))))

		       ;; ==============================================================
		       ;; 06 CAT PP : for prepositional phrases ------------------------
		       ;; ==============================================================
		       ((cat pp)
			(pattern (prep np))
			(prep ((cat prep) (lex given)))
			(np ((cat np)))
			)

		       ;;==============================================================
		       ;; 07 CAT DET : for articles -----------------------------------
		       ;;==============================================================
		       ;; No treatment of quantification, pre-det, ord. and cardinal.
		       ((cat det)
			(pattern (head)) ;; the actual word sequence.
			(is-a-determiner yes) ;; hack...

			;; Make implication distance ==> demonstrative
			(opt DIsTANCE-DET
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
     
			      ((cat #(under possessive-det))
			       (syntax ((definite yes)))
			       (alt possessive-det 
				    (:index cat)
				    (((cat #(under personal-pronoun))
				      (head ((cat article))) ;; don't do anything on it at morph.
				      (alt (:index (syntax person))
					   (((syntax ((person first)))
					     (alt (:index (syntax number))
						  (((syntax ((number singular)))
						    (head ((lex "my"))))
						   ((syntax ((number plural))) 
						    (head ((lex "our")))))))
					    ((syntax ((person second))) 
					     (head ((lex "your"))))
					    ((syntax ((person third)))
					     (alt (:index (syntax number))
						  (((syntax ((number singular)))
						    (alt (:indexz (syntax gender))
							 (((syntax ((gender masculine))) 
							   (head ((lex "his"))))
							  ((syntax ((gender feminine))) 
							   (head ((lex "her"))))
							  ((syntax ((gender neuter))) 
							   (head ((lex "its")))))))
						   ((syntax ((number plural))) 
						    (head ((lex "their"))))))))))
				     ((cat np)
				      (head ((cat {^ ^ cat})
					     (lex {^ ^ lex})
					     (semantics {^ ^ semantics})
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

		       ((cat phrase))
		       ((cat article))
		       ((cat pronoun))
		       ((cat relpro))

		       ((cat adj))
		       ((cat noun))
		       ((cat verb))
		       ((cat prep))

		       ))))

  (format t "~%gr6 installed.~%")
  (values))

