;;; -*- Mode:Lisp; Syntax:Common-Lisp; Package:FUG5 -*-
;;; -----------------------------------------------------------------------
;;; File:         gcon3.l
;;; Description:  Grammar for generation of connectives described in
;;;               Workshop paper with bk-class implemented.
;;; Author:       Michael Elhadad
;;; Created:      10-Oct-88
;;; Modified:     06-Nov-90
;;; Package:      FUG5
;;; -----------------------------------------------------------------------

(in-package "FUG5")

(defun fd-intersection (fd1 fd2)
  (let ((l1 (top-fd-to-list fd1))
	(l2 (top-fd-to-list fd2)))
    (intersection l1 l2)))

;;; Works for connectives: but, although, since, because.

(defun gsetup-con3 ()
  (setq *any-at-unification* nil)
  (setq *lexical-categories* (remove 'conj *lexical-categories*))
  (pushnew 'adj *lexical-categories*)
  (pushnew 'adv *lexical-categories*)
  (reset-typed-features)
  (clear-bk-class)
  (define-bk-class 'ao 'ao)
  (define-bk-class 'justified 'ao)
  (setq 
   *u-grammar*
   '((alt
      ;; ===========================================================
      ;; CAT DISCOURSE-SEGMENT -------------------------------------
      ;; ===========================================================
      (((cat discourse-segment)
	(directive ((cat utterance) (FS directive)))
	(alt ds
	 (((connective none)
	   (subordinate none)
	   (pattern (directive)))
	  ((connective ((cat connective)
			(P {^ ^ subordinate})
			(Q {^ ^ directive})))
	   (subordinate ((cat discourse-segment) (FS subordinate)))
	   (cset (directive subordinate connective))
	   (alt 
	    (((pattern (subordinate connective directive)))
	     ((pattern (connective directive subordinate))
	      (c ((position free)))))))
	  ((connective  ((cat connective)
			 (P {^ ^ directive})
			 (Q {^ ^ subordinate})))
	   (subordinate ((cat discourse-segment) (FS subordinate)))
	   (cset (directive subordinate connective))
	   (alt 
	    (((pattern (directive connective subordinate)))
	     ((pattern (connective subordinate directive))
	      (c ((position free)))))))))
	(Th {^ directive Th})
	(IF {^ directive IF})
	(PC {^ directive PC})
	(U  {^ directive U})
	(DL {^ directive DL})
	(AO {^ directive AO}))
       
       ;; ===========================================================
       ;; CAT UTTERANCE ---------------------------------------------
       ;; ===========================================================
       ((cat utterance)
	(pattern (pc))
	;; Process concept before pc to make sure lexical choice is done
	;; before processing the clause.
	(cset (concept pc))
	(alt (((lu support))
	      ((lu oppose))
	      ((lu general-opinion))
	      ((lu none))))
	;; Map concept --> verb in the pc before going to the clause
	(alt pc (:index (pc process-type))
	    (((pc ((process-type attributive)
		   (concept attribution))))
	     ((pc ((process-type equative)
		   (concept equation))))
	     ((pc ((process-type action))))
	     ((pc ((process-type mental))))))
	(concept ((cat lexical-entry)
		  (concept {^ ^ pc concept})))
	(pc ((cat clause)
	     (verb-concept {^ ^ concept})
	     (ao {^ ^ ao})))
	;(u ((cat utterer)))
	;(IF ((cat speech-act)))
	;(ThP ((alt (propositional argumentative-derivation illocutionary
	;            reinterpretation))))
	;(Th ((cat list)))
	;(DL ((cat topos)))
	(AO ((alt ao (:bk-class ao) (none ((justified any))))))
	(FS ((alt (directive subordinate)))))
       
       ;; ===========================================================
       ;; CAT ADJ  --------------------------------------------------
       ;; ===========================================================
       ((cat adj))

       ;; ===========================================================
       ;; CAT CONJ  -------------------------------------------------
       ;; ===========================================================
       ((cat conj)
	(alt conj (:index position)
	 (((position free)
	   (alt (:index lex)
	    (((lex "although"))
	     ((lex "since"))
	     ((lex "that"))
	     ((lex "whether"))
	     ((lex "in order"))
	     ((lex "if"))
	     ((lex "for"))
	     ((lex "because")))))
	  ((position middle)
	   (alt (:index lex)
	       (((lex "but"))
		((lex "and"))
		((lex "or"))))))))
       
       ;; ===========================================================
       ;; CAT CONNECTIVE --------------------------------------------
       ;; ===========================================================
       ((cat connective)
	;; The parts common to all connectives
	(pattern (c))
	(c ((cat conj)))
	;; Themes must intersect
	(Control (FD-intersection #@{^ P Th} #@{^ Q Th}))
	
	;; First alt: Functional Status
	;; For but: S-D order, all other, D-S order.
	(alt fs
	 (((P ((FS subordinate)))
	   (Q ((FS directive)))
	   (c ((lex "but"))))
	  ((P ((FS directive)))
	   (Q ((FS subordinate)))
	   (alt (((c ((lex "although"))))
		 ((c ((lex "because"))))
		 ((c ((lex "since")))))))))
	
	;; Second alt: Argumentation
	;; AO has 2 (main) features: conclusion and scale.
	;; DL has 4 (main) features: sign and scale/left and right.
	;; For all but `but', DL(P) must be none.
	;; The AO of P and Q is justified by the use of the connective
	(P ((AO ((justified yes)))))
	(Q ((AO ((justified yes)))))
	(alt arg (:bk-class ao)
	 (((P ((DL none)))
	   (alt (:bk-class ao)
	    (((Q ((DL ((scale-left  {^ ^ ^ Q AO scale})
		       (sign-right -)	
	       (scale-right {^ ^ ^ P AO scale})))))
	      (c ((lex "although"))))
	     ((Q ((DL ((scale-left  {^ ^ ^ Q AO scale})
		       (sign-right +)
		       (scale-right {^ ^ ^ P AO scale})))))
	      (alt (((c ((lex "because"))))
		    ((c ((lex "since"))))))))))
	  ((P ((DL ((scale-left  {^ ^ ^ P AO scale})
		    (scale-right {^ ^ ^ Q DL scale-right})))))
	   (Q ((DL ((scale-left  {^ ^ ^ Q AO scale})))))
	   ;; sign-right of DL(P) and DL(Q) must be opposed
	   (alt (:bk-class ao)
	    (((P ((DL ((sign-right +)))))
	      (Q ((DL ((sign-right -))))))
	     ((P ((DL ((sign-right -)))))
	      (Q ((DL ((sign-right +))))))))
	   (c ((lex "but"))))))

	;; Third alt: Polyphony
	;; Contrastive connective must have different utterers,
	;; locutor support the directive act.
	;; Because: same utterer in P and Q, and in both support.
	;; Since: need not be same utterer. LU(Q) un-constrained.
	(alt poly
	    (((P ((U {^ ^ Q U})
		  (LU support)))
	      (Q ((LU support)))
	      (c ((lex "because"))))
	     ((P ((LU support)))
	      (c ((lex "since"))))
	     ;; This is how we deal with negation (opt = optional).
	     ((opt ((P ((U {^ ^ Q U})))
		    (same-utterer yes)))
	      (same-utterer none)
	      (alt (((P ((LU support)))
		     (Q ((LU oppose)))
		     (c ((lex "although"))))
		    ((P ((LU oppose)))
		     (Q ((LU support)))
		     (c ((lex "but"))))))))))

       ;;==============================================================
       ;; CAT CLAUSE : clause -----------------------------------------
       ;;==============================================================
       ((cat clause)
	;; Get verb from lexicon (search done in utterance by concept)
	(verb ((cat verb-group)
	       (lex              {^ ^ verb-concept lex})
	       (process-class    {^ ^ verb-concept process-class})
	       (voice-class      {^ ^ verb-concept voice-class})
	       (transitive-class {^ ^ verb-concept transitive-class})
	       (dative-prep      {^ ^ verb-concept dative-prep})
	       (subject-clause   {^ ^ verb-concept subject-clause})
	       (object-clause    {^ ^ verb-concept object-clause})
	       (ao               {^ ^ verb-concept ao})
	       ;; add other features present in lexicon
	       ))
	(alt ao-verb (:bk-class ao)
	     (((verb ((ao none))))
	      ((verb ((ao given)
		      (ao {^ ^ ao})
		      (ao ((justified yes))))))))
	(alt mood (:index mood)
	    (((mood finite)
	      (non-finite none)
	      (alt (:index finite)
		  (((finite declarative)
		    ;; start is a dummy constituent used only in the patterns.
		    (pattern (start dots)))
		   ((finite #(under interrogative))
		    (alt (:index interrogative)
			(((interrogative yes-no))
			 ((interrogative wh)
			  (question ((cat np)
				     (np-type pronoun)
				     (pronoun-type question)))
			  (pattern (question start dots))))))
		   ((finite #(under bound))
		    (pattern (binder start dots))
		    (binder ((cat conj)))
		    (alt (:index (binder lex))
			(((binder ((lex "that"))))
			 ((binder ((lex "whether"))))
			 ((binder ((lex "if")))))))
		   ((finite #(under relative))
		    (alt (:trace relative) (:index relative-type)
		      (((relative-type simple)
			;; Example: the woman who lives there
			;;          the man whom I know
			;;          the reason why I came
			;; Simple relative is the qualifier of an NP. The NP
			;; is a constituent of the relative clause, as 
			;; indicated by the scope constituent:
			;; if NP is medium, do (scope ((role medium))) in the
			;; relative clause. Scope inherits the relevant 
			;; features from the head of the enclosing NP.
			(pattern (relative-marker start dots))
			(scope ((gap yes)
				(cat np)
				(animate {^ ^ head animate})))
			(alt 
			    (((relative-marker ((cat relpro) (lex "where")))
			      (alt (:index (scope role))
				  (((scope ((role to-loc))) 
				    (scope {^ to-loc np}))
				   ((scope ((role from-loc)))
				    (scope {^ from-loc np}))
				   ((scope ((role at-loc))) 
				    (scope {^ at-loc np})))))
			     ((relative-marker ((cat rel-pro) (lex "when")))
			      (scope ((role at-time)))
			      (scope {^ at-time np}))
			     ((relative-marker ((cat relpro) (lex "why")))
			      (scope ((role cause)))
			      (scope {^ cause}))
			     ((relative-marker ((cat relpro) (lex "how")))
			      (scope ((role manner)))
			      (scope {^ manner}))
			     ;; This is a catch-all for all other semantic 
			     ;; roles of scope they will be realized by 
			     ;; who/which/that
			     ((alt (:index (scope role))
				  (((scope ((role agent)))
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
			      (alt (:index restrictive)
				  (((restrictive yes)
				    (relative-marker ((lex "that")
						      (cat relpro))))
				   ((restrictive no)
				    (relative-marker 
				      ((cat np)
				       (np-type pronoun)
				       (pronoun-type question)
					(case {^ ^ scope case})
					(animate {^ ^ scope animate}))))))))))
		       ((relative-type embedded))
		       ;; Example: the box in which we store the elixir
		       ;;          an experience the likes of which you
		       ;;          have never seen
		       ((relative-type be-deleted))
		       ;; Example: Goldwater /who was/ crushed by his defeat
		       ;;          the team /that is/ expected to win
		       ;;          an enchanting island /that is/ waiting
		       ;;          to be seen 
		       ((relative-type wh-nominal))
		       ;; Example: They were amazed at which one they chose
		       ;;          I couldn't believe how many people
		       ;;          understood 
		       ;;          What they did next was surprising
		       ((relative-type wh-ever-nominal))))))))
	     ;; Example: Whoever did it will be sorry
	     ((mood #(under non-finite))
	      (finite none)
	      (alt (:index non-finite)
		  (((non-finite imperative)
		    ;; subject is optional in realization
		    (opt ((subject ((gap yes)))))
		    (verb ((ending root))))
		   ((non-finite present-participle)
		    (verb ((ending present-participle)))
		    ;; subject is optional or in possessive form
		    (alt (((subject ((cat np)
				     (case possessive))))
			  ((subject ((gap yes)))))))
		   ((non-finite infinitive)
		    (verb ((ending infinitive)))
		    (alt (((subject ((gap yes) (lex none))))
			  ((subject ((cat np)
				     (case objective))))))
		    ;; When the clause is subject or purpose, and there is
		    ;; a subject, use a FOR-clause as in "for her to do it
		    ;; is a bold statement" 
		    (opt ((case purposive)
			  (pattern (in-order dots))))
		    (opt ((alt (((case subjective))
				((case purposive))))
			  (pattern (dots for start dots))
			  (for ((cat conj) (lex "for")))
			  (subject ((gap none) (lex given)))))))))))
	 
	(alt (:index process-type)
	    ;; Process-type: action, mental, or relation
	    ;; -----------------------------------------
	    
	    ;; Process 1: Action --> actions, events, natural phenomena
	    ;; So far only actions done.
	    ;; inherent cases    --> agent, medium, benef.
	    ;; all are optional, but at least one of medium or agent
	    ;; must be present.
	    ;; this will be dealt with in the voice alternation.
	    (((process-type action)
	      (alt 
		  (((agent ((cat np)
			    (lex given)
			    (animate yes))))
		   ((agent ((cat np)
			    (np-type pronoun)
			    (animate yes))))
		   ((agent none))
		   ((agent ((cat clause)
			    (number {^ subject number})
			    (mood non-finite))))))
	      (alt 
		  (((medium none))
		   ((medium ((cat np))))
		   ((medium ((cat clause)
			     (number {^ subject number})
			     (mood non-finite))))))
	      (alt
		  (((benef none))
		   ((benef ((cat np))))))
	      (alt (:index intended)
		  (((intended yes))
		   ((intended no))))
	      (verb ((process-class action)
		     (lex given))))	     ;there must be a verb given
	     
	     ;; Process 2: mental --> perception, reaction, cognition,
	     ;;                       verbalization 
	     ;; inherent cases    --> processor, phenomenon
	     ;; processor is required, phenomenon is optional.
	     ((process-type mental)
	      (verb ((process-class mental)
		     (lex given)))
	      (alt (((processor ((cat np) (animate yes) (lex given))))
		    ((processor ((cat np) (np-type pronoun) (animate yes))))
		    ((processor none))))
	      (alt
		  (((phenomenon none))
		   ((phenomenon ((cat np))))
		   ((phenomenon ((cat clause)
				 (mood finite)
				 (finite bound))))
		   ((phenomenon ((cat clause)
				 (mood non-finite)
				 (non-finite inifinitive))))
		   ((phenomenon ((cat clause)
				 (mood non-finite)
				 (non-finite present-participle)))))))
	     
	     ;; Process 3: relation --> equative, attributive
	     ;; there need not be a verb, it will be determined by the
	     ;; epistemic modality features among the possible copula.
	     ((process-type attributive)
	      (verb ((process-class attributive)))
	      ;; so far all we do if the verb is not given use "be"
	      ;; later use modality...
	      (opt ((verb ((lex "be")
			   (voice-class non-middle)
			   (transitive-class neutral)))))
	      ;; inherent cases     --> carrier, attribute
	      (alt (((carrier ((cat np)
			       (lex given)
			       (definite yes))))
		    ((carrier ((cat np)
			       (np-type pronoun)
			       (definite yes))))
		    ((carrier none))
		    ((carrier ((cat clause)
			       (mood finite)
			       (finite bound)
			       (binder ((lex "that"))))))
		    ((carrier ((cat clause)
			       (mood non-finite)
			       (non-finite infinitive))))
		    ((carrier ((cat clause)
			       (mood non-finite)
			       (non-finite present-participle))))))
	      ; attribute can be a property or a class
	      ; like in "John is a teacher" or "John is happy".
	      (alt (:index (attribute cat))
		  (((attribute ((cat adj)
				(lex given))))
		   ((attribute ((cat np)
				(definite no)))))))
	     ((process-type equative)
	      ;; inherent cases    --> identified, identifier
	      ;; both cases have the same definite feature
	      ;; and are required. 
	      ;; "A book is an object" or "The president is the chief"
	      (verb ((process-class equative)))
	      (opt ((verb ((lex "be")
			   (voice-class non-middle)
			   (transitive-class neutral)))))
	      (alt (((identified ((cat np) (lex given))))
		    ((identified ((cat np) (np-type pronoun))))
		    ((identified none))
		    ((identified ((cat clause)
				  (mood non-finite)
				  (non-finite infinitive)
				  (subject ((gap yes)))
				  (definite no))))
		    ((identified ((cat clause)
				  (mood non-finite)
				  (non-finite present-participle)
				  (definite yes))))))
	      (identifier ((cat np) (definite {^ ^ identified definite}))))))
      
	;; Voice choice --> operative, middle, receptive.
	;; Operative =~ active
	;; Receptive =~ passive
	;; Middle    = sentences with only one participant ("the sun shines")
	;; Choice middle/non-middle is based on verb classification
	;; it is also based on the interaction verb/participant but we don't
	;; do that yet.
	;; Choice receptive/operative is based on focus (using pattern).
	;; The voice alternation does the mapping semantic -> syntactic roles
	(alt (:index (verb voice-class))
	  (((verb ((voice-class middle)))
	    (voice middle)
	    ; We must have only one participant, medium which is the subject
	    (alt (:index process-type) 
		;; main case is the subject.
		;; cannot have a middle verb with process-type relation.
		(((process-type action)
		  ;; (agent none)
		  (benef none)
		  (subject {^ medium}))
		 ((process-type mental)      ;; ??? Are there mental/middle
		  (subject {^ processor}))))
	    (object none)
	    (iobject none))
	   ((verb ((voice-class non-middle)))
	    (alt (((voice operative)
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
			 (alt 
			     ;; Is there an explicit prot?
			     ;; well, actually should distinguish between
			     ;; "the door opened" and "the door was opened".
			     (((agent ((lex none)))
			       (by-obj none))
			      ((agent given)
			       (by-obj ((np {^ ^ agent}))))))
			 (alt
			     ;; subject is either benef or medium
			     ;; "A book is given to mary by john"
			     ;; "Mary is given a book by John"
			     (((subject {^ medium})
			       (object none)
			       (iobject {^ benef}))
			      ((subject {^ benef})
			       (iobject none)
			       (object  {^ medium})))))
			((process-type mental)
			 (verb ((voice passive)))
			 (subject {^ phenomenon})
			 (alt
			     ;; is there an explicit processor?
			     (((processor ((lex none))))
			      ((processor ((lex given)))
			       (by-obj ((np {^ ^ processor}))))))
			 (iobject none))
			;cannot have an attributive process in receptive voice.
			((process-type equative)
			 (subject {^ identifier})
			 (object  {^ identified})
			 (verb ((voice active)))
			 (iobject none))))))))))

	;; Syntactic categories of subject/object are compatible with verb?
	;; This depends on particular verb, information we will get from the
	;; lexicon. So far, we check whether subject-clause and object-clause
	;; are ok. If you had disjunctions allowed in the input, life would be
	;; easier (we would put: subject-cat (alt <possible-values>)) in the 
	;; lexicon. We could also do it with a (cat member). And we can also
	;; do it by using a different feature for each acceptable cat.
	(alt 
	    (((verb ((subject-clause none)))
	      (subject ((cat np))))
	     ((verb ((subject-clause infinitive)))
	      (subject ((cat clause)
			(mood non-finite)
			(non-finite infinitive))))
	     ((verb ((subject-clause present-participle)))
	      (subject ((cat clause)
			(mood non-finite)
			(non-finite present-participle))))
	     ((verb ((subject-clause that)))
	      (subject ((cat clause)
			(mood finite)
			(finite bound)
			(binder ((lex "that"))))))))
	(alt object-clause
	    (((verb ((object-clause none)))
	      (alt
		  (((object ((cat np))))
		   ((object none))
		   ((object ((cat adj)))))))
	     ((verb ((object-clause infinitive)))
	      (object ((cat clause)
		       (mood non-finite)
		       (non-finite infinitive))))
	     ((verb ((object-clause present-participle)))
	      (object ((cat clause)
		       (mood non-finite)
		       (non-finite present-participle))))
	     ((verb ((object-clause that)))
	      (object ((cat clause)
		       (mood finite)
		       (finite bound)
		       (binder ((lex "that"))))))))

	;; Number of inherent participants to the process
	;; Based on verb classification:
	;; Neutral: 1 or 2 participants
	;; Intransitive: 1 participant
	;; Transitive:   2 participants
	;; Bitransitive: 3 participants
	(alt (:index (verb transitive-class))
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
	(alt (((to-loc none))
	      ((to-loc given)
	       (opt ((to-loc ((prep ((lex "to")))))))
	       (to-loc ((cat pp)
			(lex given)
			(np ((cat np)
			     (definite {^ ^ definite})
			     (np-type {^ ^ np-type})
			     (lex {^ ^ lex}))))))))
	
	(alt (((from-loc none))
	      ((from-loc given)
	       (opt ((from-loc ((prep ((lex "from")))))))
	       (from-loc ((cat pp)
			  (lex given)
			  (np ((cat np)
			       (definite {^ ^ definite})
			       (np-type {^ ^ np-type})
			       (lex {^ ^ lex}))))))))
	
	(alt (((on-loc none))
	      ((on-loc given)
	       (opt ((on-loc ((prep ((lex "on")))))))
	       (on-loc ((cat pp)
			(lex given)
			(np ((cat np)
			     (definite {^ ^ definite})
			     (np-type {^ ^ np-type})
			     (lex {^ ^ lex}))))))))
	
	(alt (((purpose none))
	      ((purpose given)
	       (opt ((purpose ((in-order ((lex "in order") (cat conj)))))))
	       (purpose ((cat clause)
			 (mood non-finite)
			 (non-finite infinitive)
			 (case purposive)
			 (punctuation ((after ",")
				       (before ",")))))
	       (alt (((pattern (purpose dots)))
		     ((pattern (dots purpose))))))))
	
	;; General things: arrange syntactic roles together
	;; and do the agreements.
	;; The patterns are here of course.
	
	; Focus first (change when add modifiers)
	; (pattern ((* focus) dots))
	(focus {^ subject})
	
	; Number and person agreement
	(verb ((cat verb-group)
	       (person {^ ^ subject person})
	       (number {^ ^ subject number})))
	
	; Arrange order of complements
	; start is a dummy constituent used only for the ordering constraints
	(pattern (dots start subject verb dots))
	(alt (:index (verb voice))
	    ; VERB VOICE ACTIVE
	    (((verb ((voice active)))
	      (by-obj none)
	      (alt 
		  (((object none)
		    (iobject none))
		   ; John gave the book
		   ((verb ((transitive-class bitransitive)))
		    (iobject none)
		    (pattern (dots verb object dots)))
		   ; John gave Mary the book
		   ((verb ((transitive-class bitransitive)
		    (dative-prep none)))
		    (pattern (dots verb iobject object dots)))
		   ((iobject none)
		    (pattern (dots verb object dots)))
		   ((verb ((dative-prep given)))
		    (dative ((cat pp) 
			     (prep ((lex {^ ^ ^ verb dative-prep})))
			     (np {^ ^ iobject})))
		    (pattern (dots verb object dative dots))))))
	     
	     ; VERB VOICE PASSIVE
	     ((verb ((voice passive)))
	      (alt 
		  (((object none))
		   ((pattern (dots verb object dots)))))
	      (alt
		  (((by-obj none))
		   ((by-obj ((cat pp)
			     (prep ((lex "by")))))
		    (pattern (dots verb dots by-obj dots)))))
	      (alt
		  (((iobject none))
		   ; the book is given by John to Mary
		   ((verb ((dative-prep given)))
		    (dative ((cat pp)
			     (prep ((lex {^ ^ ^ verb dative-prep})))
			     (np {^ ^ iobject})))
		    (pattern (dots verb dots dative dots))))))))
	
	;; Place optional roles
	(pattern (dots start dots verb dots from-loc to-loc on-loc dots)))

	

      

       ;;==============================================================
       ;; CAT VERB-GROUP ----------------------------------------------
       ;;==============================================================
       ;; No treatment of auxiliary/modals yet
       ((cat verb-group)
	(v ((cat verb)
	    (person {^ ^ person})
	    (number {^ ^ number})
	    (tense  {^ ^ tense})
	    (ending {^ ^ ending})))
	(alt verb-voice (:index voice)
	    (((voice active)
	      (pattern (v dots))
	      (v ((lex {^ ^ lex}))))
	     ((voice passive)
	      (pattern (v v1 dots))
	      (v ((lex "be")))
	      (v1 ((cat verb) (lex {^ ^ lex}) (ending past-participle)))))))
       
       ;;==============================================================
       ;; CAT NP ------------------------------------------------------
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
       
       ((cat np)
	;; GENERAL NP =================================================
	; np inherits major features from head and definite from determiner
	(pattern (dots head dots))
	(head ((lex {^ ^ lex})
	       (number {^ ^ number})
	       (np-type {^ ^ np-type})))
	
	;; NP-TYPE: Pronouns, common, proper =========================
	(alt (:index np-type)
	    ;; COMMON NOUNS -------------------------------------------
	    ;; Only common nouns can have determiners.
	    (((np-type common)
	      (head ((cat noun)
		     (lex given)))
	      (person third)
	      (alt (:index collective)
		  (((collective no))
		   ((collective #(under yes))
		    (number plural))))
	      (alt (:index countable)
		  (((countable yes))
		   ((countable #(under no))
		    (definite no))))
	      (pattern (determiner dots))
	      (determiner ((cat det)
			   (definite {^ ^ definite})
			   (distance {^ ^ distance}))))
	     
	     ;; PRONOUNS ------------------------------------------------
	     ((np-type pronoun)
	      ;; pronouns allow no classifier, no determiner, 
	      ;; all except quantified have no describer.
	      ;; can have qualifiers
	      (classifier none)
	      (determiner none)
	      (head ((cat pronoun)
		     (case {^ ^ case})
		     (number {^ ^ number})
		     (animate {^ ^ animate})
		     (pronoun-type {^ ^ pronoun-type})))
	      (pattern (head dots))
	      ;; Pronoun-type: personal, demonstrative, question, quantified
	      ;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	      (alt (:index pronoun-type)
		  (((pronoun-type personal)
		    ;; are gender and person specific, default is third masc.
		    (alt (:index gender)
			(((gender masculine) (animate yes))
			 ((gender #(under feminine)) (animate yes))
			 ((gender #(under neuter)) (animate no))))
		    (alt (:index person)
			(((person third))
			 ((person #(under first)))
			 ((person #(under second)))))
		    (head ((gender {^ ^ gender})
			   (person {^ ^ person})))
		    (definite yes)
		    (describer none))
		   ((pronoun-type #(under demonstrative))
		    (definite yes)
		    (person third)
		    (describer none)
		    (head ((distance {^ ^ distance})))
		    (alt (:index distance)
			(((distance far))
			 ((distance #(under near))))))
		   ((pronoun-type #(under question))
		    (definite no)
		    (person third)
		    (describer none))
		   ;; - describers come after head: "something green"
		   ((pronoun-type #(under quantified))
		    (definite no)
		    (person third)
		    (alt (((describer none))
			  ((describer given)
			   (pattern (head describer dots))))))
		   ))
	      ;; Case: subjective, objective, possessive, reflexive
	      ;; - - - - - - - - - - - - - - - - - - - - - - - - -
	      (alt (:index case)
		  (((case subjective))
		   ((case #(under objective)))
		   ((case #(under possessive)))
		   ((case #(under reflexive))))))
	     
	     
	     ;; Proper nouns -------------------------------------------
	     ((np-type proper)
	      (head ((cat noun)))
	      (person third)
	      (pattern (head))
	      (definite yes)
	      (determiner none)
	      (describer none)
	      (classifier none)
	      (qualifier none))))
	
	;; NUMBER ====================================================
	(alt (:index number)
	    (((number singular))
	     ((number #(under plural)))))
	
	;; DESCRIBER =================================================
	(alt (((describer none))
	      ((describer given)
	       (pattern (dots describer dots head dots))
	       (alt
		   (((describer ((cat adj))))
		    ((describer ((cat verb)
				 (ending past-participle)
				 (modifier-type objective))))
		    ((describer ((cat verb)
				 (ending present-participle)
				 (modifier-type subjective)))))))))
	
	;; CLASSIFIER =================================================
	(alt (((classifier none))
	      ((classifier given)
	       (pattern (dots classifier head dots))
	       (alt (:index (classifier cat))
		   (((classifier ((cat noun))))
		    ((classifier ((cat verb)
				  (ending present-participle)
				  (modifier-type subjective)))))))))
	
	;; QUALIFIER ==================================================
	(alt (((qualifier none))
	      ((qualifier given)
	       (pattern (dots head qualifier))
	       (alt (:index (qualifier cat))
		   (((qualifier ((cat pp)
				 (restrictive yes)))
		     ;; send features of qualifier just to np of pp.
		     ;; default prep is "of".
		     (opt ((qualifier ((prep ((lex "of")))))))
		     (qualifier ((np ((head {^ ^ head})
				      (definite {^ ^ definite})
				      (np-type {^ ^ np-type})
				      (pronoun-type {^ ^ pronoun-type})
				      (animate {^ ^ animate})
				      (number {^ ^ number})
				      (person {^ ^ person})
				      (gender {^ ^ gender})
				      (case {^ ^ case})
				      (distance {^ ^ distance}))))))
		    ((qualifier ((cat np)))
		     (alt (:index (qualifier restrictive))
			 (((qualifier ((restrictive yes))))
			  ((qualifier ((restrictive no)
				       (punctuation ((before ",")
						     (after ",")))))))))
		    ((qualifier ((cat clause)
				 (mood finite)
				 (finite relative)
				 (relative-type simple)))
		     (alt (((qualifier ((restrictive yes))))
			   ((qualifier ((restrictive no))))))))))))
	
	;; POSSESSIVE MARKER ==========================================
	(alt (((case none))
	      ((case #(under possessive))
	       (qualifier none)
	       (head ((feature possessive))))
	      ((case given))))
	)


       ;; ==============================================================
       ;; CAT LIST : for coordinated constituents ----------------------
       ;; ==============================================================
       ;; Lists have 3 main features:
       ;; partofspeech: name of the category of all the coordinated consts.
       ;; conjunction: the conjunction of coordination to use.
       ;; list: a list of constituents in car/cdr notation.
       ((cat list)
	(opt ((conjunction ((lex "and")))))
	(conjunction ((cat conj) (lex given)))
	(alt (((list ((car none))))                ;; the list is empty
	      ((list ((cdr ((car none)))))         ;; the list has only 1 elt
	       (constituent ((cat {^ ^ partofspeech})))
	       (constituent {^ list car})
	       (pattern (constituent)))
	      ((list ((cdr ((cdr ((car none))))))) ;; list has only 2 elts
	       (constituent1 ((cat {^ ^ partofspeech})))
	       (constituent2 ((cat {^ ^ partofspeech})))
	       (constituent1 {^ list car})
	       (constituent2 {^ list cdr car})
	       (pattern (constituent1 conjunction constituent2)))
	      (                                    ;; list has more than 3 elts
	       (constituent ((cat {^ ^ partofspeech})
			     (punctuation ((after ",")))))
	       (constituent {^ list car})
	       (rest ((cat list)
		      (partofspeech {^ ^ partofspeech})
		      (conjunction {^ ^ conjunction})
		      (list {^ ^ list cdr})))
	       (pattern (constituent rest))))))
       
       ;; ==============================================================
       ;; CAT PRONOUNS -------------------------------------------------
       ;; ==============================================================
       ;; Pronouns have features: pronoun-type, case, animate,
       ;; person, distance, person, number.
       ;; Most of the work for personal, question, demonstrative
       ;; done in morphology routines. Quantified to be treated here.
       ((cat pronoun))
       
       ;; ==============================================================
       ;; CAT PP : for prepositional phrases ---------------------------
       ;; ==============================================================
       ((cat pp)
	(pattern (prep np))
	(prep ((cat prep) (lex given)))
	(np ((cat np))))
       
       ;;==============================================================
       ;; CAT DET : for articles --------------------------------------
       ;;==============================================================
       ;; No treatment of quantification, pre-det, ord. and cardinal.
       ((cat det)
	(number {^ ^ number})
	(alt (:index demonstrative)
	    (((demonstrative no)
	      (distance none))
	     ((demonstrative yes)
	      (definite yes)
	      (alt (:index distance)
		  (((distance near)
		    (alt (:index number)
			(((number plural) 
			  (lex "these"))
			 ((number singular)
			  (lex "this")))))
		   ((distance far)
		    (alt (:index number)
			(((number plural)
			  (lex "those"))
			 ((number singular)
			  (lex "that"))))))))))
	(alt (:index possessive)
	    (((possessive no))
	     ((possessive yes)
	      (definite yes)
	      (alt 
		  (((np-type pronoun)
		    (pronoun-type personal)
		    (alt (:index person)
			(((person first)
			  (alt (:index number)
			      (((number singular) (lex "my"))
			       ((number plural) (lex "our")))))
			 ((person second) (lex "your"))
			 ((person third) 
			  (alt (:index number) 
			      (((number singular)
				(alt (:index gender)
				    (((gender masculine) (lex "his"))
				     ((gender feminine) (lex "her"))
				     ((gender neuter) (lex "its")))))
			       ((number plural) (lex "their"))))))))
		   ((pattern (possessive-det))
		    (alt (:index np-type)
			(((np-type common))
			 ((np-type proper))
			 ((np-type pronoun)
			  (alt (:index pronoun-type)
			      (((pronoun-type question))
			       ((pronoun-type quantified)))))))
		    (possessive-det ((cat np)
				     (determiner none)
				     (definite yes)
				     (case possessive)))))))))
	(alt
	    (((demonstrative yes))
	     ((possessive yes))
	     ((demonstrative no)
	      (possessive no)
	      (definite yes)
	      (lex "the"))
	     ((definite no)
	      (opt ((number singular)
		    (lex "a")))))))

       ;;==============================================================
       ;; CAT LEXICAL-ENTRY: for mapping concept/verb -----------------
       ;;==============================================================
       ((cat lexical-entry)
	(alt lex (:index concept) (:bk-class ao)
	  (((concept attribution)
	    (lex "be"))
	   ((concept "equation")
	    (lex "be"))
	   ((concept Transfer)
	    (alt take (:index lex) (:bk-class ao)
		(((lex "take")
		  (object-clause none)
		  (subject-clause none)
		  (dative-prep "from"))
		 ((lex "steal")
		  (dative-prep "from")
		  (object-clause none)
		  (subject-clause none)
		  (AO ((conclusion 
			 ((process-type attributive)
			  (carrier {^ ^ ^ ^ pc prot})
			  (attribute ((lex "dishonest")))))
		       (justified yes)
		       (scale dishonesty))))))))))

       
       ))))
  (format t "~%gcon3 installed.~%")
  (values))


