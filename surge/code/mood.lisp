;;; -*- Mode:Lisp; Syntax:Common-Lisp; Package:  -*-
;;; -----------------------------------------------------------------------
;;; File:         mood.lisp
;;; Description:  New mood system for SURGE
;;; Author:       Michael Elhadad & Jacques Robin
;;; Created:      27 Nov 1992
;;; Modified:      5 Jul 1995 SURGE 2.2 VERSION
;;;                  - Add generic-mood to deal with same-mood constraint
;;;                    in clause conjunction
;;;                  - Allow innermost-role to be none (no partic).
;;;                  - Allow binders for non-finite except imperative
;;;                  - Add possessive-relative "The man whose car I wash"
;;;                5 Nov 1995 - Fix rule of which/that/who for relative-marker
;;;               12 May 1995 - Fix alt infinitive for bare-infinitive...
;;;                             Fix alt subject-mood for bare-infinitive...
;;;               19 Aug 1996 - Remove (dative-move no) to (mood relative)
;;;                             to allow relative clauses of lexical
;;;                             processes of 3 roles.
;;; -----------------------------------------------------------------------

(in-package "FUG5")

(def-alt mood
  ;; MOOD SYSTEM
  ;; For relative and interrogative, the mapping scope to role is
  ;; delayed after the transitivity system (same time as the voice
  ;; system does the mapping semantic/syntactic roles).
  ;; In this first system, all pre-selections that can be derived from
  ;; the mood are set.
  (:index mood)
  (:demo "Deciding between mood finite and non-finite")

  (((mood finite)
    (alt finite (:index mood)
      (:demo
       "Is the clause declarative, interrogative, relative or subordinate?")
      (((mood declarative)
	(generic-mood declarative)
	(pattern ({^ headers 1} {^ headers 2} stop-header dots start dots)))

       ((mood #(under interrogative))
	(generic-mood interrogative)
	;; for both yes-no and wh questions, front the tensed part
	;; of the verb group and the not particle.
	;; copy everything from tensed-feature except the gap
	;; (therefore cannot just conflate them).
	;; Note: these are all the features known to the morphology.
	(scope ((gap yes)))
	;; For wh questions don't use dative-move
	(alt wh-dative-move
             (((mood wh)
	       (dative-move no))
	      ((mood yes-no))))
	(process ((interrogative {^2 mood})))
	(cset ((- fronted-aux)))
	(alt aux (:wait {^ verb tensed-feature})
	  (((fronted-aux
	     ((person {^2 verb tensed-feature person})
	      (number {^2 verb tensed-feature number})
	      (ending {^2 verb tensed-feature ending})
	      (tense  {^2 verb tensed-feature tense })
	      (cat    {^2 verb tensed-feature cat})
	      (lex    {^2 verb tensed-feature lex})))))))

       ((mood #(under bound))
	;; For conjunctions like:
	;; I know who he is and whether he is coming.
	(generic-mood declarative)
	(pattern (stop-header dots binder dots start dots))
	(binder ((cat conj)))
	(alt bound-moods (:index mood)
	  (((mood bound-nominal)
	    (binder ((lex ((alt binder-lex (given "that" "whether" "if"))))))
	    ;; If clause is scoped by a long distance dependency, don't use
	    ;; binder: the person who you think [that] won the prize.
	    (alt bound-scoped (:index scoped) (:wait scoped)
		 (((scoped no))
		  ((scoped #(under yes))
		   (binder ((gap yes))))))
	    (alt mood-bound-nominal (:index mood)
	      (((mood bound-nominal-declarative))
	       ((mood bound-nominal-subjunctive)
		(modality none)
		(epistemic-modality none)
		(deontic-modality none)
		(process ((ending root)))))))

	   ;; Added by JR-1/18/93
	   ((mood bound-adverbial)))))

       ;; ***** Missing here: wh-nominal
       ;; "I know WHAT LOVE IS."

       ;; relative -- mapping scope/role is done in voice system.
       ;; Just indicate here that scope has a gap.
       ((mood #(under relative))
	(generic-mood relative)
	;; JR-note: origin of the dgsB1c failure
	;; (dative-move no)
	(scope ((gap yes)))))))

   ((mood #(under non-finite))
    (modality none)
    (epistemic-modality none)
    (deontic-modality none)
    (alt non-finite-moods (:index mood)
      (((mood imperative)
	(generic-mood imperative)
	(process ((ending root) (voice active)))
	(innermost-role ((alt imperative-subject (((gap yes)) none))))
	(pattern (stop-header dots start dots)))
       ((pattern (stop-header dots binder dots start dots))
	;; - All cases can have a binder:
	;;   "BEFORE JUMPING, check your watch."
	;;   "We must discover HOW TO DO IT."
	;;   "AS SOON AS KOED BY IRON MIKE, he found himself a millionaire."
	;; - All cases can be conjoined together:
	;;   "The box accessible to all,
        ;;    burned by the sun and bulging under cover."
	;;   "The man crushed by love,
        ;;    now knowing what love is and happy about it."
	(generic-mood embedded-mood)
	(alt non-finite-binder (((binder none)) ((binder ((cat conj))))))
	(alt embedded-moods (:index mood)
	  (((mood present-participle)
	    (process ((ending present-participle))))

	   ((mood past-participle)
	    (process ((ending past-participle)
		      (tensed-feature ((gap yes)))
		      (voice passive))))

	   ((mood verbless)
	    (process ((type ascriptive) (gap yes))))

	   ;; Bare-infinitive cf. Quirk p.993
	   ;; "RATHER THAN SHOOT, Bo made the perfect pass."
	   ((mood #(under bare-infinitive))
	    (process ((ending root))))

	   ;; For all other variants of infinitive -
           ;; ending infinitive with a "to".
	   ((mood infinitive)
	    (process ((ending infinitive))))))))))))

(def-alt subject-mood
  (:demo "Is a subject required or does it need a special treatment?")
  (
   ;; Moods w/ obligatory subjects
   ((mood ((alt subject-mood-obligatory (declarative yes-no bound relative))))
    (synt-roles ((subject given))))

   ;; Moods w/ possibly absent subject due to question scoping
   ((mood wh)
    (alt scoped-subject
         (:wait {^ synt-roles subject})
         (((synt-roles ((subject given))))
          ((scope {^ synt-roles subject})))))

   ;; Moods w/ possibly absent subject due to control by embedding clause
   ;; Default is no subject unless (controlled none)
   ((mood ((alt (past-participle verbless))))
    (alt controlled-subject
         (:wait controlled)
         (((synt-roles ((subject given)))
           (controlled none))
          ((synt-roles ((subject none))))
          ((synt-roles ((subject given)))
           (controlled {^ synt-roles subject})
           (controlled ((gap yes)))))))

   ((mood present-participle)
    (alt present-participle-subject
      (
       ;; For nominal functions,
       ;; subject present ==> possessive (or objective) case - t13
       ((synt-roles ((subject given)))
	(controlled none)
	(synt-funct ((alt (#(under synt-role) none))))
	(synt-roles
	 ((subject ((cat np)
		    (gap none)
		    (syntax ((case ((alt (possessive objective)))))))))))
       ((synt-roles ((subject given))))
       ((synt-roles ((subject none))))

       ;; If subject absent it is controlled by embedding clause
       ((controlled ((gap yes)))
	(controlled {^ synt-roles subject})))))

   ((mood to-infinitive)
    ;; Allows only controlled subjects
    (alt to-infinitive-controlled-subject
	(((synt-roles ((subject given)))
	  (synt-roles ((subject ((gap yes)))))
	  (controlled {^ synt-roles subject}))
	 ((synt-roles ((subject none)))))))

   ((mood bare-infinitive)
    ;; Allows only controlled subjects
    (alt bare-infinitive-controlled-subject
	(((synt-roles ((subject given)))
	  (synt-roles ((subject ((gap yes)))))
	  (controlled {^ synt-roles subject}))
	 ((synt-roles ((subject none)))))))

   ((mood for-to-infinitive)
    ;; Requires present and ungaped subject (in objective case)
    ;; present means a cat is given
    (synt-roles ((subject given)
		 (subject ((cat given)
			   (gap none)
                           (syntax ((case objective)))))))
    ;; If this is the case - add a for
    (pattern (stop-header dots for start dots))
    (for ((cat prep) (lex "for"))))

   ((mood imperative)
    ;; Subject absent or gapped
    (synt-roles ((alt imperative-subject
                      (((subject none))
                       ((subject ((gap yes)))))))))))



;; ============================================================
;; DISPLACED CONSTITUENT
;; ============================================================

;; What can be controlled:
;; - Non-finite:
;;   Subject:
;;   - Subject of infinitive controlled by object:
;;     I want you to do it.
;;   - Subject of infinitive controlled by subject:
;;     I promise you to do it.
;;   Object:
;;   - This is the proper thing to do.
;;   Others:
;;   - This is the place in which to be.
;;   - This is the author whose book to buy.
;;   - It is too hard to cut.
;;   - Suspected burglar really didn’t know whose house he was breaking into.
;; Find pointer to controlled realization - this is controller.
;; Simplify - gap the controlled.
(def-alt controller-constituent
  (:wait {^ controlled synt-funct})
  ;; Gap the controlled constituent if found.
  (((controlled ((synt-funct given)))
    (controller {^ controlled}))
   ((controlled ((realization head)))
    (controller {^ controlled}))
   ((controller {^ controlled realization}))))

(def-alt displaced-constituent (:index mood)
  ;; This wait is necessary for long distance dependencies: you need to
  ;; wait until the scoped constituent is mapped in the embedded clauses
  ;; before processing it.
  (:wait {^ scope synt-funct})
  (((mood yes-no)
    ;; No displaced component except inversion of verb/subject
    ;; Adverb remains in regular position (unlike notf which is fronted).

    (pattern (stop-header dots fronted-aux fronted-not start dots)))

   ((mood wh)
    ;; Find pointer to scope realization - this is scoper.
    (alt wh-scope-realization
         (((scope ((synt-funct given)))
	   (scoper {^ scope}))
	  ((scope ((realization head)))
	   (scoper {^ scope}))
	  ((scoper {^ scope realization}))))
    (scoper ((gap yes)
	     (clause-level ((scoped yes)))
	     (question-pronoun {^2 scope question-pronoun})
	     (question-embedded {^2 scope question-embedded})))
    (:! question-embedded)
    ;; Add the question element in front.
    ;; Adverb is fronted in cases like "who never owned this book"
    (pattern
     (stop-header dots question
                  fronted-adverb fronted-aux fronted-not start dots)))

   ;; MOOD RELATIVE
   ((mood relative)
    ;; Find pointer to scope realization - this is scoper.
    (alt relative-scope-realization
         (((scope ((synt-funct given)))
	   (scoper {^ scope}))
	  ((scope ((realization head)))
	   (scoper {^ scope}))
	  ((scoper {^ scope realization}))))
    (scoper ((gap yes)
	     (clause-level ((scoped yes)))
	     (relative-marker {^2 scope relative-marker})
	     (relative-embedded {^2 scope relative-embedded})))
    (pattern (stop-header relative-marker dots start dots))

    (:! relative))

   ;; OTHER MOODS -- Nothing to do here
   ((mood declarative))
   ((mood bound))
   ((mood non-finite))))



(def-alt question-embedded (:index (scope question-embedded))
  ;; scope is a path to a semantic constituent on which the question is asked.
  ;; We expect to find in the semantic constituent the realization link to
  ;; the syntactic complement realizing it plus the following information:
  ;; question-embedded yes/no which determines whether the question is a
  ;; pronoun or a prep pronoun.
  ;; In the syntactic const. we find the prep to use for embedded questions
  ;; and the question-pronoun to use.
  ;; Default for embedded is no.
  ;; ***** Should check for other cats (adverb, ap).
  ;; ***** Should check features of verb to decide whether
  ;; ***** to use a pp or an np as question element
  ;; ***** Should do questions like "to which house did you
  ;; ***** go?" (long distance - scope is sub-constituent)
  ;; The features needed to determine a relative or question pronoun are:
  ;; case, restrictive, animate, possessive.
  ;; We pass them explicitly so that we do not depend on the cat affected
  ;; to the scoped constituent.
  (((scope ((question-embedded no)))
    (question {^ scoper question-pronoun})
    (question ((cat question-pronoun)
	       (restrictive {^2 scope restrictive})
	       (animate {^2 scope animate})
	       (possessive {^2 scope possessive})
	       (syntax {^2 scoper syntax})
	       (semantics ((index {^3 scoper semantics index})))))
    (alt question-embedded-question
         (((question ((lex given)))
	   (cset ((- question))))
	  ((cset ((+ question)))))))
   ((scope ((question-embedded yes)))
    (cset ((+ question)))
    (question ((cat pp)
	       (alt question-embedded-prep
                    (((prep {^2 scope question-prep})
		      (prep given))
		     ((prep {^2 scope prep})
		      (prep given))
		     ((prep {^2 scoper prep})
		      (prep given))
		     ((prep ((lex "of"))))))
	       (np {^2 scoper question-pronoun})
	       (np ((cat question-pronoun)
		    (restrictive {^3 restrictive})
		    (restrictive {^3 scope restrictive})
		    (animate {^3 scope animate})
		    (possessive {^3 scope possessive})
		    (syntax ((case objective)))
		    (semantics {^3 scoper np semantics})))
	       ;; If lex for pronoun is already given don't bother with it.
	       (alt (((np ((lex given)))
		      (cset ((- np prep))))
		     ((cset ((+ np) (- prep)))))))))))



(def-alt relative (:index (scoper relative-embedded))
  (:demo "Is the relative clause simple or embedded in a PP?")
  (:wait (({^ scope cat} #(under lexical-cat))))
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

    (scoper ((gap yes)
	     (relative-embedded no)
	     (cat simple-np)
	     (lex {^3 head lex})
	     (semantics ((index {^4 semantics index})))))
    (relative-marker {^ scoper relative-marker})
    (relative-marker ((cat relative-pronoun)
		      (restrictive {^2 scope restrictive})
		      (animate {^2 scope animate})
		      (possessive {^2 scope possessive})
		      (restrictive {^2 restrictive})
		      (syntax {^2 scoper syntax})
		      (semantics {^2 scoper semantics})))
    (alt relative-marker-restrictive
         (((relative-marker ((lex given)))
	   (cset ((- relative-marker))))
	  ;; Rule (Winograd p.479 B.1.4)
	  ;; Stylistic rule: restrictive pronoun -> that vs. which
	  ;; for animate, keep who in any case.
	  ((relative-marker ((restrictive #(under yes))
			     (animate no)
			     (lex "that")))
	   (cset ((- relative-marker))))
          ((use-that #(under yes))
           (cset ((- relative-marker)))
           (relative-marker ((lex "that"))))
	  ((cset ((+ relative-marker)))))))

   ;; possessive relative - scope is NOT determiner of an NP
   ;; it is the whole NP because you need to displace the whole thing around:
   ;; Ex: "The man WHOSE CAR I WASH [trace]."
   ;; If you had trace on {affected possessor} and were not defining
   ;; possessive-relative as a separate mood, you would get:
   ;; "The man WHOSE I WASH [trace] CAR."
   ;; Must force dative-move to no
   ((mood possessive-relative)
    (dative-move no)
    ;; ***** Copying any type of NP in any form (annoying).
    ;; Done here only for common and partitive.
    ;; @@Todo: example with partitive scoper
    (alt possessive-relative-marker
         (((scoper ((cat #(under partitive))))
	   (relative-marker ((cat partitive)
			     (part {^2 scoper part})
			     (part-of {^2 scoper part-of})
			     (part-of ((possessor ((cat relative-pronoun)))))
			     (all {^2 scoper all})
			     (prep {^2 scoper prep}))))
	  ;; Note: need to process scoper here otw all the features at the
	  ;; top level of the NP won't be propagated as needed into head
	  ;; and others (syntax...).  So add it to cset.
	  ;; Otherwise wouldn't work with:
	  ;; "The soil, whose layers are topsoil and subsoil."
	  ;; as number wouldn't be processed correctly - credit to Charles
	  ;; Brendan Callaway for finding this bug.
	  ((scoper ((cat np)))
	   (cset ((+ scoper relative-marker)))
	   (relative-marker ((cat np)
			     (lex {^2 scoper lex})
			     (head {^2 scoper head})
			     (possessor ((cat relative-pronoun)))
			     (cardinal {^2 scoper cardinal})
			     (ordinal {^2 scoper ordinal})
			     (reference {^2 scoper reference})
			     (semantics {^2 scoper semantics})
			     (syntax {^2 scoper syntax})
			     (describer {^2 scoper describer})
			     (classifier {^2 scoper classifier})
			     (qualifier {^2 scoper qualifier})))))))

   ;; embedded relative - scope is within a PP
   ((mood embedded-relative)
    (scoper ((relative-embedded yes)))
    ;; Example: the box in which we store the elixir
    ;;          an experience the likes of which you have never seen
    ;; Bind scope to embedding np head
    (scoper ((np ((semantics ((index {^5 semantics index})))))))
    (relative-marker ((cat pp)
		      (pattern (prep np))
		      (prep {^2 scoper prep})
		      (np {^2 scoper relative-marker})
		      (np ((cat relative-pronoun)
			   (syntax ((case objective)))
			   (restrictive {^3 scope restrictive})
			   (animate {^3 scope animate})
			   (possessive {^3 scope possessive})
			   (semantics {^3 scoper np semantics})))))
    (alt embedded-relative-marker
         (((relative-marker ((np ((lex given)))))
	   (cset ((- relative-marker))))
	  ((cset ((+ relative-marker)))))))

   ;; ***** be-deleted not implemented [From Winograd]
   ;; Actually debatable whether useful: in the meantime, can
   ;; generate them as AP in qualifier (cf note below on
   ;; criteria to distinguish adjectives and participles).
   ;; Example: Goldwater /who was/ crushed by his defeat
   ;;          the team /that is/ expected to win
   ;;          an enchanting island /that is/ waiting to be seen
   ;; NOTE: This is replaced by the following construct:
   ;; qualifier of an NP as:
   ;; ((qualifier ((cat clause)
   ;;              (mood past-participle)
   ;;              (controlled {path to implicit participant filled by head}))))
   #+ignore((mood be-deleted-relative))

   ;; ***** wh-nominal not implemented
   ;; Example: They were amazed at which one they chose
   ;;          I couldn't believe how many people understood
   ;;          What they did next was surprising
   ;; To rework when nominalization is implemented [cf Vendler]
   ;; NOTE: This is replaced by the following construct:
   ;; ((cat clause)
   ;;  (mood bound-nominal)
   ;;  (binder ((lex "what")))
   ;;  ...)
   #+ignore((mood wh-nominal-relative))

   ;; ***** wh-ever-nominal not implemented
   ;; Example: Whoever did it will be sorry
   #+ignore((mood wh-ever-nominal-relative))))

;; ============================================================
(provide "mood")
;; ============================================================
