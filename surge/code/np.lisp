;;; -*- Mode:Lisp; Syntax:Common-Lisp; Package: -*-
;;; -----------------------------------------------------------------------
;;; File:         np.lisp
;;; Description:  Grammatical systems for NP
;;; Author:       Michael Elhadad & Jacques Robin
;;; Created:      19 Dec 1991
;;; Modified:     12 May 1992: allowed head of common to be an embedded NP.
;;;               19 Aug 1992: merged in expanded NP (JR).
;;;               24 Jan 1993: removed fsets from top-level nominal cats (JR)
;;;                            added adverbial functions to np-functions (JR)
;;;               23 Mar 1993: added qualifying PPs in measures (JR)
;;;               28 Jun 1993: added commas before restrictive quals. (ME)
;;;               05 Dec 1993: added , after restrictive quals. (ME)
;;;               06 Dec 1993: changed partitive to gap head of part when
;;;                            not specified. (ME)
;;;               21 Dec 1993: defined np-propagate. (ME)
;;;                5 Jul 1995: SURGE 2.2 VERSION
;;;                            - Added synt-funct qualifier to qualifiers
;;;                              to avoid confusion with main-clauses.
;;;                5 Nov 1995: moved person-name to special.l
;;;                            made address and date as np-types.
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

(def-conj trivial-proper
  ;; A branch of the grammar for proper nouns that are already lexicalized
  ;; and do not want any of the expensive np processing.
  ;; ***** Do defaults for number and other features...
  (cat #(under trivial-proper))
  (pattern (head))
  (head ((lex {^2 lex})
	 (cat noun))))


;; List of synt-funct that an NP can fill - except classifier!
(def-alt np-functions
  (head
   #(under subject)
   #(under object)
   #(under subj-comp)
   #(under iobject)
   #(under dative)
   #(under obj-comp)
   #(under by-obj)
   #(under pred-adjunct)
   #(under sent-adjunct)
   #(under disjunct)))


(def-conj partitive
  ;; A sort of NP of the form: a cup of tea, 12 of 13 3 point shots.
  ;; Constituents are: part, part-of, prep, total
  (cat #(under partitive))
  (pattern (all part prep part-of))
;;  (fset (all total part prep part-of
;;	     cat pattern sss synt-funct syntax clause-level semantics))
  (alt all-partitive (:index all)
    (((all none)
      (total none))
     ((total +)
      (all ((cat phrase) (lex "all"))))))
  (prep ((cat prep) (opt ((lex "of")))))
  (part ((alt part-cat (:index cat)
	   (((cat cardinal))
	    ((cat compound-cardinal))
	    ((cat ordinal))
	    ((cat compound-ordinal))
	    ((cat common)
	     ;; Default head is the gap of the head of part-of
	     (alt (((head given))
		   ((lex given))
		   ((head ((lex {^3 part-of head lex}) (gap yes)))))))
	    ((cat measure))
	    ((cat fraction))))))
  (part-of ((alt part-of-cat (:index cat)
	      (((cat common))
	       ((cat measure))))))
  (synt-funct ((:! np-functions)))
  ;; Number agreement for partitive: all of <plural>
  (opt ((total #(under +))
	(part ((value given)
	       (control (and (numberp #@{^ value})
			     (/= #@{^ value} 1)))))
	(part-of ((cat ((alt (#(under common) #(under measure)))))
		  (number plural)))))
  )


(def-conj measure
  (cat #(under measure))
  (quantity ((alt quantity-type (((cat cardinal))
				 ((cat compound-cardinal))
				 ((cat ordinal))
				 ((cat compound-ordinal))
				 ((cat fraction))))))
  (unit ((alt (((cat noun))
	       ((cat noun-compound))))))

  ;; dots added 4/24/93 to allow split quantity constituent,
  ;; e.g., 2 games in a row at home
  (alt (((qualifier none)
	 (pattern (quantity unit dots)))
	((qualifier ((cat pp) (synt-funct qualifier)))
	 (pattern (quantity unit dots qualifier)))))

  ;; Measure used as a classifier does not put an s on unit
  (alt measure-unit-number
    (((synt-funct ((:! np-functions)))
      (unit ((number {^2 quantity number}))))
     ((synt-funct #(under classifier))
      (unit ((number singular))))))
  )


;; Recursively embedded noun compounds
;; Example: "The pleasant (((house property) tax) office) furniture)"
;; (Quirk p.919)
;; This only appears as an NP-head or classifier, not as a whole NP.
(def-conj noun-compound
  (cat #(under noun-compound))
;;  (fset (cat pattern sss number classifier head synt-funct))
  (pattern (classifier head))
  (classifier ((synt-funct classifier)
	       (alt (((cat noun))
		     ((cat verb)
		      (ending ((alt (present-participle past-participle)))))
		     ((cat adj))
		     ((cat ap))
		     ((cat measure))
		     ((cat list))
		     ((cat noun-compound))))))
  (head
   ((alt (((cat noun)
	   (number {^2 number}))
	  ((cat noun-compound)
	   (number {^2 number}))))))
  )


;; Propagate all top-level features in an NP under their appropriate group
;; syntax, semantics, reference and head.
(def-conj np-propagate
  (syntax ((fset (animate gender case definite person partitive
			  number a-an distance countable))
	   (animate {^2 animate})
	   (number {^2 number})
	   (gender {^2 gender})
	   (case   {^2 case})
	   (person {^2 person})
	   (definite {^2 definite})
	   (a-an {^2 a-an})
	   (partitive {^2 partitive})
	   (distance {^2 distance})
	   (countable {^2 countable})))
  ;; JR-11-15-92: \
  ;; allowed index at the top-level and then copied it under semantics
  ;; This saves 24 characters per co-reference pointer, making them to fit on a
  ;; single line, e.g.
  ;; ((circum ((partic ((processor ((index {^5 partic processor index}))))))))
  ;; instead of
  ;; ((circum ((partic ((processor ((semantics ((index {^5 partic processor
  ;; semantics index}))))
  (index ((fset (concept animate gender person number reference-number
		 denotation countable))
	  (concept {^2 concept})
	  (animate {^2 animate})
	  (number {^2 number})
	  (gender {^2 gender})
	  (person {^2 person})
	  (denotation {^2 denotation})
	  (reference-number {^2 reference-number})
	  (countable {^2 countable})))
  (semantics ((fset (index describer qualifier classifier))
	      (index ((fset (concept animate gender person number
				     reference-number denotation
				     countable))
		      (concept {^3 concept})
		      (animate {^3 animate})
		      (number {^3 number})
		      (gender {^3 gender})
		      (person {^3 person})
		      (denotation {^3 denotation})
		      (reference-number {^3 reference-number})
		      (countable {^3 countable})))
	      (describer {^2 describer})
	      (qualifier {^2 qualifier})
	      (classifier {^2 classifier})))
  (reference ((fset (type total selective possessive interrogative distance
			  quantitative exact orientation evaluative status
			  degree comparative superlative evaluation))
	      (type {^2 reference-type})
	      (status {^2 status})
	      (degree {^2 degree})
	      (total {^2 total})
	      (selective {^2 selective})
	      (possessive {^2 possessive})
	      (interrogative {^2 interrogative})
	      (distance {^2 distance})
	      (quantitative {^2 quantitative})
	      (exact {^2 exact})
	      (comparative {^2 comparative})
	      (superlative {^2 superlative})
	      (evaluation {^2 evaluation})
	      (orientation {^2 orientation})
	      (evaluative {^2 evaluative})))
  (head ((lex {^2 lex}))))

;; If we have reason to believe a det is needed - propagate
(def-alt np-determiner
    ;; Distance is not always propagated to determiner
    ;; If lex is already specified, don't do anything.
    (((determiner ((lex given)))
      (cset ((- determiner))))
     ((determiner ((cat det)
                   (definite  {^2 syntax definite})
                   (countable {^2 syntax countable})
                   (number    {^2 syntax number})
                   (quantitative {^2 reference quantitative})
                   (status {^2 reference status})
                   (degree {^2 reference degree})
                   (exact {^2 reference exact})
                   (comparative {^2 reference comparative})
                   (superlative {^2 reference superlative})
                   (evaluative {^2 reference evaluative})
                   (evaluation {^2 reference evaluation})
                   (total     {^2 reference total})
                   (selective {^2 reference selective})
                   (possessive {^2 reference possessive})
                   (interrogative {^2 reference interrogative})
                   (orientation {^2 reference orientation})
                   (head-denotation {^2 semantics index denotation})
                   (reference-number {^2 semantics index reference-number})
                   (partitive {^2 partitive}))))
     ((determiner none))))


;; BRANCH FOR SIMPLE-NP AND ITS SPECIALIZATIONS
(def-conj simple-np
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

  (cat #(under np))
  (complex none)
  ;; GENERAL NP =================================================
  ;; General structure: head, syntax, semantics, determiner.
  (pattern (dots head dots))
  (generic-cat np)

  (:& np-propagate)

  (:! np-type)

  (:& np-number)

  (:& np-pre-det)

  ;; DETERMINER
  ;; If possible propagate distance to determiner
  (opt det-distance ((determiner ((distance {^2 syntax distance})))))
  ;; Check if possessor is defined here - it becomes the determiner
  (alt possessor (:index possessor)
    (((possessor given)
      (possessive yes)
      (possessor ((syntax ((case possessive)))))
      ;; JR: opt added
      (opt ((determiner ((possessor {^2 possessor})
			 (possessive yes))))))
     ((possessor none)
      (possessive no))
     ((possessor nil)
      (cset ((- possessor)))
      (possessive #(under yes)))))

  (semantics ((:! describer)))

  (semantics ((:! classifier)))

  (semantics ((:! qualifier)))

  (:! np-case))



(def-alt np-type (:index cat)
  (:demo "Is this a common noun, a pronoun or a proper noun?")

  ;; COMMON NOUNS -------------------------------------------
  (((cat common)
    (np-type common)
    ;; @@@TODO: Check that common is not specified
    ;; @@@TODO: gap
    #+ignore(alt np-common-gap
     (((gap given)
       (head ((gap {^2 gap}))))
      ((gap none))))
    (alt common-determiner
	 (((gap given) (determiner none))
          ((determiner ((head-cat common)))
           (:! np-determiner)
	   (pattern (determiner dots)))
	  ((determiner none))))
    (alt np-head
     (((gap given)
       (head none))
      ((head ((cat np-head)
              (synt-funct head)
              (a-an {^2 syntax a-an})
              ;; Single head nor conjunction of nouns?
              (alt common-head (:index cat)
                   (((lex given)
                     (cat noun)
                     ;; these only accepted by morphology.
                     ;; (fset (cat generic-cat lex number a-an feature semr kind))
                     (number {^2 syntax number})
                     )
                    ((gap given))
                    ;; For measure and partitive, no agreement number head/np
                    ;; Allow for "a season high 27 points."
                    ;; Allow for "a season high 47 of 53 free throws."
                    ;; Allow for "Six assignments IS a lot."
                    ((cat #(under measure))
                     ({^}
                      ((alt measure-head-det
                            (:demo "Does an NP with a measure as head take a det?")
                            (((classifier given)
                              (determiner ((head-cat common))))
                             ((classifier none)
                              (possessor given)
                              (determiner ((head-cat common))))
                             ((classifier none)
                              (possessor none)
                              (determiner none)))))))
                    ((cat #(under partitive)))
                    ((cat #(under noun-compound))
                     (number {^2 syntax number}))
                    ((complex given)
                     ;; to make morphology understand this is not a
                     ;; simple noun.
                     (cat np-head)
                     (number {^2 syntax number})
                     (common ((cat noun)))))))))))
       
    (person third)
    (alt definite (:wait definite)
      (((definite yes))
       ((definite #(under no)))))
    (countable ((alt (yes #(under no))))))


   ;; PRONOUNS ------------------------------------------------
   ((cat pronoun)
    (np-type pronoun)
    ;; pronouns allow no classifier.
    ;; all except quantified have no describer.
    ;; can have qualifiers
    (semantics ((classifier none)))
    ;; Check whether any feature indicating possible det is specified
    (:! det-given)
    (alt pronoun-determiner
	(((determiner-given no)
          (determiner none))
         ((determiner-given yes)
          (:! np-determiner)
          (determiner ((head-cat pronoun)))
	  (pattern (determiner head dots)))
	 ((determiner none)
	  (pattern (head dots)))))
    (cset ((- head)))        ;; do not recurse on head
    (head ((cat pronoun)     ;; known to morphology plus its args.
	   ;; (fset (cat pronoun-type case gender animate feature semr kind
	   ;;        syntax lex number distance person restrictive))
	   (gender {^2 syntax gender})
	   (number {^2 syntax number})
	   (animate {^2 syntax animate})
	   (pronoun-type {^2 pronoun-type})
	   (distance {^2 syntax distance})
	   (person {^2 syntax person})
	   (restrictive {^2 restrictive})))
    ;; can have determiner put after head: they all, them all.
    ;; Check in determiner.l how the new order is enforced.
    ;; (pattern (determiner head dots))
    (:! pronoun-type)
    ;; Case: subjective, objective, possessive, reflexive
    (syntax
     ((alt pronoun-case
	  (:demo "Is the pronoun subject, object, possessive ~
                            or reflexive?")
	(((case subjective))
	 ((case given)
	  (case ((alt (objective possessive reflexive)))))))))
    ;; Propagate case from np to head only if not partitive
    (syntax
     ((alt partitive-case (:index partitive) (:wait partitive)
	(((partitive no)
	  (case {^2 head case}))
	 ((partitive yes)
	  ({^ head case} objective)))))))


   ;; PROPER NOUNS -------------------------------------------
   ((cat proper)
    (np-type proper)
    (syntax ((person third)
	     (definite yes)))
    (:! proper-type))))



(def-alt pronoun-type
  (:index cat)
  (:demo "Is this a personal, demonstrative, question or quantified pronoun?")
  ;; Pronoun-type: personal, demonstrative, question, quantified
  (((cat personal-pronoun)
    (np-type personal-pronoun)
    ;; are gender and person specific, default is third masculine.
    (head ((pronoun-type personal)))  ;; arg to morph.
    (syntax
     ((alt (:index person) (:wait person)
	  (((person third))
	   ((person #(under first)) (animate yes))
	   ((person #(under second)) (animate yes))))
      (alt gender (:index gender)
	(((gender neuter) (animate no))
	 ((gender masculine) (animate yes))
	 ((gender #(under feminine)) (animate yes))))))
    (semantics ((describer none))))
   ((cat demonstrative-pronoun)
    (np-type demonstrative-pronoun)
    (head ((pronoun-type demonstrative)))
    ;; distance does not propagate to determiner in this case
    (determiner ((distance none)))
    (syntax ((definite yes)
	     (person third)
	     (distance ((alt (far #(under near)))))))
    (semantics ((describer none))))
   ((cat relative-pronoun)
    (np-type relative-pronoun)
    (determiner ((distance none)))
    (head ((pronoun-type relative)))
    (syntax ((person third))))
   ((cat question-pronoun)
    (np-type question-pronoun)
    (determiner ((distance none)))
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


(def-alt proper-type (:index cat)
  (((cat basic-proper)
    (describer none)
    (qualifier none)
    (classifier none)
    (alt basic-proper-determiner
	(((determiner none)
          ;; number plural requires det for proper
          ;; The Smiths
          (number singular)
	  (pattern (head)))
	((determiner ((head-cat proper)))
         (:! np-determiner)
         (pattern (determiner head)))))
    (head ((cat noun)
	   (lex given)
	   ;; Only the ones accepted by morphology + the generic
	   ;; (fset (cat generic-cat lex number a-an feature semr kind))
	   (number {^2 syntax number})
	   (a-an {^2 syntax a-an}))))

   ((cat compound-proper)
    (classifier none)
    (head ((alt compound-proper-cat (((cat #(under person-name)))
				     ((cat #(under team-name)))))
	   (number {^2 number})))

    ;; note: valid here because the only two name types currently covered
    ;; - person-name and team-name - are both animate
    (animate yes)

    ;; plural requires determiner
    (alt (((number singular))
          ((number plural) (determiner-given yes))))
    ;; describer requires determiner
    (alt (((describer given) (determiner-given yes))
          ((describer none))))
    (:! det-given)

    (alt compound-proper-determiner
	(((determiner-given yes)
          (:! np-determiner)
          (determiner ((head-cat proper)
		       (alt compound-proper-head-premod
			   ((({^ describer} none)
			     (head-describer no))
			    ((head-describer yes))))))
	  (pattern (determiner describer head qualifier)))

	 ((determiner-given no)
          (determiner none)
	  (pattern (describer head qualifier)))))

    (alt (((qualifier given)
	   (qualifier ((cat clause)
		       (synt-funct qualifier))))
	  ((qualifier none)))))))



(def-conj np-number
  ;; Handle number, cardinal and ordinal for NPs
  ;; Only add cardinal to determiner if det is not none
  (opt ((cardinal given)
	(determiner any)   ;; for not-none
	(determiner ((cardinal {^2 cardinal})))))

  ;; Only add ordinal to determiner if det is not none
  (opt ((ordinal given)
	(determiner any)   ;; for not-none
	(determiner ((ordinal {^2 ordinal})))))

  ;; If cardinal is given, use its value to determine number.
  (alt cardinal-number ;; (:wait {^ cardinal value})
    (((cardinal ((value given)))
      (control (and (numberp #@{^ cardinal value})
		    (= #@{^ cardinal value} 1)))
      (syntax ((number singular))))
     ((cardinal ((numeral ((value given)))))
      (control (and (numberp #@{^ cardinal numeral value})
		    (= #@{^ cardinal numeral value} 1)))
      (syntax ((number singular))))

     ;; any other number is plural
     ((cardinal ((value given)))
      (control (numberp #@{^ cardinal value}))
      (syntax ((number plural))))
     ((cardinal ((numeral ((value given)))))
      (control (numberp #@{^ cardinal numeral value}))
      (syntax ((number plural))))

     ((syntax ((number singular))))
     ((syntax ((number #(under dual)))))
     ((syntax ((number #(under plural))))))))


(def-conj np-pre-det
  ;; Deal with fraction and multiplier
  (alt fraction (:index fraction)
    (((fraction none))
     ((fraction given)
      (determiner ((pre-det ((type fraction)))
		   (pre-det {^2 fraction}))))))
  (alt multiplier (:index multiplier)
    (((multiplier none))
     ((multiplier given)
      (determiner ((pre-det ((type multiplier)))
		   (pre-det {^2 multiplier})))))))


(def-alt describer
  (:demo "Are there describers?")
  ;; Under NP (semantics x)
  ;; ***** "Soon to be released book" "man eating tiger"
  (((describer none))
   ((describer given)
    ({^ pattern} (dots describer dots head dots))
    (describer
     ((synt-funct describer)
      (alt describer-cat (:index cat)
	(((cat adj))    ;; either a single adj
	 ((cat ap)      ;; or an ap with no modifiers @@check
	  (describer none)
	  (qualifier none))
	 ((cat list))   ;; no check on lists
	 ((cat verb)
	  (ending past-participle)
	  (modifier-type objective))
	 ((cat verb)
	  (ending present-participle)
	  (modifier-type subjective)))))))))

(def-alt classifier (:demo "Is there a classifier?")
  (((classifier none))
   ((classifier given)
    (classifier ((synt-funct classifier)))
    ({^ pattern} (dots classifier head dots))
    (classifier
     ((alt classifier-cat (:index cat)
	(((cat np-head))
	 ((cat adj))
	 ((cat ap))
	 ((cat trivial-proper))
	 ((cat np))

	 ;; JR+9/1/92
	 ((cat score))

	 ((cat #(under list)))  ;; no check on lists
	 ((cat #(under verb))
	  (ending present-participle)
	  (modifier-type subjective)))))))))


(def-alt qualifier
  (:demo "Is there a qualifier? Is it a PP or a clause?")
  (((qualifier none))
   ((qualifier given)

    ;; dots added 4/24/93 to allow split quantity constituent,
    ;; e.g., 2 games in a row at home
    ({^ pattern} (dots head dots qualifier))

    (qualifier
     ((synt-funct qualifier)
      (alt qualifier-cat (:index cat)
	(((cat pp)
	  (restrictive yes)
	  ;; send features of qualifier just to np of pp.
	  ;; This is messy - should be a different constituent.
	  ;; default prep is "of".
	  (opt ((prep ((lex "of")))))
	  (np ((syntax {^2 syntax})
	       (semantics {^2 semantics})
	       (lex {^2 lex}))))
	 ((cat #(under list)))    ;; an heterogeneous list of qualifiers
	 ;; "The elephant that came to brunch"
	 ;; "The game he played so wonderfully"

	 ;; "The fare specified on the ticket"
	 ;; Debatable whether these are adjectives or
	 ;; passive-participles - tests in general are:
	 ;; If can add very - is an adjective
	 ;; If can change "is" by "seems" or "remains" - is an adj
	 ;; In general, want the ap to have a complement except if
	 ;; there is a reason why not.
	 ((cat #(under ap)))

	 ((cat #(under clause))
	  (alt clausal-qualifier (:index mood)
	    (((mood relative))

	     ;; The car repaired by the mechanic
	     ((mood #(under past-participle))
	      (subject none)
	      (binder none))

	     ;; "The time remaining before the buzzer"
	     ((mood #(under present-participle))
	      (subject none)
	      (binder none))

	     ;; "The game to be played tomorrow"
	     ((mood #(under to-infinitive))
	      (binder none))

	     ((mood #(under for-to-infinitive))
	      (binder none)))))))

      (alt qualifier-restrictive (:index restrictive)
	   (((restrictive yes)
	     (punctuation ((before none))))
	    ((restrictive no)
	     (punctuation ((before ",") (after ",")))))))))))


(def-alt np-case (:index (syntax case))
  (:demo "Is this a possessive NP?")
  (((syntax ((case subjective))))
   ((syntax ((case given)))
    (syntax ((case ((alt (objective reflexive)))))))
   ((syntax ((case #(under possessive))))
    (alt (((semantics ((qualifier none)))
	   (head ((feature possessive))))
	  ((semantics ((qualifier given)))))))))



;; ============================================================
(provide "np")
;; ============================================================
