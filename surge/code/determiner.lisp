;;; -*- Mode:Lisp; Syntax:Common-Lisp; Package: -*-
;;; -----------------------------------------------------------------------
;;; File:         determiner.lisp
;;; Description:  Grammatical systems for determiners
;;; Author:       Michael Elhadad
;;; Created:      19 Dec 1991
;;; Modified:     14 Jul 92: Allow status to be a string.
;;;               19 Aug 92: Move cardinal/ordinal to gr-modular cats.
;;;                          Use head-determiner to decide use of "the"
;;;                          with proper name (the happy Jacques)
;;;                          Use multiplier/fraction to decide use of "the"
;;;                          with (countable no): (three times the butter).
;;;                5 Jul 95: SURGE 2.2 VERSION
;;;                          Add compound cardinal and ordinal
;;;                          "10 XX in a row" "10th straight XX"
;;;                          Add possessive-relative case (whose car is that?)
;;; -----------------------------------------------------------------------

(in-package "FUG5")


;; ============================================================
;; DETERMINER
;; ============================================================
;; Get head-cat head-case head-denotation from head of np.
;; Allow for number dual
;; Check for syntax before countable/number

;; Features of interest:
;; definite yes/no
;; countable yes/no
;; partitive no/yes
;; interrogative yes/no
;; possessive yes/no
;; number singular/dual/plural
;; reference-number singular/dual/plural (number of reference set)
;; distance far/near
;; selective yes/no
;; total +/-/none
;; orientation +/-
;; degree +/-/none
;; evaluative yes/no
;; evaluation +/-
;; status none/same/different/mentioned/specific/usual/entire
;; From NP:
;; head-case objective/subjective...
;; head-cat pronoun/common/proper
;; head-denotation quantity/season/institution/transportation/meal/illness

;; TODO
;; partitive no/measure/typical/general/quantifier
;; reference specific/non-specific
;;   non-specific: quantifier/generic
;;     quantifier: universal/existential/negative/degree/evaluative
;;       features: total/partial/orientation/evaluation
;;     generic: typical/definition (check out when plural/a/the -
;;              depending on denotation natural-kind or derived.
;;              e.g., a tiger is an animal, the tiger eats a lot, white
;;              tigers are ferocious. (derived -> plural).
;;   specific: as is now.

(def-alt det-given
  ;; Is determiner specified in input?
  ;; Check any of the features that can require usage of a det
  ;; Used in particular to decide whether a pronoun should have a det
  (((partitive no)
    (reference-number none)
    (selective none)
    (total none)
    (orientation none)
    (degree none)
    (evaluative none)
    (evaluation none)
    (status none)
    (fraction none)
    (multiplier none)
    (pre-det none)
    (determiner-given no))
   ((determiner-given yes))))


(def-conj det
  (cat det)
  ;; pre-det can be post det for all or both + can have of before det
  (pattern (dots det dots deictic2 ordinal dots))
  (cset ((- deictic2 ordinal cardinal quantifier)))
  (complex none)
  (generic-cat det)                  ;; for conjunctions

  ;; Decide whether to use a quantifier
  ;; If quantitative is yes, can still be expressed in the det part, with
  ;; things like "many"? Aha! No, if we adopt Halliday's view: when you
  ;; have "many cars" many is not like "the cars" it is like "three cars".
  ;; That is, it is still a quantifier, not a det.
  ;; Note that quantifier-det remains useful for each/every/some/any.
  ;; NOTE: the list of none enforces the implication that if any of the
  ;; feature is given then quantitative is yes.
  (alt quantitative (:index quantitative)
    (((quantitative no)
      (exact none)
      (orientation none)
      (comparative none)
      (superlative none)
      (evaluative none)
      (quantifier none)
      (cardinal none))
     ((quantitative yes)
      (alt quant-yes (:index exact)
	(((exact yes)
	  (cardinal given)
	  (quantifier none))
	 ((exact no)
	  (cardinal none)
	  (quantifier ((cat phrase)))))))))

  ;; Next choice is whether total is expressed through pre-det or within
  ;; det (all is definitely a det in "all cars").  Basically, it looks safe
  ;; to always assume total implies a pre-det. (all as a det would be
  ;; indefinite plural so we can assume that "all cars" is "all @ cars".)
  ;; We'll keep total yes / orientation - expressed as a quantifier-det for
  ;; neither and no.
  ;; How about a "none of" as a pre-det? Funny that no grammar I have
  ;; mentions "none" in the determiner section.
  (alt pre-det-total (:index total)
    (((total #(under +))
      (alt (((number plural))
	    ((countable no))))
      (total-realized pre-det)
      (pre-det ((type total))))
     ((total #(under +))
      (number singular)
      (countable yes)
      (total-realized det))
     ((total #(under -))
      (total-realized det)
      (pre-det none))
     ((total none)
      (total-realized none))))


  ;; Deal with pre-det
  (:! pre-det)

  ;; Deictic2 Halliday IFG p.162
  (:! deictic2)

  ;; Ordinal system
  (:! ordinal)

  ;; Cardinal exclusive with quantifier
  (alt cardinal-quantifier
      (((cardinal none))
       ((cardinal given)
	(quantifier none)
	(pattern (dots det dots cardinal))
	(cardinal ((cat ((alt (cardinal compound-cardinal)))))))))

  ;; Quantifier system
  (:! quantifier)

  ;; The determiner itself
  (:! det-type))



;; The cat fraction has features num, det and digit
(def-alt pre-det
  (((pre-det none))
   ((pre-det given)
    (alt pre-det1 (:index (pre-det type))
      (((pre-det ((type total)))
	(cset ((- pre-det of)))
	;; Cooccurrence constraints with det (Quirk p.140 4.19)
	;; Ignore half.
	(pre-det ((cat article)))
	(alt (((number #(under dual))
	       (countable yes)
	       (quantifier none)
	       (pre-det ((lex "both"))))
	      ((number plural)
	       (countable yes)
	       (pre-det ((lex "all"))))
	      ((number singular)
	       (countable yes)
	       (pre-det ((lex "all")))
	       (definite yes))
	      ((countable no)
	       (pre-det ((lex "all"))))))
	;; Put after head? Use of? Necessary for pronouns
	;; "them all" "all of them"
	;; For others, preferred when quantifier/cardinal is used.
	(alt pre-det-total-position
	  (((head-cat pronoun)
	    (ordinal none)
	    (cardinal none)
	    (alt pre-det-post-head (:index partitive)
	      (((pattern (det dots))
		(partitive no)
		({^ pattern} (determiner head {^ determiner pre-det} dots)))
	       ((of ((cat prep) (lex "of")))
		(partitive yes)
		(cset ((- of)))
		(pattern (pre-det of det dots))
		(deictic2 none)))))
	   ((head-cat ((alt (common proper))))
	    (alt (((alt (((quantifier given))
			 ((cardinal given))
			 ((partitive #(under yes)))))
		   (of ((cat prep) (lex "of")))
		   (partitive yes)
		   (cset ((- of)))
		   (pattern (pre-det of det dots)))
		  ((quantifier none)
		   (cardinal none)
		   (pattern (pre-det det dots)))))))))

       ;; type multiplier: Quirk p.142 4.20
       ((pre-det ((type multiplier)))
	(pattern (pre-det dots det dots))
	;; Cooccurrence constraints
	;; *****  Check them: find example of mult + count.plur
	(alt (((countable yes)
	       (number plural))
	      ((countable no))
	      ((countable yes)
	       (number singular)
	       (head-denotation quantity))))
	(definite yes)
	;; Value x times
	(alt (((pre-det ((value 1)
			 (cat phrase)
			 (lex "once"))))
	      ((pre-det ((value 2)
			 (cat phrase)
			 (lex ((ralt ("double" "twice")))))))
	      ((pre-det ((control (numberp #@{^ value}))))
	       (times ((cat phrase) (lex "times")))
	       (pre-det ((cat cardinal)))
	       (cset ((- times)))
	       (pattern (pre-det times det dots))))))

       ;; type fraction: Quirk p.142 4.21
       ((pre-det ((type fraction)
		  (cat fraction)))
	(alt pre-det-fraction
	  (((partitive yes)
	    ;; "half of the boxes", "half of an hour"
	    (of ((cat prep) (lex "of")))
	    (cset ((- of)))
	    (pattern (pre-det of det dots)))
	   ((partitive no)
	    ;; "half an hour", "half the people"
	    ;; Should restrict it to HALF only *****
	    (pattern (pre-det det dots)))))))))))



(def-conj fraction
  (cat fraction)
  (control (numberp #@{^ num}))
  (control (numberp #@{^ den}))
  (digit ((alt (no yes)))))


(def-alt deictic2
  (((status none)
    (deictic2 none))
   ((control (stringp #@{^ status}))
    (deictic2 ((cat adj) (lex {^2 status}))))
   ((status given)
    (deictic2 ((cat adj)
	       (type {^ ^ status})
	       (alt (((lex given))
		     ((type same)
		      (lex ((ralt ("same" "identical")))))
		     ((type different)
		      (lex ((ralt ("other" "different")))))
		     ((type mentioned)
		      (lex ((ralt ("given" "above" "aforementioned")))))
		     ((type specific)
		      (lex ((ralt ("particular" "certain" "specific"
				   "original" "special")))))
		     ((type usual)
		      (lex ((ralt ("usual" "well-known" "normal"
				   "typical" "habitual" "expected")))))
		     ((type entire)
		      (lex ((ralt ("entire" "complete" "whole"))))))))))))

(def-alt ordinal
  (((ordinal none))
   ((ordinal given)
    (ordinal ((cat ((alt (ordinal compound-ordinal))))))
    ;; Cf Winograd p.514
    (definite yes)
    ;; Cf Quirk p.143 4.22
    (alt (((ordinal ((value ((alt (1 + last <>))))))
	   (number plural)
	   (countable yes))
	  ((cardinal none)
	   (quantifier none)
	   (number singular)
	   (countable yes)))))))


(def-alt quantifier
  ;; Quirk p.144 4.25 & 4.26
  (((quantifier none))
   ((quantifier any)
    (cardinal none)
    (quantifier ((cat phrase)
		 (countable {^ ^ countable})
		 (orientation {^ ^ orientation})
		 (degree {^ ^ degree})
		 (partitive {^ ^ partitive})
		 (comparative {^ ^ comparative})
		 (superlative {^ ^ superlative})
		 (evaluative {^ ^ evaluative})
		 (evaluation {^ ^ evaluation})))
    ;; Make it a ralt to allow going to the 3rd branch
    (alt quantifier1 (:index countable) (:order :random)
      (((countable yes)
	(number plural)
	(quantifier ((:! quant-count-plural))))

       ((countable #(under no))
	(quantifier ((:! quant-mass))))

       ;; This is both for count-plur and mass
       ;; a lot of, lots of, plenty of
       ((definite no)
	(alt (((number plural))
	      ((countable #(under no)))))
	(pre-det none)
	(ordinal none)
	(evaluative no)
	(evaluation none)
	(partitive yes)
	(degree #(under +))
	(orientation +)
	(full-quantifier yes)
	(quantifier
	 ((system open)
	  (orientation +)
	  (pattern (quant-head of))
	  (cset ((- quant-head of)))
	  (quant-head ((cat phrase)
		       (lex ((ralt ("a lot" "lots" "plenty"))))))
	  (of ((cat prep) (lex "of"))))))))
    (pattern (dots det dots quantifier)))))


(def-alt quant-count-plural (:index orientation)
  ;; Under quantifier
  (((orientation +)
    ;; many, several, a great many, a good many, a few, more, most, enough,
    ;; too many, a large/great/good number of
    (alt quant-count-plural-pos (:index system)
      (((system closed)
	;; The many students
	(alt quant-cppos-eval (:index evaluative)
	  (((evaluative no)
	    (evaluation none)
	    (alt quant-cppos-comp (:index comparative)
	      (((comparative no)
		(superlative no)
		(alt ((({^ definite} yes)
		       (degree #(under +))
		       (lex ((ralt ("many" "several")))))
		      (({^ definite} no)
		       ({^ full-quantifier} yes)
		       (alt quant-cppos-degree (:index degree)
			 (((degree #(under +))
			   (lex ((ralt ("a great many" "a good many")))))
			  ((degree #(under -))
			   (lex "a few")))))
		      (({^ definite} no)
		       ({^ full-quantifier} indef)
		       (degree #(under +))
		       (lex ((ralt ("many" "several"))))))))
	       ((comparative yes)
		(superlative no)
		({^ pre-det} none)
		({^ distance} none)
		({^ full-quantifier} indef)
		(lex "more"))
	       ((superlative yes)
		(comparative no)
		(degree +)
		({^ pre-det} none)
		({^ distance} none)
		({^ full-quantifier} indef)
		(lex "most")))))
	   ((evaluative #(under yes))
	    (alt quant-cppos-eval-dir (:index evaluation)
	      (((evaluation +)
		({^ definite} no)
		({^ full-quantifier} yes)
		(lex "enough"))
	       ((evaluation -)
		(degree +)
		({^ full-quantifier} indef)
		(lex "too many")))))))
	(:! quant-partitive))
       ((system open)
	;; a/the/this/my great number of students
	(partitive yes)
	(interrogative no)
	(evaluative no)
	(evaluation none)
	(superlative no)
	(comparative no)
	(degree +)
	(pattern (quant-mod quant-head of))
	(cset ((- quant-mod quant-head of)))
	(quant-head ((cat noun) (lex ((alt (given "number"))))))
	(quant-mod ((cat adj)
		    (lex ((alt (given
				((ralt ("large" "great" "good")))))))))
	({^ head-case} objective)
	(of ((cat prep) (lex "of")))))))
   ((orientation #(under -))
    ;; few, fewer, fewest, enough, too few, a small/tiny number of
    (alt quant-count-plural-neg (:index system)
      (((system closed)
	(alt quant-cpneg-eval (:index evaluative)
	  (((evaluative no)
	    (evaluation none)
	    (alt quant-cpneg-comp (:index comparative)
	      (((comparative no)
		(superlative no)
		({^ full-quantifier} indef)
		(lex "few"))
	       ((comparative yes)
		(superlative no)
		({^ pre-det} none)
		({^ distance} none)
		({^ full-quantifier} indef)
		(lex "fewer"))
	       ((superlative yes)
		(comparative no)
		({^ pre-det} none)
		({^ definite} yes)
		({^ distance} none)
		(lex "fewest")))))
	   ((evaluative #(under yes))
	    (alt quant-cpneg-eval-dir (:index evaluation)
	      (((evaluation +)
		({^ definite} no)
		({^ full-quantifier} yes)
		(lex "enough"))
	       ((evaluation -)
		({^ full-quantifier} indef)
		(lex "too few")))))))
	(:! quant-partitive))
       ((system open)
	(evaluative no)
	(evaluation none)
	(superlative no)
	(comparative no)
	(pattern (quant-mod quant-head of))
	(cset ((- quant-mod quant-head of)))
	(quant-head ((cat noun) (lex "number")))
	(quant-mod ((cat adj)))
	(alt (((degree none)
	       (quant-mod ((lex "small"))))
	      ((degree #(under +))
	       (quant-mod ((lex "tiny"))))
	      ((quant-mod ((lex given))))))
	(of ((cat prep) (lex "of")))))))))



(def-alt quant-partitive (:index partitive)
  ;; Under quantifier - for closed system, of required for pronouns
  ;; many of them // * many of cars // much of him // * much of butter
  (((partitive yes)
    ({^ head-cat} pronoun)
    ({^ interrogative} no)
    ({^ pre-det} none)
    (pattern (quant-head of))
    (quant-head ((lex {^ ^ lex}) (cat article)))
    (of ((cat prep) (lex "of")))
    (cset ((- quant-head of))))
   ((partitive no)
    ({^ head-cat} ((alt (common proper)))))))


(def-alt quant-mass (:index system)
  ;; Under quantifier
  (((system closed)
    (alt quant-mass-closed-eval (:index evaluative)
      (((evaluative no)
	(evaluation none)
	;; a little, some, much, more, most / little, less, least
	(alt quant-mass-closed-comp (:index comparative)
	  (((comparative no)
	    (superlative no)
	    ({^ ordinal} none)
	    (alt quant-mass-closed1 (:index orientation)
	      (((orientation +)
		({^ definite} no)
		({^ full-quantifier} yes)
		(alt quant-mass+ (:index degree)
		  (((degree none)
		    (lex "some"))
		   ((degree #(under +))
		    (lex "much"))
		   ((degree #(under -))
		    (lex "a little")))))
	       ((orientation #(under -))
		({^ full-quantifier} indef)
		(lex "little")))))
	   ((comparative yes)
	    (superlative no)
	    ({^ pre-det} none)
	    ({^ ordinal} none)
	    ({^ definite} no)
	    (alt quant-mass-closed2 (:index orientation)
	      (((orientation +)
		({^ full-quantifier} indef)
		(lex "more"))
	       ((orientation #(under -))
		({^ full-quantifier} indef)
		(lex "less")))))
	   ((superlative yes)
	    (comparative no)
	    ({^ pre-det} none)
	    ({^ ordinal} none)
	    ({^ full-quantifier} indef)
	    (degree +)
	    (alt quant-mass-closed3 (:index orientation)
	      (((orientation +)
		(lex "most"))
	       ((orientation #(under -))
		(lex "least"))))))))
       ((evaluative #(under yes))
	(alt quant-mass-eval-dir (:index evaluation)
	  (((evaluation +)
	    ({^ full-quantifier} yes)
	    (lex "enough"))
	   ((evaluation -)
	    (alt quant-mass-eval-neg (:index orientation)
	      (((orientation +)
		({^ full-quantifier} indef)
		(lex "too much"))
	       ((orientation -)
		({^ full-quantifier} indef)
		(lex "too little"))))))))))
    (:! quant-partitive))
   ((system open)
    ;; a good deal of, a/the/my/this/some small/large quantity of
    (evaluative no)
    (evaluation none)
    (superlative no)
    (comparative no)
    (degree +)
    ({^ partitive} yes)
    (of ((cat prep) (lex "of")))
    (pattern (quant-mod quant-head of))
    (cset ((- quant-mod quant-head of)))
    (alt quant-mass-open (:index orientation)
      (((orientation +)
	({^ full-quantifier} yes)
	(quant-head ((cat noun) (lex "deal")))
	({^ definite} no)
	(quant-mod ((cat adj) (lex ((alt (given
					  ((ralt ("a great" "a good"))))))))))
       ((quant-head ((cat noun) (lex ((ralt ("quantity" "amount"))))))
	(alt quant-mass-open-partitive (:index orientation)
	  (((orientation +)
	    (quant-mod ((cat adj) (lex ((alt (given
					      ((ralt ("large" "great"
						      "good"))))))))))
	   ((orientation #(under -))
	    (quant-mod ((cat adj) (lex "small"))))))))))))



(def-alt det-type (:index cat)
  (:demo "Is this a demonstrative, possessive or article determiner?")
  (((cat article-det)
    (alt (((total none))
	  ((total-realized #(under pre-det)))))
    (selective none)
    (distance none)
    (interrogative no)
    (possessive no)
    (det ((cat article)))
    (:! article-det))

   ((cat possessive-det)
    (possessive #(under yes))
    (definite yes)
    ;; (total none)   ;; c35 was failing (all their holes)
    (selective none)
    (interrogative no)
    (distance none)
    (reference specific)
    (possessor {^ det})
    (:! possessive-det))

   ((cat question-det)
    (interrogative #(under yes))
    (total none)
    (distance none)
    ;; can be possessive question
    (:! question-det))

   ((cat demonstrative-det)
    (definite yes)
    ;; (total none)  ;; all this work
    (selective none)
    (interrogative no)
    (possessive no)
    (distance given)
    (reference specific)
    (det ((cat article)))
    (:! demonstrative-det))

   ((cat quantifier-det)
    (interrogative no)
    (distance none)
    (possessive no)
    (definite no)
    (reference non-specific)
    (det ((cat article)))
    (:! quantifier-det)
    ;; Partitive?
    (alt quant-det-partitive (:index partitive)
      (((partitive no)
	(head-cat ((alt (common proper))))
	(quantifier none))
       ((partitive yes)
	(head-cat pronoun)
	(countable no)
	(of ((cat prep) (lex "of")))
	(pattern (dots det of dots))
	(cset ((- of)))))))))


(def-alt demonstrative-det (:index distance)
  (:demo "Is this a reference to a near or to a far object?")
  (((distance near)
    (alt (:index number)
	(((number singular)
	  (det ((lex "this"))))
	 ((partitive #(under yes))
	  (det ((lex "this"))))
	 ((number plural)
	  (partitive no)
	  (det ((lex "these")))))))
   ((distance #(under far))
    (alt (:index number)
	(((number singular)
	  (det ((lex "that"))))
	 ((partitive #(under yes))
	  (det ((lex "that"))))
	 ((number plural)
	  (partitive no)
	  (det ((lex "those")))))))))


(def-alt possessive-det
  ;; Careful here on the meaning of number:
  ;; distinguish between number of the possessive det and of the
  ;; head of the np.  In [my first two years] my is singular and
  ;; years is plural...
  ;; Features of the possessor det are under possessor (which is
  ;; conflated with head).
  (((possessor ((cat #(under relative-pronoun))))
    (cset ((- possessor det)))
    (possessor ((lex "whose"))))

   ((cset ((- possessor det)))  ;; must also say det even though det/poss
				;; are conflated (bug with cset).
    (possessor
     ((cat #(under pronoun))
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
	    (alt poss-pron-num (:index number)
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
   ((possessor ((cat ((alt (common proper))))
		(syntax ((case possessive))))))))


(def-alt article-det (:demo "Choose between A, THE and nothing")
  (((definite yes)
    ;; Deal with zero-article nouns
    (alt article-det-definite
	 (((head-denotation #(under zero-article-thing))
	   (det ((lex ""))))
	  ((head-cat proper)
	   (alt number-proper (:index number)
	     (((number singular)
	       ;; JR-added alt
	       (alt article-det-describer
		   (((alt head-describer (((head-describer #(under yes))
					   (det ((lex "the"))))
					  ((det ((lex ""))))))))))
	      ((number plural)
	       (full-quantifier ((alt (none #(under indef)))))
	       (det ((lex "the")))))))
	  ((head-cat pronoun)
	   (alt (((quantifier given)
		  (full-quantifier ((alt (none #(under indef)))))
		  (det ((lex "the"))))
		 ((quantifier none)
		  (det ((lex ""))))
		 ((quantifier given)
		  (full-quantifier #(under yes))
		  (det ((lex "")))))))
	  ((countable #(under no))
	   ;; JR-added alt: three times *the* butter
	   (ALT (((pre-det ((type #(under multiplier))))
		  (det ((lex "the"))))
		 ((pre-det ((type #(under fraction))))
		  (det ((lex "the"))))
		 ((det ((lex "")))))))
	  ((head-cat common)
	   (countable yes)
	   (head-denotation article-thing)
	   (full-quantifier ((alt (none #(under indef)))))
	   (det ((lex "the"))))
	  ((full-quantifier #(under yes))
	   (det ((lex "")))))))

   ((definite #(under no))
    (countable yes)
    (alt article-det-indefinite
	 (((full-quantifier ((alt (#(under yes) #(under indef)))))
	   (det ((lex ""))))
	  ((number singular)
	   (full-quantifier none)
	   (cardinal none)
	   (det ((lex "a"))))
	  ((partitive #(under yes))
	   (full-quantifier none)
	   (cardinal none)
	   (det ((lex "a"))))
	  ((cardinal given)
	   (number singular)
	   (det ((lex ""))))
	  ((number #(under plural))
	   (partitive no)
	   (det ((lex "")))))))

   ((countable #(under no))
    (definite no)
    (alt partitive-mass (:index partitive)
      (((partitive #(under yes))
	(full-quantifier none)
	(alt (((head-cat ((alt (common proper)))))
	      ((head-cat pronoun)
	       (quantifier given))))
	(det ((lex "a"))))
       ((full-quantifier ((alt (#(under yes) #(under indef)))))
	(det ((lex ""))))
       ((partitive no)
	(det ((lex "")))))))))


(def-alt question-det (:index possessive)
  (((possessive no)
    (selective yes)
    (det ((cat article) (lex "which"))))
   ((possessive no)
    (selective no)
    (det ((cat article) (lex "what"))))
   ;; Whose car is that?
   ((possessive yes)
    (definite yes)
    (alt (((possessor none))
	  ((possessor ((cat #(under pronoun)))))))
    (cset ((- possessor)))
    (det ((cat article) (lex "whose"))))
   ;; Which man's car is that?
   ((possessive yes)
    (definite yes)
    (possessor given)
    (det {^ possessor})
    (possessor ((cat ((alt (common proper))))   ;; not pronoun
		(syntax ((case possessive)))
		(cset ((- determiner)))
		(determiner ((det ((lex "which")
				   (cat article)))
			     (pattern (det dots))
			     (possessive no)
			     (interrogative yes)
			     (selective yes))))))))


;; If total is already expressed in pre-det, det is ""
(def-alt quantifier-det (:index total)
  (((total +)
    (total-realized det)
    (countable yes)
    ;; all, each, both, every
    (alt quant-total-number+ (:index number)
      (((number singular)
	(reference-number plural)
	(det ((lex ((alt ("each" "every")))))))
       ((number #(under dual))
	(quantifier none)
	(det ((lex "both"))))
       ((number plural)
	(det ((lex "all")))))))
   ((total #(under -))
    (total-realized det)
    (alt quant-total-number- (:index number)
      (((reference-number #(under dual))
	(number singular)
	(det ((lex "neither"))))
       ((number singular)
	(det ((lex "no"))))
       ((number plural)
	(det ((lex "no")))))))
   ((total given)
    (total-realized pre-det)
    (det ((lex ""))))
   ((total none)
    ;; one, either, some, any
    (alt quant-partial (:index selective)
      (((selective yes)
	(reference-number plural)
	(countable yes)
	;; one, either, some, any
	(alt quant-partial-sel (:index reference-number)
	  (((number singular)
	    (reference-number #(under dual))
	    (countable yes)
	    (det ((lex "either"))))
	   ((number singular)
	    (det ((lex "one"))))
	   (;; number/countable unmarked
	    (reference-number plural)
	    ;; OH NO!!! THE ANY/SOME ALTERNATION!!!! *****
	    ;; Check summary of problems in Winograd's notes on semantics
	    ;; and in Robin Lakoff's paper.  Basically use some in positive
	    ;; clauses, any in any "negative" context.  Check out influence
	    ;; of Argumentation on this definition of negative.
	    (det ((lex ((alt ("some" "any"))))))))))
       ((selective no)
	(alt quant-partial-non-sel (:index countable)
	  (((countable yes)
	    (number plural)
	    (evaluative #(under yes))
	    (det ((lex "enough"))))
	   ((countable yes)
	    (number singular)
	    (full-quantifier none)
	    (head-cat ((alt (common proper))))
	    (partitive no)
	    (det ((lex "a"))))
	   ((countable #(under no))
	    (evaluative #(under yes))
	    (det ((lex "enough"))))
	   ((countable #(under no))
	    (degree +)
	    (evaluative no)
	    (det ((lex "much"))))
	   ((countable #(under no))
	    (det ((lex "some"))))))))))))


;; ============================================================
(provide "determiner")
;; ============================================================
