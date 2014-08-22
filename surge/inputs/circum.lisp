;;; -*- Mode:Lisp; Syntax:Common-Lisp; -*-
;;; ------------------------------------------------------------
;;; File        : circum.lisp
;;; Description : Sample inputs to test new list of circumstancials
;;;               & other SURGE additions necessary in the NBA domain
;;; Author      : Jacques Robin
;;; Created     : 11/1/91
;;; Modified    :
;;; Language    : FUF
;;; ------------------------------------------------------------
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

(in-package :fug5)

;; Tests: location as pred-modif PP
;;        direction as adverb
;;        origin, destination and distance as PP
;;        path
;;        co-occurrence of all spatial roles
(def-test c1
  "In France, Bo biked south 300 miles along the Rhone from Lyon to the Camargue."
  ((cat clause)
   (tense past)
   (process ((type material) (effective no) (lex "bike")))
   (partic ((agent ((cat basic-proper) (lex "Bo")))))
   (pred-modif ((direction ((cat adv) (lex "south")))
		(distance ((cat measure)
			   (quantity ((value 300)))
			   (unit ((lex "mile")))))
		(path ((cat pp)
		       (prep ((lex "along")))
		       (np ((cat basic-proper) (lex "the Rhone")))))
		(origin ((cat pp)
			 (np ((cat basic-proper) (lex "Lyon")))))
		(destination ((cat pp)
			      (np ((cat basic-proper) (lex "the Camargue")))))))
   (circum ((location ((cat pp)
		       (prep ((lex "in")))
		       (np ((cat basic-proper)
			    (lex "France")))))))))


;; Tests: co-occurrence of 2 locations, 1 pred-modif & 1 circum
(def-test c2
  "On the platform, Bo kissed her on the cheek."
  ((cat clause)
   (tense past)
   (process ((type material) (lex "kiss")))
   (partic ((agent ((cat basic-proper) (lex "Bo")))
	    (affected ((cat personal-pronoun) (gender feminine)))))
   (pred-modif ((location ((cat pp)
			   (prep ((lex "on")))
			   (np ((cat common) (lex "cheek")))))))
   (circum ((location ((cat pp)
		       (prep ((lex "on")))
		       (np ((cat common) (lex "platform")))))))))


;; Tests: location as adverb and finite clause
(def-test c3
  "There, Bo kissed her where she wanted."
  ((cat clause)
   (tense past)
   (process ((type material) (lex "kiss")))
   (partic ((agent ((cat basic-proper) (lex "Bo")))
	    (affected ((cat personal-pronoun) (gender feminine)))))
   (pred-modif
    ((location ((cat clause)
		(mood bound-adverbial)
		(tense past)  ;; how time agreement enforced in grammar?
		(process ((type mental) (lex "want") (transitive no)))
		(partic ((processor ((cat personal-pronoun)
				     (index {^5 partic affected index})))))))))
   (circum ((location ((cat adv) (lex "there")))))))


;; Tests: duration as measure
;;        frequency as common
;;        time as common
;;        co-occurrence of 3 temporal roles
;;        inclusion as PP
(def-test c4
  "This month, Bo works out four hours each day, including Sundays."
  ((cat clause)
   (process ((type material) (effective no) (lex "work out")))
   (partic ((agent ((cat basic-proper) (lex "Bo")))))
   (pred-modif ((duration ((cat measure)
			   (quantity ((value 4)))
			   (unit ((lex "hour")))))))
   (circum ((frequency ((cat common)
			(total +)
			(number singular)
			(lex "day")
			(position end)))
	    (time ((cat common) (distance near) (lex "month") (position front)))
	    (inclusion ((cat pp)
			(np ((cat common)
			     (number plural)
			     (definite no)
			     (lex "Sunday")))))))))

(store-verbs
 '(("work out" "works out" "worked out" "working out" "worked out")))


;; Tests: direction as common
;;        destination as adverb
;;        time as adverb
;;        frequency as adverb
(def-test c5
  "Often, Bo runs this way home now."
  ((cat clause)
   (process ((type material) (effective no) (lex "run")))
   (partic ((agent ((cat basic-proper) (lex "Bo")))))
   (pred-modif ((destination ((cat adv) (lex "home")))
		(direction ((cat common) (distance near) (lex "way")))))
   (circum ((frequency ((cat adv) (lex "often")))
	    (time ((cat adv) (lex "now")))))))

(store-verbs '(("run" "runs" "ran" "running" "ran")))

;; Note: a more sophisticated treatment of adverbial ordering taking
;; into account a wide range of constraints including semantic and
;; realization length is needed, e.g. to generate "Bo often runs home this
;; way now." instead of c5. It's a tough problem. Will be tackled using
;; domain-dependent corpus observations in STREAK. If encoded as revisions,
;; a way to specify partial ordering constraints in the input overriding
;; the default is needed.

;; Either you specify by order - no semantic role.
(def-test c5bis
  "Bo runs home this way often now."
  ((cat clause)
   (process ((type material) (effective no) (lex "run")))
   (partic ((agent ((cat basic-proper) (lex "Bo")))))
   (end-adverbial-1 ((cat adv) (lex "home")))
   (end-adverbial-2 ((cat common) (distance near) (lex "way")))
   (end-adverbial-3 ((cat adv) (lex "often")))
   (end-adverbial-4 ((cat adv) (lex "now")))))


;; Or you use the semantic roles and rely on default ordering.
(def-test c5ter
  "Often, Bo runs this way home now."
  ((cat clause)
   (process ((type material) (effective no) (lex "run")))
   (partic ((agent ((cat basic-proper) (lex "Bo")))))
   (pred-modif ((destination ((cat adv) (lex "home")))
		(direction ((cat common) (distance near) (lex "way")))))
   (circum ((frequency ((cat adv) (lex "often")))
	    (time ((cat adv) (lex "now")))))))

;; Alternative analysis with destination as participant
(def-test c5quad
  "Often, Bo runs home this way now."
  ((cat clause)
   (process ((type composite) (relation-type locative) (lex "run")))
   (partic ((agent ((cat basic-proper) (lex "Bo")))
	    (located {^ agent})
	    (location ((cat adv) (lex "home")))))
   (pred-modif ((direction ((cat common) (distance near) (lex "way")))))
   (circum ((frequency ((cat adv) (lex "often")))
	    (time ((cat adv) (lex "now")))))))


;; Tests: location as verbless clause
;;        time as finite clause
(def-test c6
  "As soon as you find it, keep it where accessible to all authorized users."
  ((cat clause)
   (mood imperative)
   (process ((type material) (lex "keep")))
   (partic ((agent ((index {^3 circum time partic processor index})))
	    (affected ((cat personal-pronoun)))))
   (pred-modif
    ((location
      ((cat clause)
       (mood verbless)
       (controlled {^ partic carrier})
       (process ((type ascriptive)))
       (partic ((carrier ((index {^5 partic affected index})))
		(attribute ((cat ap)
			    (head ((lex "accessible")))
			    (qualifier ((cat pp)
					(prep ((lex "to")))
					(np ((cat common)
					     (number plural)
					     (definite no)
					     (total +)
					     (describer ((lex "authorized")))
					     (head ((lex "user")))))))))))))))
   (circum
    ((time ((cat clause)
            (mood bound-adverbial)
            (binder ((lex "as soon as")))
            (process ((type mental) (lex "find")))
            (partic ((processor ((cat personal-pronoun) (person second)))
                     (phenomenon ((cat personal-pronoun)
                                  (index {^5 partic affected index})))))))))))


;; Tests: direction as PP
;;        distance as common
;;        time as present-participle clause
(def-test c7
  "After lifting weights, Tony biked up the mountain a few miles."
  ((cat clause)
   (tense past)
   (process ((type material) (effective no) (lex "bike")))
   (partic ((agent ((cat basic-proper) (lex "Tony")))))
   (pred-modif ((direction ((cat pp)
			    (prep ((lex "up")))
			    (np ((cat common) (lex "mountain")))))
		(distance ((cat common)
			   (lex "mile")
			   (definite no)
			   (orientation +)
			   (exact no)
			   (number plural)
			   (degree -)))))
   (circum ((time ((cat clause)
		   (mood present-participle)
		   (controlled {^ partic agent})
		   (binder ((lex "after")))
		   (process ((type material) (effective no) (lex "lift")))
		   (partic ((agent ((index {^5 partic agent index})))
			    (range ((cat common)
				    (definite no)
				    (number plural)
				    (lex "weight")))))))))))


;; Tests: complex time PP
(def-test c8
  "I will call you on Monday, at noon."
  ((cat clause)
   (tense future)
   (process ((type verbal) (lex "call")))
   (partic ((sayer ((cat personal-pronoun) (person first)))
	    (addressee ((cat personal-pronoun) (person second)))))
   (circum ((time ((cat pp)
		   (complex apposition)
		   (distinct ~(((prep ((lex "on")))
				(np ((cat basic-proper) (lex "Monday"))))
			       ((prep ((lex "at")))
				(np ((cat basic-proper) (lex "noon"))))))))))))


;; Tests: time as past-participle clause
;;        duration as adverb
(def-test c9
  "Once refused a new contract, Bo held out indefinitely."
  ((cat clause)
   (tense past)
   (process ((type composite) (relation-type locative) (lex "hold")))
   (partic ((agent ((cat basic-proper) (lex "Bo")))
	    (located {^ agent})
	    (location ((cat adv) (lex "out")))))
   (pred-modif ((duration ((cat adv) (lex "indefinitely")))))
   (circum ((time ((cat clause)
		   (mood past-participle)
		   (controlled {^ partic possessor})
		   (binder ((lex "once")))
		   (process ((type composite)
			     (relation-type possessive)
			     (lex "refuse")))
		   (partic ((possessor ((index {^5 partic processor index})))
			    (possessed ((cat common)
				        (definite no)
					(describer ((lex "new")))
					(head ((lex "contract")))))
			    (affected {^ possessor})))))))))
;; no agent allowed despite default (agentive yes),
;; since automatically ellided by part-participle mood
;; which implies (voice agentless-passive)

(store-verbs '(("hold" "holds" "held" "holding" "held")))


;; Tests: duration as PP
;;        frequency as PP
(def-test c10
  "For two years, Bo worked out on Sundays."
  ((cat clause)
   (tense past)
   (process ((type material) (effective no) (lex "work out")))
   (partic ((agent ((cat basic-proper) (lex "Bo")))))
   (circum ((frequency ((cat pp)
			(position end)
			(prep ((lex "on")))
			(np ((cat common)
			     (definite no)
			     (number plural)
			     (lex "Sunday")))))
	    (duration ((cat pp)
		       (position front)
		       (np ((cat measure)
			    (quantity ((value 2)))
			    (unit ((lex "year")))))))))))


;; Tests: duration as finite clause
;;        reason as PP
(def-test c11
  "Because of his injury, Bo did not play until the playoffs started."
  ((cat clause)
   (tense past)
   (polarity negative)
   (process ((type material) (effective no) (lex "play")))
   (partic ((agent ((cat basic-proper) (lex "Bo") (gender masculine)))))
   (circum ((reason ((cat pp)
		     (np ((cat common)
			  (possessor ((cat personal-pronoun)
				      (index {^5 partic agent index})))
			  (lex "injury")))))
	    (duration ((cat clause)
		       (tense past)
		       (mood bound-adverbial)
		       (binder ((lex "until")))
		       (process ((type material)
				 (agentive no)
				 (effect-type creative)
				 (lex "start")))
		       (partic ((created ((cat common)
					  (number plural)
					  (lex "playoff")))))))))))

;; Tests: duration as present-participle clause
;;        co-event as present-participle clause
;;        present-participle clause with subject
(def-test c12
  "With his knees hampering him, Blackman has not played since becoming a Knick."
  ((cat clause)
   (tense present-perfect)
   ;; note the tense constraint: past tense in the matrix
   ;; would require a finite subordinate
   (polarity negative)
   (process ((type material) (effective no) (lex "play")))
   (partic ((agent ((cat basic-proper) (lex "Blackman") (gender masculine)))))
   (circum
    ((duration ((cat clause)
		(mood present-participle)
		(controlled {^ partic agent})
		(binder ((lex "since")))
		(process ((type composite)
                          (relation-type ascriptive)
                          (lex "become")))
		(partic ((agent ((index {^5 partic agent index})))
			 (carrier {^ agent})
			 (attribute ((cat common)
				     (definite no)
				     (head ((lex "Knick")))))))))
     (co-event ((cat clause)
		(mood present-participle)
		(binder ((lex "with")))
		(process ((type material) (lex "hamper")))
		(partic ((agent ((cat common)
				 (possessor ((cat personal-pronoun)
					     (index {^6 partic agent index})))
				 (number plural)
				 (lex "knee")))
			 (affected ((cat personal-pronoun)
				    (index {^5 partic agent index})))))))))))


;; Tests: duration as verbless clause
;;        habitual co-event as finite clause with subject
(def-test c13
  "Whenever pain resurfaces, rest as long as necessary."
  ((cat clause)
   (mood imperative)
   (process ((type material) (effective no) (lex "rest")))
   (circum ((duration ((cat clause)
		       (mood verbless)
		       (binder ((lex "as long as")))
		       (process ((type ascriptive)))
		       (controlled {^ partic carrier})
		       (partic ((attribute ((cat ap) (lex "necessary")))))))
	    (co-event ((cat clause)
		       (mood bound-adverbial)
		       (habitual yes)
		       (process ((type material) (effective no)
                                 (lex "resurface")))
		       (partic ((agent ((cat common)
					(countable no)
					(lex "pain")))))))))))


;; Tests: co-event as subjectless present-participle clause
;;        cat date
;;        cat address
;;        header adverbial
;;        inclusion as present-participle clause
(def-test c14
  "Salt Lake City, Utah -- Karl Malone scored 28 points Saturday, including making 10 of 12 field goals, leading the Utah Jazz to a 105 - 95 win over the Los Angeles Clippers."
  ((cat clause)
   (tense past)
   (process ((type material) (effect-type creative) (lex "score")))
   (partic ((agent ((cat compound-proper)
		    (head ((cat person-name)
			   (first-name ((lex "Karl")))
			   (last-name ((lex "Malone")))))))
	    (created ((cat measure)
		      (quantity ((value 28)))
		      (unit ((lex "point")))))))
   (circum
    ((time ((cat date) (day-name ((lex "Saturday")))))
     (location ((cat address)
		(position header)
		(punctuation ((after "--")))
		(city ((lex "Salt Lake City")))
		(state ((lex "Utah")))))
     (inclusion ((cat clause)
		 (mood present-participle)
		 (position end)
		 (controlled {^ partic agent})
		 (process ((type material) (effect-type creative) (lex "make")))
		 (partic
		  ((agent ((index {^5 partic agent index})))
		   (created ((cat partitive)
			     (part ((value 10) (digit yes)))
			     (part-of ((cat measure)
				       (quantity ((value 12)))
				       (unit ((lex "field goal")))))))))))
     (co-event
      ((cat clause)
       (mood present-participle)
       (position end)
       (controlled {^ partic agent})
       (process ((type composite)
		 (relation-type locative)
		 (lex "lead")))
       (partic
	((agent ((index {^5 partic agent index})))
	 (located ((cat compound-proper)
		   (number plural)
		   (head ((cat team-name)
			  (home ((lex "Utah")))
			  (franchise ((lex "Jazz")))))))
	 (affected {^ located})
	 (location
	  ((cat pp)
	   (prep ((lex "to")))
	   (np ((cat common)
		(definite no)
		(classifier ((cat score)
			     (win ((value 105)))
			     (lose ((value 95)))))
		(head ((lex "win")))
		(qualifier
		 ((cat pp)
		  (prep ((lex "over")))
		  (np ((cat compound-proper)
		       (number plural)
		       (head ((cat team-name)
			      (home ((lex "Los Angeles")))
			      (franchise ((lex "Clipper")))))))))))))))))))))

(store-plurals '(("Jazz" "Jazz")))


;; Tests: co-event as subjectless past-participle clause
;;        opposition role
(def-test c15
  "Injured against the Giants, Bo missed 11 games."
  ((cat clause)
   (tense past)
   (process ((type material) (effective no) (lex "miss")))
   (partic ((agent ((cat basic-proper) (lex "Bo")))
	    (range ((cat measure) (quantity ((value 11)))
                    (unit ((lex "game")))))))
   (circum
    ((co-event
      ((cat clause)
       (mood past-participle)
       (controlled {^ partic affected})
       (binder none)
       (position front)
       (process ((type material) (lex "injure")))
       (partic ((affected ((index {^5 partic agent index})))))
       (circum ((opposition
                 ((cat pp)
                  (np ((cat compound-proper)
                       (number plural)
                       (head ((cat team-name)
                              (franchise ((lex "Giant")))))))))))))))))


;; Tests: co-event as verbless clause
;;        to-infinitive clause as adjective qualifier
;;        reason as adjunct finite clause
(def-test c16
  "Unable to play because he was injured, Bo stayed home."
  ((cat clause)
   (tense past)
   (process ((type composite) (relation-type locative) (lex "stay")))
   (partic ((agent ((cat basic-proper) (lex "Bo") (gender masculine)))
	    (located {^ agent})
	    (location ((cat adv) (lex "home")))))
   (circum
    ((co-event
      ((cat clause)
       (position front)
       (mood verbless)
       (binder none)
       (controlled {^ partic carrier})
       (process ((type ascriptive)))
       (partic
	((carrier ((index {^5 partic agent index})))
	 (attribute
	  ((cat ap)
	   (head ((lex "unable")))
	   (qualifier ((cat clause)
		       (mood to-infinitive)
		       (controlled {^ partic agent})
		       (process ((type material) (effective no) (lex "play")))
		       (partic ((agent ((index {^5 carrier index})))))))))))
       (circum ((reason ((cat clause)
			 (mood bound-adverbial)
			 (tense past)
			 (process ((type ascriptive)))
			 (partic ((carrier ((cat personal-pronoun)
					    (index {^7 partic located index})))
				  (attribute ((cat verb)
					      (ending past-participle)
					      (lex "injure")))))))))))))))


;; Tests: reason as disjunct finite clause
;;        embedded reason
(def-test c17
  "Since he was unable to play because of injury, Bo stayed home."
  ((cat clause)
   (tense past)
   (process ((type composite) (relation-type locative) (lex "stay")))
   (partic ((agent ((cat basic-proper) (lex "Bo") (gender masculine)))
	    (located {^ agent})
	    (location ((cat adv) (lex "home")))))
   (circum
    ((reason
      ((cat clause)
       (position front)
       (mood bound-adverbial)
       (tense past)
       (binder ((lex "since")))
       (process ((type ascriptive)))
       (partic
	((carrier ((cat personal-pronoun)
		   (index {^5 partic agent index})))
	 (attribute
	  ((cat ap)
	   (head ((lex "unable")))
	   (qualifier ((cat clause)
		       (mood to-infinitive)
		       (controlled {^ partic agent})
		       (process ((type material) (effective no) (lex "play")))
		       (partic ((agent ((index {^5 carrier index})))))
		       (circum ((reason ((cat pp)
					 (np ((cat common)
					      (denotation illness)
					      (lex "injury")))))))))))))))))))


;; Tests: result as finite clause
(def-test c18
  "Bo was unable to play because of injury, so he stayed home."
  ((cat clause)
   (tense past)
   (process ((type ascriptive)))
   (partic
    ((carrier ((cat basic-proper) (lex "Bo") (gender masculine)))
     (attribute
      ((cat ap)
       (head ((lex "unable")))
       (qualifier ((cat clause)
                   (mood to-infinitive)
                   (process ((type material) (effective no) (lex "play")))
                   (partic ((agent ((index {^5 carrier index})))))
                   (circum ((reason ((cat pp)
                                     (np ((cat common)
                                          (denotation illness)
                                          (lex "injury")))))))))))))
   (circum
    ((result
      ((cat clause)
       (mood bound-adverbial)
       (binder ((lex "so")))
       (tense past)
       (process ((type composite) (relation-type locative) (lex "stay")))
       (partic ((agent ((cat personal-pronoun)
                        (index {^5 partic carrier index})))
                (located {^ agent})
                (location ((cat adv) (lex "home")))))))))))


;; Tests: result as to-infinitive clause
;;        score as circumstance
(def-test c19
  "The Utah Jazz defeated the Los Angeles Clippers 105 - 95, to extend their winning streak to 11 games."
  ((cat clause)
   (tense past)
   (process ((type material) (lex "defeat")))
   (partic ((agent ((cat compound-proper)
		    (number plural)
		    (head ((cat team-name)
			   (home ((lex "Utah")))
			   (franchise ((lex "Jazz")))))))
	    (affected ((cat compound-proper)
		       (number plural)
		       (head ((cat team-name)
			      (home ((lex "Los Angeles")))
			      (franchise ((lex "Clipper")))))))))
   (pred-modif ((score ((cat score) (win ((value 105))) (lose ((value 95)))))))
   (circum ((result ((cat clause)
		     (mood to-infinitive)
		     (controlled {^ partic agent})
		     (process ((type composite)
			       (relation-type locative)
			       (lex "extend")))
		     (partic ((agent ((index {^5 partic agent index})))
			      (affected {^ located})
			      (located ((cat common)
					(possessor ((cat personal-pronoun)
						    (index {^3 agent index})))
					(describer ((cat verb)
						    (ending present-participle)
						    (lex "win")))
					(head ((lex "streak")))))
			      (location ((cat pp)
					 (prep ((lex "to")))
					 (np ((cat measure)
					      (quantity ((value 11)))
					      (unit ((lex "game")))))))))))))))


;; Tests: purpose as PP
(def-test c20
  "The Utah Jazz defeated the Los Angeles Clippers 105 - 95 for its 11th straight victory."
  ((cat clause)
   (tense past)
   (process ((type material) (lex "defeat")))
   (partic ((agent ((cat compound-proper)
		    (number plural)
		    (head ((cat team-name)
			   (home ((lex "Utah")))
			   (franchise ((lex "Jazz")))))))
	    (affected ((cat compound-proper)
		       (number plural)
		       (head ((cat team-name)
			      (home ((lex "Los Angeles")))
			      (franchise ((lex "Clipper")))))))))
   (pred-modif ((score ((cat score) (win ((value 105))) (lose ((value 95)))))))
   (circum ((purpose ((cat pp)
		      (position end)
		      (np ((cat common)
			   (possessor ((cat personal-pronoun)
				       (index {^4 partic agent index})))
			   (ordinal ((value 11)))
			   (classifier ((cat adj) (lex "straight")))
			   (head ((lex "victory")))))))))))


;; Tests: purpose as finite clause
;;        exception role
(def-test c21
  "Except Bo, all players do commercials so they can make extra money."
  ((cat clause)
   (process ((type material) (effective no) (lex "do")))
   (partic ((agent ((cat common)
		    (number plural)
		    (definite no)
		    (total +)
		    (lex "player")))
	    (range ((cat common)
		    (number plural)
		    (definite no)
		    (lex "commercial")))))
   (circum
    ((exception ((cat pp) (np ((cat basic-proper) (lex "Bo")))))
     (purpose ((cat clause)
               (mood bound-adverbial)
               (binder ((lex "so")))
               (epistemic-modality possible)
               (process ((type material) (effect-type creative) (lex "make")))
               (partic ((agent ((cat personal-pronoun)
                                (index {^5 partic agent index})))
                        (created ((cat common)
                                  (countable no)
                                  (definite no)
                                  (classifier ((cat adj) (lex "extra")))
                                  (head ((lex "money")))))))))))))


;; Tests: purpose as to-infinitive clause
;;        substitution as bare-infinitive clause
(def-test c22
  "Rather than admit his problem, he lies to conceal it."
  ((cat clause)
   (process ((type verbal) (lex "lie")))
   (partic ((sayer ((cat personal-pronoun) (gender masculine)))))
   (circum
    ((substitution
      ((cat clause)
       (mood bare-infinitive)
       (controlled {^ partic sayer})
       (process ((type verbal) (lex "admit")))
       (partic ((sayer ((index {^5 partic sayer index})))
		(verbalization ((cat common)
				(possessor ((cat personal-pronoun)
					    (index {^3 sayer index})))
				(head ((lex "problem")))))))))
     (purpose
      ((cat clause)
       (mood to-infinitive)
       (binder none)
       (controlled {^ partic sayer})
       (process ((type verbal) (lex "conceal")))
       (partic
	((sayer ((index {^5 partic sayer index})))
	 (verbalization
          ((cat personal-pronoun)
           (index {^4 substitution partic verbalization index})))))))))))


;; Tests: behalf
;;        condition as finite clause
(def-test c23
  "If he is 100 percent fit, Bo will play for the Raiders on Sunday."
  ((cat clause)
   (tense future)
   (process ((type material) (effective no) (lex "play")))
   (partic ((agent ((cat basic-proper) (lex "Bo") (gender masculine)))))
   (circum ((behalf ((cat pp)
		     (np ((cat compound-proper)
			  (number plural)
			  (head ((cat team-name)
				 (franchise ((lex "Raider")))))))))
	    (time ((cat pp)
		   (position end)
		   (prep ((lex "on")))
		   (np ((cat basic-proper) (lex "Sunday")))))
	    (condition
             ((cat clause)
              (position front)
              (mood bound-adverbial)
              (process ((type ascriptive)))
              (partic ((carrier ((cat personal-pronoun)
                                 (index {^5 partic agent index})))
                       (attribute ((cat ap)
                                   (classifier ((cat measure)
                                                (quantity ((value 100)))
                                                (unit ((lex "percent")))))
                                   (head ((lex "fit")))))))))))))



;; Tests: condition as verbless clause
;;        substitution as PP
;;        concession as finite clause
(def-test c24
  "Even though he was not a starter at the beginning of the season, if fully fit, Bo will start tomorrow, instead of Smith."
  ((cat clause)
   (tense future)
   (process ((type material) (agentive no) (lex "start")))
   (partic ((affected ((cat basic-proper) (lex "Bo") (gender masculine)))))
   (circum
    ((concession
      ((cat clause)
       (tense past)
       (binder ((lex "even though")))
       (polarity negative)
       (process ((type ascriptive)))
       (partic ((carrier ((cat personal-pronoun)
                          (index {^5 partic affected index})))
                (attribute ((cat common)
                            (definite no)
                            (lex "starter")))))
       (circum ((time ((cat pp)
                       (prep ((lex "at")))
                       (np ((cat common)
                            (head ((lex "beginning")))
                            (qualifier ((cat pp)
                                        (prep ((lex "of")))
                                        (np ((cat common)
                                             (lex "season")))))))))))))
     (condition ((cat clause)
		 (position front)
		 (mood verbless)
		 (process ((type ascriptive)))
		 (controlled {^ partic carrier})
		 (partic ((carrier ((index {^5 partic affected index})))
			  (attribute ((cat ap)
				      (modifier ((cat adv) (lex "fully")))
				      (head ((lex "fit")))))))))
     (substitution ((cat pp) (np ((cat basic-proper) (lex "Smith")))))
     (time ((cat adv) (lex "tomorrow")))))))


;; Tests: condition as past-participle clause
;;        concession as PP
(def-test c25
  "If defeated tonight, the Raiders will not make the playoffs, despite their better division record."
  ((cat clause)
   (tense future)
   (polarity negative)
   (process ((type material) (effective no) (lex "make")))
   (partic ((agent ((cat compound-proper)
		    (number plural)
		    (head ((cat team-name)
			   (franchise ((lex "Raider")))))))
	    (range ((cat common) (number plural) (lex "playoff")))))
   (circum ((condition ((cat clause)
			(mood past-participle)
			(controlled {^ partic affected})
			(process ((type material) (lex "defeat")))
			(partic ((affected ((index {^5 partic agent index})))))
			(circum ((time ((cat adv) (lex "tonight")))))))
	    (concession ((cat pp)
			 (prep ((lex "despite")))
			 (np ((cat common)
			      (possessor ((cat personal-pronoun)
					  (index {^5 partic agent index})))
			      (describer ((lex "better")))
			      (classifier ((lex "division")))
			      (head ((lex "record")))))))))))


;; Tests: time as verbless clause
;;        matter role
;;        manner as adverb
;;        distance as measure
;;        destination as finite clause
;;        substitution as present-participle clause

;; @@Todo:
;; Expected:
;; "Instead of panicking,                     substitution
;;  when in doubt concerning his position,    time
;;  Bo cautiously retreated five miles        main(manner)
;;  towards where he came from."              destination
;; Instead:
;; "Cautiously,                               manner
;;  Bo retreated five miles                   main
;;  towards where he came from                destination
;;  when in doubt, concerning his position,   time
;;  rather than panicking."                   substitution
(def-test c26
  ("Instead of panicking, when in doubt concerning his position, Bo cautiously retreated five miles towards where he came from."
   "Cautiously, Bo retreated five miles towards where he came from when in doubt, concerning his position, rather than panicking.")
  ((cat clause)
   (tense past)
   (process ((type material) (effective no) (lex "retreat")))
   (partic ((agent ((cat basic-proper) (lex "Bo") (gender masculine)))))
   (pred-modif
    ((distance ((cat measure)
                (quantity ((value 5)))
                (unit ((lex "mile")))))
     (destination ((cat clause)
                   (mood bound-adverbial)
                   (tense past)
                   (binder ((lex "towards where")))
                   (process ((type material)
                             (effective no)
                             (lex "come from")))
                   (partic ((agent ((cat personal-pronoun)
                                    (index {^5 partic agent index})))))))))
;; This non-composite analysis of "he came from" sounds weird, so it might
;; in this case be a relative clause with gap. The problem is to find a
;; criteria to distinguish it with the usages of where in locative adjuncts.

   (circum
    ((time ((cat clause)
	    (mood verbless)
	    (controlled {^ partic carrier})
	    (process ((type ascriptive)))
	    (partic ((carrier ((index {^5 partic agent index})))
		     (attribute ((cat pp)
				 (prep ((lex "in")))
				 (np ((cat common)
				      (denotation zero-article-thing)
				      (lex "doubt")))))))
	    (circum
             ((matter ((cat pp)
                       (prep ((lex "concerning")))
                       (np ((cat common)
                            (possessor ((cat personal-pronoun)
                                        (index {^5 partic carrier index})))
                            (head ((lex "position")))))))))))
     (manner ((cat adv) (lex "cautiously")))
     (substitution
      ((cat clause)
       (mood present-participle)
       (controlled {^ partic processor})
       (process ((type mental) (transitive no) (lex "panic")))
       (partic ((processor ((index {^5 partic agent index})))))))))))

(store-verbs '(("come from" "comes from" "came from" "coming from" "came from")))
(store-verbs '(("panic" "panics" "panicked" "panicking" "panicked")))


;; Tests: duration as past-participle clause
;;        habitual co-event as verbless clause
;;        manner as PP
;;        means as PP
(def-test c27
  "Whenever in doubt, resist with obstination until coerced by force."
  ((cat clause)
   (mood imperative)
   (process ((type material) (effective no) (lex "resist")))
   (partic ((agent ((cat personal-pronoun) (person second)))))
   (pred-modif
    ((manner ((cat pp)
              (np ((cat common) (lex "obstination") (countable no)))))))
   (circum
    ((duration ((cat clause)
		(position end)
		(mood past-participle)
		(binder ((lex "until")))
		(process ((type material) (lex "coerce")))
		(controlled {^ partic affected})
		(partic ((affected ((index {^5 partic agent index})))))
		(pred-modif ((means ((cat pp)
				     (np ((cat common) (lex "force")))))))))
                ;; Note how filling means function ==> zero determiner

     (co-event ((cat clause)
		(position front)
		(mood verbless)
		(habitual yes)
		(process ((type ascriptive)))
		(controlled {^ partic carrier})
		(partic ((carrier ((index {^5 partic agent index})))
			 (attribute ((cat pp)
				     (prep ((lex "in")))
				     (np ((cat common)
					  (denotation zero-article-thing)
					  (lex "doubt")))))))))))))


;; Tests: negative comparison as PP
;;        concessive-condition as finite clause
(def-test c28
  "Unlike Barkley, Jordan can dominate, even if he is not 100 percent fit."
  ((cat clause)
   (epistemic-modality possible)
   (process ((type material) (effective no) (lex "dominate")))
   (partic ((agent ((cat basic-proper) (lex "Jordan") (gender masculine)))))
   (circum ((comparison ((cat pp)
			 (position front)
			 (comp-polarity -)
			 (np ((cat basic-proper)
			      (lex "Barkley")))))
	    (concessive-condition
	     ((cat clause)
	      (mood bound-adverbial)
	      (polarity negative)
	      (process ((type ascriptive)))
	      (partic ((carrier ((cat personal-pronoun)
				 (index {^5 partic agent index})))
		       (attribute ((cat ap)
				   (classifier ((cat measure)
						(quantity ((value 100)))
						(unit ((lex "percent")))))
				   (head ((lex "fit")))))))))))))


;; Tests: concessive-condition as present-participle clause
;;        positive instrument
;;        contrast as PP
(def-test c29
  "As opposed to pure penetrators, Jordan can kill you with his three point shot, even if suffering from tendinitis."
  ((cat clause)
   (epistemic-modality possible)
   (process ((type material) (lex "kill")))
   (partic ((agent ((cat basic-proper) (lex "Jordan") (gender masculine)))
	    (affected ((cat personal-pronoun) (person second)))))
   (pred-modif ((instrument ((cat pp)
			     (np ((cat common)
				  (possessor ((cat personal-pronoun)
					      (index {^5 partic agent index})))
				  (classifier ((cat measure)
					       (quantity ((value 3)))
					       (unit ((lex "point")))))
				  (head ((lex "shot")))))))))
   (circum ((contrast ((cat pp)
		       (prep ((lex "as opposed to")))
		       (np ((cat common)
			    (number plural)
			    (definite no)
			    (describer ((lex "pure")))
			    (head ((lex "penetrator")))))))
	    (concessive-condition
	     ((cat clause)
	      (mood present-participle)
	      (controlled {^ partic processor})
	      (process ((type mental) (lex "suffer from")))
	      (partic ((processor ((index {^5 partic agent index})))
		       (phenomenon ((cat common)
				    (denotation illness)
				    (lex "tendinitis")))))))))))

(store-verbs '(("suffer from" "suffers from" "suffered from"
                "suffering from" "suffered from")))



;; Tests: concessive-condition as verbless clause
;;        contrast as finite clause
;;        present-participle clause as adjective qualifier
(def-test c30
  "Even if capable of playing forward, Magic was a guard, whereas Bird was a true forward."
  ((cat clause)
   (tense past)
   (process ((type ascriptive)))
   (partic ((carrier ((cat basic-proper) (lex "Magic")))
	    (attribute ((cat common) (definite no) (lex "guard")))))
   (circum ((contrast ((cat clause)
		       (tense past)
		       (mood bound-adverbial)
		       (process ((type ascriptive)))
		       (partic ((carrier ((cat basic-proper) (lex "Bird")))
				(attribute ((cat common)
					    (definite no)
					    (classifier ((lex "true")))
					    (head ((lex "forward")))))))))
	    (concessive-condition
	     ((cat clause)
	      (mood verbless)
	      (process ((type ascriptive)))
	      (controlled {^ partic carrier})
	      (partic
	       ((carrier ((index {^5 partic carrier index})))
		(attribute
		 ((cat ap)
		  (head ((lex "capable")))
		  (qualifier
		   ((cat clause)
		    (mood present-participle)
		    (binder ((lex "of")))
		    (controlled {^ partic agent})
		    (process ((type material) (effective no) (lex "play")))
		    (partic ((agent ((index {^5 partic carrier index})))
			     (range ((cat common)
				     (denotation zero-article-thing)
				     (lex "forward")))))))))))))))))


;; Tests: concessive-condition as past-participle clause
;;        positive comparison as PP
;;        past-participle clause with agent
(def-test c31
  "Like Jordan, Drexler can kill you from outside, even if slowed down by injury."
  ((cat clause)
   (epistemic-modality possible)
   (process ((type material) (lex "kill")))
   (partic ((agent ((cat basic-proper) (lex "Drexler")))
	    (affected ((cat personal-pronoun) (person second)))))
   (circum
    ((comparison ((cat pp) (np ((cat basic-proper) (lex "Jordan")))))
     (concessive-condition
      ((cat clause)
       (mood past-participle)
       (controlled {^ partic affected})
       (process ((type material)
                 (lex "slow down")
                 (agentless no)))
       (partic ((agent ((cat common) (denotation illness) (lex "injury")))
                (affected ((index {^5 partic agent index})))))))
     (location ((cat pp)
                (prep ((lex "from")))
                (np ((cat common)
                     (denotation zero-article-thing)
                     (lex "outside")))))))))
;; outside as a noun seems weird but [prep adv] definitely can't be a
;; proper constituent, right Master Capelovici?

(store-verbs
 '(("slow down" "slows down" "slowed down" "slowing down" "slowed down")))


;; Tests: concession as past-participle clause
;;        positive accompaniment
(def-test c32
  "Although not wanted by the Knicks, Kimble was traded with Smith for Jackson."
  ((cat clause)
   (tense past)
   (process ((type material) (lex "trade") (voice passive)))
   (partic ((agent ((cat compound-proper)
		    (number plural)
		    (head ((cat team-name) (franchise ((lex "Knick")))))))
	    (affected ((cat basic-proper) (lex "Kimble")))))
   (circum ((accompaniment ((cat pp)
			    (np ((cat basic-proper) (lex "Smith")))))
	    (concession
	     ((cat clause)
	      (position front)
	      (mood past-participle)
	      (polarity negative)
	      (process ((type mental) (lex "want") (agentless no)))
	      (controlled {^ partic phenomenon})
	      (partic ((processor {^4 partic agent})
		       (phenomenon ((index {^5 partic affected index})))))))
	    (purpose ((cat pp)
		      (position end)
		      (np ((cat basic-proper) (lex "Jackson")))))))))


;; Tests: concession as present-participle clause
;;        negative accompaniment
;;        comparison as finite clause
(def-test c33
  "Although playing without Bo, the Raiders won as they did with him."
  ((cat clause)
   (tense past)
   (process ((type material) (effective no) (lex "win")))
   (partic ((agent ((cat compound-proper)
		    (number plural)
		    (head ((cat team-name)
			   (franchise ((lex "Raider")))))))))
   (pred-modif
    ((comparison
      ((cat clause)
       (tense past)
       (mood bound-adverbial)
       (binder ((lex "as")))
       (process ((type material) (effective no) (lex "do")))

       ;; Should really be:
       ;; (cat pro-verb)
       ;; (index {^4 process index})
       ;; Such pro-form inference for verb would be quite nice wouldn't it?
       (partic ((agent ((cat personal-pronoun)
			(index {^5 partic agent index})))))
       (circum
	((accompaniment
	  ((cat pp)
	   (np ((cat personal-pronoun)
		(index
                 {^6 circum concession circum accompaniment np index})))))))))))
   (circum ((concession
             ((cat clause)
              (mood present-participle)
              (controlled {^ partic agent})
              (process ((type material) (effective no) (lex "play")))
              (partic ((agent ((index {^5 partic agent index})))))
              (circum ((accompaniment ((cat pp)
                                       (accomp-polarity -)
                                       (np ((cat basic-proper)
                                            (gender masculine)
                                            (lex "Bo")))))))))))))

(store-verbs '(("win" "wins" "won" "winning" "won")))

;; Tests: concession as verbless clause
;;        addition as PP
(def-test c34
  "Although in need of a center, they drafted a guard in addition to Flint."
  ((cat clause)
   (tense past)
   (process ((type composite) (relation-type possessive) (lex "draft")))
   (partic ((agent ((cat personal-pronoun) (number plural)))
	    (possessor {^ agent})
	    (possessed ((cat common) (definite no) (lex "guard")))))
   (circum ((addition ((cat pp)
		       (prep ((lex "in addition to")))
		       (np ((cat basic-proper) (lex "Flint")))))
	    (concession ((cat clause)
			 (mood verbless)
			 (process ((type ascriptive)))
			 (controlled {^ partic carrier})
			 (partic ((carrier ((index {^5 partic agent index})))
				  (attribute ((cat pp)
					      (prep ((lex "in need of")))
					      (np ((cat common)
						   (definite no)
						   (lex "center")))))))))))))


;; Tests: negative instrument
;;        means as present-participle clause and PP
;;        standard and perspective roles
;;        addition as present-participle clause
(def-test c35a
  "All their holes."
  ((cat common)
   (number plural)
   (total +)
   (possessor ((cat personal-pronoun) (number plural)))
   (head ((lex "hole")))))

(def-test c35b
  "All this work."
  ((cat common)
   (total +)
   (countable no)
   (distance near)
   (head ((lex "work")))))

(def-test c35
    "They filled all their holes without a draft pick by acquiring Bowman, whose passing game is exceptional, for a seven footer, through free agency in addition to trading for Ashbliss, who remains incomparable, as a defensive stopper."
  ((cat clause)
   (tense past)
   (process ((type material) (lex "fill")))
   (partic ((agent ((cat personal-pronoun) (number plural)))
	    (affected ((cat common)
		       (number plural)
		       (total +)
		       (possessor ((cat personal-pronoun)
				   (index {^3 agent index})))
		       (head ((lex "hole")))))))
   (pred-modif ((instrument ((cat pp)
			     (instr-polarity -)
			     (np ((cat common)
				  (definite no)
				  (classifier ((lex "draft")))
				  (head ((lex "pick")))))))))
   (circum
    ((means
      ((cat clause)
       (position end)
       (mood present-participle)
       (process ((type composite) (relation-type possessive) (lex "acquire")))
       (controlled {^ partic agent})
       (partic
	((agent ((index {^5 partic agent index})))
	 (possessor {^ agent})
	 (possessed
          ((cat compound-proper)
           (head ((cat person-name)
                  (last-name ((lex "Bowman")))))
           (qualifier
            ((cat clause)
             (mood possessive-relative)
             (restrictive no)
             (process ((type ascriptive)))
             (scope {^ partic carrier})
             (partic ((carrier ((cat common)
                                (classifier ((cat verb)
                                             (ending present-participle)
                                             (lex "pass")))
                                (head ((lex "game")))))
                      (attribute ((cat ap) (lex "exceptional")))))
             (circum ((standard ((cat pp)
                                 (np ((cat common)
                                      (definite no)
                                      (lex "seven footer")))))))))))))
       (pred-modif ((means ((cat pp)
			    (prep ((lex "through")))
			    (np ((cat common) (lex "free agency")))))))
       ;; Note how filling means function ==> zero determiner

       (circum
	((addition
	  ((cat clause)
	   (position end)
	   (mood present-participle)
	   (controlled {^ partic agent})
	   (process
            ((type composite) (relation-type possessive) (lex "trade for")))
	   (partic
	    ((agent ((index {^5 partic agent index})))
	     (possessor {^ agent})
	     (possessed
	      ((cat compound-proper)
	       (head ((cat person-name)
		      (last-name ((lex "Ashbliss")))))
	       (qualifier
		((cat clause)
		 (mood relative)
		 (process ((type ascriptive) (lex "remain")))
		 (scope {^ partic carrier})
		 (restrictive no)
		 (partic ((attribute ((cat ap) (lex "incomparable")))))
		 (circum
		  ((perspective
		    ((cat pp)
		     (np ((cat common)
			  (definite no)
			  (classifier ((lex "defensive")))
			  (head ((lex "stopper")))))))))))))))))))))))))

(store-verbs
 '(("trade for" "trades for" "traded for" "trading for" "traded for")))

;; Tests: comparison as verbless clause
;;        means as adverb
(def-test c36
  "They treated him homeopatically as if afraid of conventional treatments."
  ((cat clause)
   (tense past)
   (process ((type material) (lex "treat")))
   (partic ((agent ((cat personal-pronoun) (number plural)))
	    (affected ((cat personal-pronoun) (gender masculine)))))
   (pred-modif
    ((means ((cat adv) (lex "homeopatically")))
     (comparison
      ((cat clause)
       (mood verbless)
       (process ((type ascriptive)))
       (controlled {^ partic carrier})
       (partic
        ((carrier ((index {^5 partic agent index})))
         (attribute ((cat ap)
                     (head ((lex "afraid")))
                     (qualifier ((cat pp)
                                 (prep ((lex "of")))
                                 (np ((cat common)
                                      (number plural)
                                      (definite no)
                                      (describer ((lex "conventional")))
                                      (head ((lex "treatment")))))))))))))))))


;; Tests: comparison as past-participle clause
(def-test c37
  "They treated him homeopatically as if frightened by conventional treatments."
  ((cat clause)
   (tense past)
   (process ((type material) (lex "treat")))
   (partic ((agent ((cat personal-pronoun) (number plural)))
	    (affected ((cat personal-pronoun) (gender masculine)))))
   (pred-modif
    ((means ((cat adv) (lex "homeopatically")))
     (comparison
      ((cat clause)
       (mood past-participle)
       (process ((type mental) (lex "frighten") (agentless no)))
       (controlled {^ partic phenomenon})
       (partic ((phenomenon ((index {^5 partic agent index})))
		(processor ((cat common)
			    (number plural)
			    (definite no)
			    (describer ((lex "conventional")))
			    (head ((lex "treatment")))))))))))))

(store-verbs
 '(("frighten" "frightens" "frightened" "frightening" "frightened")))


;; Tests: comparison as present-participle clause
(def-test c38
  "They treated him homeopatically as if distrusting conventional treatments."
  ((cat clause)
   (tense past)
   (process ((type material) (lex "treat")))
   (partic ((agent ((cat personal-pronoun) (number plural)))
	    (affected ((cat personal-pronoun) (gender masculine)))))
   (pred-modif
    ((means ((cat adv) (lex "homeopatically")))
     (comparison
      ((cat clause)
       (mood present-participle)
       (process ((type mental) (lex "distrust")))
       (controlled {^ partic processor})
       (partic ((processor ((index {^5 partic agent index})))
		(phenomenon ((cat common)
			     (number plural)
			     (definite no)
			     (describer ((lex "conventional")))
			     (head ((lex "treatment")))))))))))))


;; Tests: comparison as to-infinitive clause
;;        past-participle clause with subject
(def-test c39
  "The analysis completed, they treated him homeopatically as if to avoid conventional treatments."
  ((cat clause)
   (tense past)
   (process ((type material) (lex "treat")))
   (partic ((agent ((cat personal-pronoun) (number plural)))
	    (affected ((cat personal-pronoun) (gender masculine)))))
   (pred-modif
    ((means ((cat adv) (lex "homeopatically")))
     (comparison
      ((cat clause)
       (mood to-infinitive)
       (process ((type material) (effective no) (lex "avoid")))
       (controlled {^ partic agent})
       (partic ((agent ((index {^5 partic agent index})))
		(range ((cat common)
			(number plural)
			(definite no)
			(describer ((lex "conventional")))
			(head ((lex "treatment")))))))))))
   (circum
    ((co-event ((cat clause)
                (mood past-participle)
                (process ((type material) (lex "complete")))
                (partic ((affected ((cat common) (lex "analysis")))))))))))


;; Alternative analysis, tests: verbless clause with subject
(def-test c39bis
  "The analysis completed, they treated him homeopatically as if to avoid conventional treatments."
  ((cat clause)
   (tense past)
   (process ((type material) (lex "treat")))
   (partic ((agent ((cat personal-pronoun) (number plural)))
	    (affected ((cat personal-pronoun) (gender masculine)))))
   (pred-modif
    ((means ((cat adv) (lex "homeopatically")))
     (comparison
      ((cat clause)
       (mood to-infinitive)
       (process ((type material) (effective no) (lex "avoid")))
       (partic ((agent ((index {^5 partic agent index})))
		(range ((cat common)
			(number plural)
			(definite no)
			(describer ((lex "conventional")))
			(head ((lex "treatment")))))))))))
   (circum ((co-event ((cat clause)
		       (mood verbless)
		       (process ((type ascriptive)))
		       (partic ((carrier ((cat common) (lex "analysis")))
				(attribute ((cat verb)
					    (ending past-participle)
					    (lex "complete")))))))))))


;; Tests: present-participle clause as noun qualifier
;;        past-participle clause as noun qualifier
;;        to-infinitive clause as noun qualifier
(def-test c40
  "The first team winning two games will be the team to take the title recognized by the WSL."
((cat clause)
 (tense future)
 (process ((type ascriptive) (relation-mode equative)))
 (partic
  ((identified ((cat common)
		(ordinal ((value 1)))
		(head ((lex "team")))
		(qualifier
                 ((cat clause)
                  (mood present-participle)
                  (process ((type material) (effective no) (lex "win")))
                  (controlled {^ partic agent})
                  (partic ((agent ((index {^5 identified index})))
                           (range ((cat measure)
                                   (quantity ((value 2)))
                                   (unit ((lex "game")))))))))))
   (identifier
    ((cat common)
     (head ((lex "team")))
     (qualifier
      ((cat clause)
       (mood to-infinitive)
       (process ((type material) (effective no) (lex "take")))
       (controlled {^ partic agent})
       (partic
	((agent ((index {^5 identifier index})))
	 (range ((cat common)
		 (head ((lex "title")))
		 (qualifier
		  ((cat clause)
		   (mood past-participle)
		   (process ((type mental) (lex "recognize") (agentless no)))
		   (controlled {^ partic phenomenon})
		   (partic
                    ((processor ((cat basic-proper) (lex "the WSL")))
                     (phenomenon ((index {^5 range index})))))))))))))))))))


;; Tests: for-to-infinitive clause with subject as adverbial
(def-test c41
  "I did it for you to graduate."
  ((cat clause)
   (tense past)
   (process ((type material) (effect-type creative) (lex "do")))
   (partic ((agent ((cat personal-pronoun) (person first)))
	    (created ((cat personal-pronoun)))))
   (circum
    ((purpose ((cat clause)
	       (mood for-to-infinitive)
	       (binder none)
	       (process ((type mental) (transitive no) (lex "graduate")))
	       (partic ((processor ((cat personal-pronoun)
                                    (person second)))))))))))

;; Tests: backward compatibility of condition w/ cond-relater
(def-test c42
  "If the compartment is dirty, then clean it."
  ((cat clause)
   (mood imperative)
   (proc ((type material) (lex "clean")))
   (partic ((affected ((cat personal-pronoun)))))
   (relaters ((cond ((lex "then")))))
   (circum ((condition ((cat clause)
			(position front)
			(then yes)
			(proc ((type ascriptive)))
			(partic ((carrier ((cat common) (lex "compartment")))
				 (attribute ((cat ap) (lex "dirty")))))))))))


(def-test c43
  "It comes from the KY57."
  ((cat clause)
   (proc ((type material) (lex "come")))
   (partic ((agent ((cat personal-pronoun)))))
   (circum ((from-loc ((cat common) (lex "KY57")))))))

(def-test c44
  "It comes from the KY57."
  ((cat clause)
   (proc ((type material) (lex "come")))
   (partic ((agent ((cat personal-pronoun)))))
   (pred-modif ((origin ((cat pp)
			 (np ((cat common) (lex "KY57")))))))))




;; because of injury, from tendinitis  illness
;; with obstination                    mass
;; in doubt, playing forward           generic ref
;; by force, through free agency       means filler
;; in need of                          complex prep

;; need, doubt: abstract count
;; obstination: abstract mass
;; injury, tendinitis: illness Quirk p.279
;; forward: unique role Quirk p.276


(def-test charles1
  "The hand of the man pets the dog on the nose."
  ((cat clause)
   (proc ((type composite)
	  (lex "pet")))
   (partic ((agent ((cat partitive)
		    (part ((cat common)
			   (lex "hand")))
		    (part-of ((cat common)
			      (lex "man")))))
	    (affected ((cat common)
		       (lex "dog")))
	    (location ((cat pp)
		       (prep === "on")
		       (np ((cat common)
			    (lex "nose")))))))))
(def-test charles2
  "The hand of the man pets the dog on the nose."
  ((cat clause)
   (proc ((type composite) (relation-type locative) (lex "pet")))
   (partic ((agent ((cat partitive)
		    (part ((cat common)
			   (lex "hand")))
		    (part-of ((cat common)
			      (lex "man")))))
	    (affected ((cat common)
		       (lex "dog")))
	    (located {^ affected})
	    (location ((cat pp)
		       (prep ((lex "on")))
		       (np ((cat common)
			    (lex "nose")))))))))
(def-test charles3
  "The hand of the man pets the dog on the nose."
  ((cat clause)
   (proc ((type material) (lex "pet")))
   (partic ((agent ((cat partitive)
		    (part ((cat common)
			   (lex "hand")))
		    (part-of ((cat common)
			      (lex "man")))))
	    (affected ((cat common)
		       (lex "dog")))))
   (circum ((location ((cat pp)
		       (prep ((lex "on")))
		       (np ((cat common)
			    (lex "nose")))))))))

;; Tests: time as present-participle clause
(def-test c45
  "Before jumping, check your watch."
  ((cat clause)
   (mood imperative)
   (process ((type mental) (lex "check")))
   (partic ((processor ((cat pronoun)
                        (index ((person second)
                                (number singular)))))
            (phenomenon ((cat np)
                         (possessor ((cat pronoun)
                                     (index {^3 processor index})))
                         (lex "watch")))))
   (circum
    ((time
      ((cat clause)
       (position front)
       (mood present-participle)
       (binder ((lex "before")))
       (process ((type material) (lex "jump")))
       (controlled {^ partic agent})
       (partic ((agent ((cat pronoun)
                        (effective no)
			(index {^5 partic processor index})))))))))))

(def-test c46
  "We must discover how to do it."
  ((cat clause)
   (process ((type mental)
	     (lex "discover")
	     (transitive yes)
	     (deontic-modality "must")))
   (partic ((processor ((cat personal-pronoun)
                        (index ((concept we1)
                                (person first)
                                (number plural)))))
	    (phenomenon ((cat clause)
			 (mood infinitive)
			 (binder ((lex "how")))
			 (controlled {^ partic agent})
                         (process ((type material) (lex "do")))
                         (partic ((agent ((semantics ((concept we1)))))
				  (affected ((cat pronoun)))))))))))

(def-test c47
  "As soon as koed by Iron Mike, he found himself a millionaire."
  ((cat clause)
   (process ((type composite)
             (relation-type ascriptive)
             (mode equative)
	     (lex "find")))
   (tense past)
   (partic ((agent ((cat personal-pronoun)
                    (index ((concept he1)
                            (person third)
                            (gender masculine)
                            (number singular)))))
            ;; Without binding - specify reflexive in input
	    (identified ((cat personal-pronoun)
                         (lex "himself")))
            (identifier ((cat np)
                         (definite no)
                         (lex "millionaire")))))
   (circum
    ((time ((position front)
            (cat clause)
            (binder ((lex "as soon as")))
            (mood past-participle)
            (controlled {^ partic affected})
            (agentless no)
            (process ((type material)
                      (lex "ko")))
            (tense past)
            (partic ((agent ((head ((cat person-name)
                                    (first-name ((lex "Iron")))
                                    (last-name ((lex "Mike")))))))
                     (affected ((index {^5 partic processor})))))))))))

(store-verbs '(("find" "finds" "found" "finding" "found")))

;; Use framenet lex-roles: Grant-permission
(def-test c48
  "Within 10 feet, no smoking is allowed."
  ((cat clause)
   (process ((type lexical) (lex "allow")
             (object-clause to-infinitive)
             (subcat ((1 {^3 lex-roles grantor})
                      (2 {^3 lex-roles grantee})
                      (3 {^3 lex-roles action})))))
   (lex-roles ((action ((cat common)
                        (total -)
                        (lex "smoking")))
               (grantor none)
               (grantee none)))
   (circum ((distance ((cat pp)
                       (position front)
		       (prep ((lex "within")))
		       (np ((cat measure)
			    (quantity ((value 10) (digit yes)))
                            (unit ((lex "foot")))))))))))

(store-plurals '(("foot" "feet")))

;; Variation on Grant-permission
;; Position default is end
(def-test c48bis
  "You are not allowed to smoke within 10 feet."
  ((cat clause)
   (process ((type lexical) (lex "allow")
             (object-clause to-infinitive)
             (subcat ((1 {^3 lex-roles grantor})
                      (2 {^3 lex-roles grantee})
                      (3 {^3 lex-roles action})))))
   (polarity negative)
   (lex-roles ((action ((cat clause)
                        (proc ((lex "smoke")
                               (type lexical)
                               (subcat ((1 {^3 lex-roles ingestor})
                                        (2 {^3 lex-roles substance})))))
                        (lex-roles ((ingestor ((index {^4 grantee index})))))))
               (grantor none)
               (grantee ((cat pronoun)
                         (person second)))))
   (circum ((distance ((cat pp)
		       (prep ((lex "within")))
		       (np ((cat measure)
			    (quantity ((value 10) (digit yes)))
                            (unit ((lex "foot")))))))))))
