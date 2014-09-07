;;; -*- Mode:Lisp; Syntax:Common-Lisp; -*-
;;; ------------------------------------------------------------
;;; File        : IR.Lisp
;;; Description : Sample inputs to test GR
;;; Author      : Michael Elhadad and Jacques Robin
;;; Created     : Aug 14 91
;;; Modified    : Sep 17 91 Added relative examples
;;;               19 Dec 91 Changed to use def-test.
;;;               01 Dec 95 Added HTML examples
;;;               31 Mar 97 Added Comparative/Superlative examples (Yael)
;;; Language    : Common Lisp
;;; ------------------------------------------------------------

(in-package :fug5)

(clear-tests)


(def-test t1
  "This car is expensive."
  ((cat clause)
   (proc ((type ascriptive)
	  (mode attributive)))        ;; default
   (partic ((carrier ((lex "car")
		      (cat common)
		      (distance near)))
	    (attribute ((lex "expensive")
			(cat ap)))))))


(def-test t2
  "John gives a blue book to Mary."
  ((cat clause)
   (proc ((type composite)
	  (relation-type possessive)
	  (mode attributive)          ;; default
	  (agentive yes)              ;; default
	  (effective yes)             ;; default
	  (effect-type dispositive)   ;; default
	  (dative-prep "to")
	  (lex "give")))
   (dative-move no)
   (partic ((agent ((cat proper) (lex "John")))
	    (affected ((cat proper) (lex "Mary")))
	    (possessor {^ affected})
	    (possessed ((lex "book")
			(cat common)
			(definite no)
			(describer === "blue")))))))


(def-test t2bis
  "John gives it to Mary."
  ;; fail with (dative-move yes)
  ((cat clause)
   (proc ((type composite)
	  (relation-type possessive)
	  (lex "give")))
   (partic ((agent ((cat proper) (lex "John")))
	    (affected ((cat proper) (lex "Mary")))
	    (possessor {^ affected})
	    (possessed ((cat pronoun)))))))


(def-test t2ter
  "John never gives it to Mary."
  ;; test adverb
  ((cat clause)
   (adverb ((lex "never")))
   (proc ((type composite)
	  (relation-type possessive)
	  (lex "give")))
   (partic ((agent ((cat proper) (lex "John")))
	    (affected ((cat proper) (lex "Mary")))
	    (possessor {^ affected})
	    (possessed ((cat pronoun)))))))

(def-test t2quad
  "John does not often give it to Mary."
  ;; test adverb + negation
  ((cat clause)
   (adverb ((lex "often")))
   (polarity negative)
   (proc ((type composite)
	  (relation-type possessive)
	  (lex "give")))
   (partic ((agent ((cat proper) (lex "John")))
	    (affected ((cat proper) (lex "Mary")))
	    (possessor {^ affected})
	    (possessed ((cat pronoun)))))))


(def-test t3
  "A science book is given by John to Mary."
  ((cat clause)
   (proc ((type composite)
	  (relation-type possessive)
	  (voice passive)
	  (lex "give")
	  (dative-prep "to")
	  (agentless no)))
   (dative-move no)
   (partic ((agent ((cat proper) (lex "John")))
	    (affected ((cat proper) (lex "Mary")))
	    (possessed ((cat common)
			(lex "book")
			(definite no)
			(classifier === "science")))))))


(def-test t3bis
  "A science book is never given by John to Mary."
  ((cat clause)
   (adverb ((lex "never")))
   (proc ((type composite)
	  (relation-type possessive)
	  (voice passive)
	  (lex "give")
	  (agentless no)
	  (dative-move no)
	  (dative-prep "to")))
   (partic ((agent ((cat trivial-proper) (lex "John")))
	    (affected ((cat trivial-proper) (lex "Mary")))
	    (possessed ((cat common)
			(lex "book")
			(definite no)
			(classifier === "science")))))))

(def-test t3ter
  "A science book is not really given by John to Mary."
  ((cat clause)
   (polarity negative)
   (adverb ((lex "really")))
   (proc ((type composite)
	  (relation-type possessive)
	  (voice passive)
	  (lex "give")))
   (dative-move no)
   (agentless no)
   (partic ((agent ((cat trivial-proper) (lex "John")))
	    (affected ((cat trivial-proper) (lex "Mary")))
	    (possessed ((cat common)
			(lex "book")
			(definite no)
			(classifier === "science")))))))

(def-test t3quad
  "Mary is not really given a science book."
  ((cat clause)
   (polarity negative)
   (adverb ((lex "really")))
   (agentless yes)
   (proc ((type composite)
	  (relation-type possessive)
	  (voice passive)
	  (lex "give")))
   (partic ((agent ((cat trivial-proper) (lex "John")))
	    (affected ((cat trivial-proper) (lex "Mary")))
	    (possessed ((cat common)
			(lex "book")
			(definite no)
			(classifier === "science")))))))


(def-test t4
  "John likes Mary."
  ((cat clause)
   (proc ((type mental) (lex "like")))
   (partic ((processor ((cat proper) (lex "John")))
	    (phenomenon ((cat proper) (lex "Mary")))))))



(def-test t5
  "John is the teacher."
  ((cat clause)
   (proc ((type ascriptive) (mode equative)))
   (partic ((identified ((lex "John") (cat proper)))
	    (identifier ((lex "teacher") (cat common)))))))



(def-test t6
  "The president is John."
  ((cat clause)
   (proc ((type ascriptive)
	  (voice passive)
	  (mode equative)))
   (partic ((identified ((lex "John") (cat proper)))
	    (identifier ((lex "president") (cat common)))))))




;; Really difficult: throw is classified as "locative" by Fawcett.
;; However, throw allows for dative move, which we prefer to leave
;; as a characteristic unique to possessive processes.
;; We could say that throw is a combination of material/locative AND
;; possessive, that would require us to change the grammar to support
;; composite processes of 3 processes.  That would be a Good thing to do in
;; any case, to account for sentences like [4 participants]:
;; I paid the hotel $2 for the room.
;; No! This is not a 4 participants sentence, sorry...
(def-test t7
  "Mary is thrown a heavy ball by John."
  ((cat clause)
   (proc ((type composite)
	  (relation-type possessive)
	  (dative-prep "to")
	  (lex "throw")
	  (agentless no)
	  (voice passive)))
   (partic ((agent ((lex "John") (cat proper)))
	    (possessed ((lex "ball")
			(cat common)
			(definite no)
			(describer === "heavy")))
	    (possessor ((lex "Mary") (cat proper)))))))



(def-test t8
  "John's book is beautiful."
  ((cat clause)
   (proc ((type ascriptive) (mode attributive)))
   (partic ((carrier ((lex "book")
		      (cat common)
		      (possessor ((lex "John") (cat proper)))))
	    (attribute === "beautiful")))))



(def-test t9
  "The SIG-display marker moves."
  ((cat clause)
   (proc ((type material)
	  (lex "move")
	  (agentive no)))
   (partic ((affected ((cat common)
		       (lex "SIG-display marker")))))))



(def-test t10
  "The SIG-display marker moves to the right."
  ((cat clause)
   (proc ((type composite)
	  (relation-type locative)
	  (agentive no)
	  (lex "move")))
   (partic ((affected ((cat common)
		       (lex "SIG-display marker")))
	    (located {^ affected})
	    (location ((cat pp)
		       (prep ((lex "to")))
		       (np ((cat common) (lex "right")))))))))



(def-test t10bis
  "The SIG-display marker can move to the right."
  ((cat clause)
   (epistemic-modality possible)
   (proc ((type composite)
	  (relation-type locative)
	  (agentive no)
	  (lex "move")))
   (partic ((affected ((cat common)
		       (lex "SIG-display marker")))
	    (located {^ affected})
	    (location ((cat pp)
		       (prep === "to")
		       (np ((cat common) (lex "right")))))))))



(def-test t11
  "The SIG-display marker moves from the left."
  ((cat clause)
   (proc ((type composite)
	  (relation-type locative)
	  (agentive no)
	  (lex "move")))
   (partic ((affected ((cat common)
		       (lex "sig-display marker")))
	    (located {^ affected})
	    (location ((cat pp)
		       (prep === "from")
		       (np ((cat common) (lex "left")))))))))


(def-test t12
  "The SIG-display marker moves from the left to the right."
  ((cat clause)
   (proc ((type composite)
	  (relation-type locative)
	  (agentive no)
	  (lex "move")))
   (partic
    ((affected ((cat common)
		(lex "sig-display marker")))
     (located {^ affected})
     (location
      ((cat list)
       (distinct ~(((cat pp)
		    (prep === "from")
		    (np ((cat common) (lex "left"))))
		   ((cat pp)
		    (prep === "to")
		    (np ((cat common) (lex "right"))))))))))))



;; Present-participle and infinitive
;; (both governed by the verb "cause")
(def-test t13-0
    "The power level increases."
  ((cat clause)
   (proc ((type material)
	  (lex "increase")
	  (agentive no)))
   (partic ((affected ((cat common) (lex "power level")))))))

(def-test t13-1
    "The power level's increasing."
  ((cat clause)
   (mood present-participle)
   (proc ((type material)
	  (lex "increase")
	  (agentive no)))
   (partic ((affected ((cat common) (lex "power level")))))))

(def-test t13-2
    "The marker moves to the right."
  ((cat clause)
   (proc ((type composite) (relation-type locative)
	  (agentive no) (lex "move")))
   (partic
    ((affected ((cat common) (lex "marker") (index ((concept sdmarker)))))
     (located {^ affected})
     (location ((cat pp)
		(prep === "to")
		(np ((cat common) (lex "right")))))))))

(def-test t13
  "The power level's increasing causes the sig-display marker to move to the right."
  ((cat clause)
   (proc ((type lexical)
	  (lex "cause")
	  ;; These two features are a short notation for the
          ;; syntactic constraints described in the subcat frame below
	  (subject-clause present-participle)
	  (object-clause to-infinitive)
	  (subcat ((1 {^3 lex-roles influence})
		   (2 {^3 lex-roles influenced})
		   (3 {^3 lex-roles soa})
		   (1 ((cat clause)
		       (mood present-participle)))
		   (2 ((cat np)))
		   (3 ((cat clause)
		       (mood to-infinitive)
		       (controlled {^ oblique 1})))))))
   (lex-roles
    ((influence ((cat clause)
		 (proc ((type material)
			(lex "increase")
			(agentive no)))
		 (partic ((affected ((cat common)
				     (lex "power level")))))))
     (influenced ((cat common) (index ((concept sdmarker)))
		  (classifier === SIG-display) (lex "marker")))
     (soa ((cat clause)
	   (proc ((type composite) (relation-type locative)
		  (agentive no) (lex "move")))
	   (partic
	    ((affected ((cat common) (lex "marker") (index ((concept sdmarker)))))
	     (located {^ affected})
	     (location ((cat pp)
			(prep === "to")
			(np ((cat common) (lex "right")))))))))))))


(def-test t14
  "For her to do it."
  ((cat clause)
   (mood for-to-infinitive)
   (proc ((lex "do") (type material)))
   (partic ((agent ((cat personal-pronoun)
		    (gender feminine)))
	    (affected ((cat personal-pronoun)
		       (gender neuter)))))))



(def-test t15
  "For her to do it is a bold statement."
  ((cat clause)
   (proc ((type ascriptive)
	  (mode attributive)))
   (partic ((carrier ((cat clause)
		      (mood for-to-infinitive)
		      (proc ((lex "do")
			     (type material)))
		      (partic ((agent ((cat personal-pronoun)
				       (gender feminine)))
			       (affected ((cat personal-pronoun)
					  (gender neuter)))))))
	    (attribute ((cat common)
			(definite no)
			(lex "statement")
			(describer === "bold")))))))



(def-test t15-bis
  "For her to do it must be a bold statement."
  ((cat clause)
   (epistemic-modality inference)
   (proc ((type ascriptive)
	  (mode attributive)))
   (partic ((carrier ((cat clause)
		      (mood for-to-infinitive)
		      (proc ((lex "do")
			     (type material)))
		      (partic ((agent ((cat personal-pronoun)
				       (gender feminine)))
			       (affected ((cat personal-pronoun)
					  (gender neuter)))))))
	    (attribute ((cat common)
			(definite no)
			(lex "statement")
			(describer === "bold")))))))


(def-test t16
  "The man, whom I know."
  ((cat common)
   (head === "man")
   (animate yes)
   (qualifier ((cat clause)
	       (restrictive no)
	       (scope {^ participants phenomenon})
	       ;; (scope ((role phenomenon)))
	       (proc ((type mental)
		      (lex "know")))
	       (participants ((processor ((cat personal-pronoun)
					  (person first)))))))))




(def-test t16bis
  "The man, whom I may know."
  ((cat common)
   (head === "man")
   (animate yes)
   (qualifier ((cat clause)
	       (epistemic-modality "may")
	       (restrictive no)
	       ;; (scope ((role phenomenon)))
	       (scope {^ participants phenomenon})
	       (proc ((type mental)
		      (lex "know")
		      (transitive yes)))
	       (participants
		((processor ((cat personal-pronoun)
			     (person first)))))))))


(def-test t17
    ;; Use-that to force that
  ("The man that I know."
   "The man whom I know.")
  ((cat common)
   (head === "man")
   (animate yes)
   (qualifier ((cat clause)
               ;; ***** here is the difference
               (restrictive yes)
               (use-that yes)
               (scope {^ participants phenomenon})
               (proc ((type mental)
                      (lex "know")))
               (participants
                ((processor ((cat personal-pronoun)
                             (person first)))))))))



(def-test t18
  "The man that knows me."
  ((cat common)
   (head === "man")
   (qualifier ((cat clause)
	       (restrictive yes)
	       (scope {^ partic processor})
	       (proc ((type mental)
		      (lex "know")
		      (transitive yes)))
	       (partic ((phenomenon ((cat personal-pronoun)
				     (person first)))))))))


(def-test t19
  "The position to which the marker moves."
  ((cat common)
   (head === "position")
   (qualifier ((cat clause)
	       (scope {^ partic location})
	       (proc ((type composite)
		      (lex "move")
		      (agentive no)))
	       (partic ((affected ((cat common) (head === "marker")))
			(location ((cat pp)
				   (relative-embedded yes)
				   (question-embedded no)
				   (prep === to)))))))))

(def-test t19bis
  "Where does the marker move?"
  ((cat clause)
   (mood wh)
   (scope {^ partic location})
   (proc ((type composite)
	  (lex "move")
	  (agentive no)))
   (partic ((affected ((cat common) (head === "marker")))
	    (location ((relative-embedded yes)
		       (question-embedded no)
		       (prep === to)))))))



(def-test t20
  "The position of the marker."
  ((cat common)
   (head === "position")
   (definite yes)
   (qualifier ((cat pp)
	       (restrictive yes)
	       (np ((cat common)
		    (head === "marker")))))))



(def-test t21
  "My wife, Carol."
  ((cat np)
   (complex apposition)
   (restrictive no)
   (distinct ~(((cat common)
		(head === "wife")
		(possessor ((cat personal-pronoun)
			    (person first))))
	       ((cat proper)
		(restrictive no)
		(head === "Carol"))))))



(def-test t22
  "My brother Steve."
  ((cat np)
   (complex apposition)
   (restrictive yes) ;; **** Here is the difference
   (distinct
    ~(((cat common)
       (head === "brother")
       (possessor  ((cat personal-pronoun)
		    (person first))))
      ((cat proper)
       (head === "Steve"))))))



(def-test t23
  "An intelligent person."
  ((cat common)
   (head === "person")
   (definite no)
   (describer === "intelligent")))




(def-test t24
  ;; New treatment of lists: complex can be apposition or conjunction
  "My brother Steve and my wife, Carol."
  ((cat np)
   (complex conjunction)
   (distinct
    ~(((complex apposition)
       (restrictive yes)
       (distinct ~(((cat common)
		    (head === "brother")
		    (possessor ((cat personal-pronoun)
				(person first))))
		   ((cat proper)
		    (head === "Steve")))))
      ((complex apposition)
       (restrictive no)
       (distinct ~(((cat common)
		    (head === "wife")
		    (possessor ((cat personal-pronoun)
				(person first))))
		   ((cat proper)
		    (head === "Carol")))))))))




(def-test t25
  "My wife, Carol, my brother Steve and John."
  ((cat np)
   (complex conjunction)
   (distinct
    ~(((complex apposition)
       (restrictive no)
       (distinct ~(((cat common)
		    (head === "wife")
		    (possessor ((cat personal-pronoun)
				(person first))))
		   ((cat proper)
		    (head === "Carol")))))
      ((complex apposition)
       (restrictive yes)
       (distinct ~(((cat common)
		    (head === "brother")
		    (possessor ((cat personal-pronoun)
				(person first))))
		   ((cat proper)
		    (head === "Steve")))))
      ((cat proper)
       (lex "John"))))))



(def-test t26
  "Take the hammer and hit the nail."
  ((cat clause)
   (complex conjunction)
   (common ((mood imperative)))
   (distinct
    ~(((proc ((type material) (lex "take")))
       (partic ((agent ((cat personal-pronoun) (person second)))
		(affected ((cat common) (head === "hammer"))))))
      ((proc ((type material) (lex "hit")))
       (partic ((agent ((cat personal-pronoun) (person second)))
		(affected ((cat common) (head === "nail"))))))))))



(def-test t26-bis
  "First, take the hammer."
  ((cat clause)
   (mood imperative)
   (relaters ((time === "first")))
   (proc ((type material) (lex "take")))
   (partic ((agent ((cat personal-pronoun) (person second)))
	    (affected ((cat common) (head === "hammer")))))))




(def-test t27
  "First, take the hammer and then, hit the nail."
  ((cat clause)
   (complex conjunction)
   (common ((mood imperative)))
   (distinct
    ~(((proc ((type material) (lex "take")))
       (relaters ((time === "first")))
       (partic ((affected ((cat common) (head === "hammer"))))))
      ((proc ((type material) (lex "hit")))
       (relaters ((time === "then")))
       (partic ((affected ((cat common) (head === "nail"))))))))))



;; Ellipsis of verb
(def-test t27bis
  "Bears bite people and dogs cats."
  ((cat clause)
   (complex conjunction)
   (common ((mood declarative)))
   (distinct
    ~(((process ((type material) (lex "bite")))
       (partic ((agent ((cat common)
			(lex "bear")
			(semantics ((index ((concept bears)))))
			(number plural)
			(definite no)))
		(affected ((cat common)
			   (lex "people")
			   (countable no))))))
      ((process ((type material) (lex "bite")))
       (partic ((agent ((cat common)
			(lex "dog")
			(semantics ((index ((concept dog)))))
			(number plural)
			(definite no)))
		(affected ((cat common)
			   (lex "cat")
			   (definite no)
			   (number plural))))))))))

(def-test t28
  ;; t28 fails because epistemic-modality is not compatible with imperative.
  "<fail>"
  ((cat clause)
   (complex conjunction)
   (common ((mood imperative)))
   (distinct
    ~(((proc ((type material) (lex "take")))
       (epistemic-modality possible)
       (partic ((affected ((cat common) (head === "hammer"))))))
      ((proc ((type material) (lex "hit")))
       (partic ((affected ((cat common) (head === "nail"))))))))))



(def-test t29
  "In order to install the battery, clean the box."
  ((cat clause)
   (mood imperative)
   (proc ((type material)
	  (lex "clean")))
   (partic ((affected ((cat common) (head === "box")))
	    (agent ((semantics ((index ((concept c-user)))))))))
   (circum ((purpose ((cat clause)
		      (position front)
		      (mood to-infinitive)
		      (proc ((type material)
			     (lex "install")))
		      (binder ((lex "in order")))
		      (controlled {^ partic agent})
		      (partic ((affected ((cat common)
					  (head === "battery")))))))))))

(def-test t30
  "The SIG-display marker can move to the right."
  ((cat clause)
   (epistemic-modality possible)
   (proc ((type composite)
	  (relation-type locative)
	  (agentive no)
	  (lex "move")))
   (partic
    ((affected ((cat common)
		(lex "sig-display marker")))
     (located {^ affected})
     (location ((cat pp)
		(prep === "to")
		(np ((cat common)
		     (lex "right")))))))))



(def-test t33
  "Hit the nail with the hammer."
  ((cat clause)
   (mood imperative)
   (proc ((type material)
	  (lex "hit")))
   (partic ((affected ((cat common) (head === "nail")))))
   (pred-modif ((instrument ((cat pp)
			     (np ((cat common) (head === "hammer")))))))))


(def-test t34
  "Hit the nail using the hammer."
  ((cat clause)
   (mood imperative)
   (proc ((type material) (lex "hit")))
   (partic ((affected ((cat common) (head === nail)))))
   (pred-modif ((instrument ((cat pp)
			     (prep ((lex "using")))
			     (np ((cat common) (head === "hammer")))))))))


(def-test t35
  "Take the nail off using a screwdriver."
  ((cat clause)
   (mood imperative)
   (proc ((type material)
	  (lex "take")
	  (particle "off")))
   (partic ((affected ((cat common) (head === nail)))))
   (pred-modif ((instrument ((cat pp)
			     (prep ((lex "using")))
			     (np ((cat common)
				  (head === screwdriver)
				  (definite no)))))))))



(def-test t36
  "The nail is taken off using a screwdriver."
  ((cat clause)
   (proc ((type material)
	  (voice passive)
	  (lex "take")
	  (particle "off")))
   (partic ((affected ((cat common) (head === nail)))))
   (pred-modif ((instrument ((cat pp)
			     (prep ((lex "using")))
			     (np ((cat common)
				  (head === screwdriver)
				  (definite no)))))))))



(def-test t38
  ;; Add an irregular verb to the lexicon with store-verbs
  ;; Treatment of non-movable particles as a single string (sort of hack)
  ;; Test accompaniment role
  "The battery lines up with the compartment."
  ((cat clause)
   (proc ((type locative) (lex "line up")))
   (partic ((location ((cat pp)
		       (prep === "with")
		       (np ((cat common) (head === compartment)))))
	    (located ((cat common) (head === battery)))))))
(store-verbs '( ("line up" "lines up" "lined up" "lining up" "lined up")) )


(def-test t39
  ;; Test reason role
  "He does it because of you."
  ((cat clause)
   (process ((type material)
	     (lex "do")))
   (partic ((agent ((cat personal-pronoun)
		    (animate yes)
		    (gender masculine)
		    (person third)
		    (number singular)))
	    (affected ((cat personal-pronoun)
		       (gender neuter)
		       (person third)
		       (number singular)))))
   (circum ((reason ((cat pp)
		     (np ((cat personal-pronoun)
			  (gender masculine)
			  (person second)
			  (number singular)))))))))



(def-test t40
  ;; Test reason role with a clause
  "He does it because he likes you."
  ((cat clause)
   (process ((lex "do") (type material)))
   (partic ((agent ((cat personal-pronoun) (gender masculine)))
	    (affected ((cat personal-pronoun)))))
   (circum ((reason ((cat clause)
		     (process ((type mental) (lex "like")))
		     (partic ((processor ((cat personal-pronoun)
                                          (gender masculine)))
			      (phenomenon ((cat personal-pronoun)
					   (person second)))))))))))

(def-test t40bis
  "He does it when he likes you."
  ((cat clause)
   (process ((lex "do")
	     (type material)))
   (partic ((agent ((cat personal-pronoun)
		    (animate yes)
		    (gender masculine)
		    (person third)
		    (number singular)))
	    (affected ((cat personal-pronoun)
		       (gender neuter)
		       (person third)
		       (number singular)))))
   (circum
    ((time
      ((cat clause)
       (mood bound-adverbial)
       (process ((type mental)
		 (lex "like")))
       (partic ((processor ((cat personal-pronoun)
			    (index {^5 partic agent index})))
		(phenomenon ((cat personal-pronoun)
			     (person second)
			     (number singular)))))))))))


;; JR-11-15-92: changed concept pointer to index pointer in controlled subject.
(def-test n40ter
  "When hitting you, Sam hurt Mary."
  ((cat clause)
   (process ((lex "hurt")
	     (tense past)
	     (type material)))
   (partic ((agent ((cat proper)
		    (animate yes)
		    (gender masculine)
		    (lex "Sam")
		    (number singular)))
	    (affected ((cat proper)
		       (lex "Mary")
		       (number singular)))))
   (circum
    ((time
      ((cat clause)
       (position front)
       (mood present-participle)
       (process ((type material) (lex "hit")))
       (controlled {^ partic agent})
       (partic ((agent ((index {^5 partic agent index})))
		(affected ((cat personal-pronoun)
			   (person second)
			   (number singular)))))))))))
(store-verbs '( ("hurt" "hurts" "hurt" "hurting" "hurt")) )


;; JR-11-15-92: changed concept pointer to index pointer in controlled subject.
(def-test t40quad
  "When done properly, it can be safe."
  ((cat clause)
   (process ((type ascriptive)))
   (epistemic-modality possible)
   (partic ((carrier ((cat personal-pronoun)
		      (gender neuter)
		      (person third)
		      (number singular)))
	    (attribute ((lex "safe") (cat ap)))))
   (circum ((time ((cat clause)
		   (position front)
		   (mood past-participle)
		   (process ((type material) (lex "do")))
		   (controlled {^ partic affected})
		   (partic ((affected ((index {^5 partic carrier index})))))
		   (circum ((manner ((cat adv)
				     (lex "properly")))))))))))


(def-test t41
  ;; Test behalf role
  "He scored 39 points for the Lakers."
  ((cat clause)
   (tense past)
   (process ((type material)
	     (lex "score")))
   (partic ((agent ((cat personal-pronoun)
		    (animate yes)
		    (gender masculine)
		    (person third)
		    (number singular)))
	    (affected ((cat common)
		       (definite no)
		       (cardinal ((value 39) (digit yes)))
		       (lex "point")))))
   (circum ((behalf ((cat pp)
		     (np ((cat compound-proper)
			  (number plural)
			  (head ((cat team-name)
				 (franchise ((lex "Laker")))))))))))))


(def-test t42
  "The holding battery cover plate covers the holding battery compartment."
  ((cat clause)
   (proc ((type locative)
	  (mode equative)
	  (cat verb-group)
	  (lex "cover")))
   (partic ((identified ((cat common)
			 (head ((cat noun)
				(lex "holding battery cover plate")))))
	    (identifier ((cat common)
			 (head ((cat noun)
				(lex "holding battery compartment")))))))))



(def-test t43
  "The holding battery compartment is in the back of the RT at the bottom left corner."
  ((cat clause)
   (proc ((type locative) (mode attributive)))
   (partic
    ((carrier ((cat common)
	       (lex "holding battery compartment")))
     (attribute ((cat pp)
		 (complex apposition)
		 (restrictive yes)
		 (distinct
		  ~(((prep ((cat prep) (lex "in")))
		     (np
		      ((cat common)
		       (head ((cat noun) (lex "back")))
		       (qualifier ((cat pp)
				   (np ((cat common)
					(head ((cat noun)
					       (lex "RT"))))))))))
		    ((prep ((cat prep) (lex "at")))
		     (np ((cat common)
			  (describer ((cat list)
				      (elements ~(((cat ap) (lex "bottom"))
						  ((cat ap) (lex "left"))))))
			  (head ((cat noun) (lex "corner"))))))))))))))



(def-test t44
  ;; TEST: try your own syntactic analysis of this one and come up with 3
  ;; reasons why it is inferior to this one.
  ;; Send answers to djk@cs.columbia.edu
  "The holding battery compartment contains the old holding battery shown in the cut-away view."
  ((cat clause)
   (proc ((type locative)
	  (mode equative)
	  (cat verb-group)
	  (lex "contain")))
   (partic
    ((identified ((cat common)
		  (head ((cat noun)
			 (lex "holding battery compartment")))))
     (identifier
      ((cat common)
       (head ((cat noun) (lex "old holding battery")))
       (qualifier
	((cat ap)
	 (head === "shown")
	 (qualifier ((cat pp)
		     (prep ((cat prep) (lex "in")))
		     (np ((cat common)
			  (head ((cat noun)
				 (lex "cut-away view")))))))))))))))



(def-test t44bis
  ;; Something like "a candle contains wax" in terms of case roles
  ;; So we do locative, with candle as location.
  "A candle is composed of wax."
  ((cat clause)
   (process ((type locative) (mode equative)
	     (lex "compose") (voice passive)
	     (passive-prep ((lex "of")))))
   ;; Default is without agent - override it.
   (agentless no)
   (partic ((location ((cat common)
		       (definite no)
		       (lex "candle")))
	    (located ((cat common)
		      (countable no)
		      (lex "wax")))))))


;; Example of "is contained in" as a passive of "contains"
;; NOW: there is something very deep going on here concerning the semantics
;; of "contain" and the semantics of the roles "location" and "located".
;; This should be inverse: anther is location, sac is located.
;; But, the passive voice switches the roles around, and the semantics of
;; "contain" is "Location contains located".
;; SO: what is really important here is not the relation located/location
;; which is expressed by the lexical verb, but rather the discourse
;; perspective which is captured by the distinction identified/identifier
;; which is the more generic classification of equative relations.
;; So here, we use identified:anther, identifier:sac, voice:passive
;; (switches around the perspective).
;; NOTE: Check Beth Levin's approach
(def-test t44ter
  "A pollen sac is contained in an anther."
  ((cat clause)
   (proc ((type locative)
	  (mode equative);; important - otw relation not passivable
	  (voice passive)
	  (lex "contain")
	  (passive-prep ((lex "in")))))
   (agentless no)
   (partic ((identifier ((cat common)
			 (definite no)
			 (classifier ((lex "pollen")))
			 (lex "sac")))
	    (identified ((cat common)
			 (definite no)
			 (lex "anther")))))))


(def-test t45
  ;; Show that conjunction of different cats fails
  "<fail>"
  ((cat np)
   (complex conjunction)
   (distinct ~( ((cat common)
		 (head === "box"))
		((cat ap)
		 (head === "blue")) ))))




(def-test t46
  ;; Show conjunction of low-level constituents (noun)
  "My advisor and friend, Steve."
  ((cat np)
   (complex apposition)
   (restrictive no)
   (distinct
    ~(((cat common)
       (head ((cat noun)
	      (complex conjunction)
	      (distinct ~( ((lex "advisor"))
			   ((lex "friend")) ))))
       (possessor ((cat personal-pronoun)
		   (person first))))
      ((cat proper)
       (head === "Steve"))))))




(def-test t47
  ;; Test ordinal
  "My first two years were happy and uneventful."
  ((cat clause)
   (proc ((type ascriptive)
	  (mode attributive)))
   (tense past)
   (partic ((carrier ((cat common)
		      (head === "year")
		      (ordinal ((value 1) (digit no)))
		      (cardinal ((value 2) (digit no)))
		      (possessor ((cat personal-pronoun)
				  (number singular)
				  (person first)))))
	    (attribute ((cat ap)
			(complex conjunction)
			(distinct ~( ((head === "happy"))
				     ((head === "uneventful"))))))))))




(def-test t48
  "Old McDonald had a farm."
  ((cat clause)
   (proc ((type possessive) (mode attributive)))
   (tense past)
   (partic ((possessor ((cat proper) (head === "Old McDonald")))
	    (possessed ((cat common) (head === farm) (definite no)))))))



(def-test t49
  "Old McDonald owned a farm."
  ((cat clause)
   (proc ((type possessive) (mode equative)))
   (tense past)
   (partic ((possessor ((cat proper) (head === "Old McDonald")))
	    (possessed ((cat common) (head === farm) (definite no)))))))


(def-test t50
  "A farm was owned by Old McDonald."
  ((cat clause)
   (proc ((type possessive) (mode equative) (voice passive) (agentless no)))
   (tense past)
   (partic ((possessor ((cat proper) (head === "Old McDonald")))
	    (possessed ((cat common) (head === farm) (definite no)))))))


(def-test t51
  "The book was given by John to Mary."
  ((cat clause)
   (tense past)
   (proc ((type composite)
	  (relation-type possessive)
	  (voice passive)
	  (agentless no)
	  (dative-move no)))
   (partic ((agent ((cat basic-proper) (lex "John")))
	    (affected ((cat basic-proper) (lex "Mary")))
	    (possessor {^ affected})
	    (possessed ((cat common) (lex "book")))))))


;; ======================================================================
;; Test questions
;; ======================================================================

;; ----------------------------------------------------------------------
;; YES-NO
;; ----------------------------------------------------------------------

(def-test t52
  "Is this car expensive?"
  ((cat clause)
   (mood yes-no)
   (proc ((type ascriptive)))
   (partic ((carrier ((lex "car")
		      (cat common)
		      (distance near)))
	    (attribute === "expensive")))))



(def-test t53
  "Will John give Mary a blue book?"
  ((cat clause)
   (mood yes-no)
   (tense future)
   (proc ((type composite)
	  (relation-type possessive)
	  (dative-prep "to")
	  (lex "give")))
   (partic ((agent ((cat proper) (lex "John")))
	    (affected ((cat proper) (lex "Mary")))
	    (possessor {^ affected})
	    (possessed ((lex "book")
			(cat common)
			(definite no)
			(describer === "blue")))))))



(def-test t54
  "John did give Mary a blue book."
  ((cat clause)
   (insistence yes)
   (proc ((type composite)
	  (relation-type possessive)
	  (dative-prep "to")
	  (lex "give")))
   (tense past)
   (partic ((agent ((cat proper) (lex "John")))
	    (affected ((cat proper) (lex "Mary")))
	    (possessor {^ affected})
	    (possessed ((lex "book")
			(cat common)
			(definite no)
			(describer === "blue")))))))




;; ----------------------------------------------------------------------
;; WH
;; ----------------------------------------------------------------------

(def-test t55
  "What is expensive?"
  ((cat clause)
   (mood wh)
   ;; (scope ((role carrier)))
   (scope {^ partic carrier})
   (proc ((type ascriptive)))
   (partic ((attribute === "expensive")))))


(def-test t55bis
  "How is this car?"
  ((cat clause)
   (mood wh)
   ;; (scope ((role carrier)))
   (scope {^ partic attribute})
   (proc ((type ascriptive)))
   (partic ((attribute === "expensive")
	    (carrier ((lex "car")
		      (cat common)
		      (distance near)))))))


(def-test t56
  "What is a classic?"
  ((cat clause)
   (mood wh)
   (proc ((type ascriptive)))
   (scope {^ partic carrier})
   (partic ((attribute ((cat common) (definite no) (lex "classic")))))))



(def-test t57
  "Who is your father?"
  ((cat clause)
   (mood wh)
   (scope {^ partic identified})
   (proc ((type ascriptive) (mode equative)))
   (partic ((identified ((animate yes)))
	    (identifier ((cat common)
			 (possessor ((cat personal-pronoun)
				     (person second)))
			 (head === "father")))))))



(def-test t58
  "Which is your father?"
  ((cat clause)
   (mood wh)
   (scope {^ partic identifier})
   (proc ((type ascriptive) (mode equative)))
   (partic ((identified ((cat common)
			 (possessor ((cat personal-pronoun)
				     (person second)))
			 (head === "father")))))))



(def-test t59
  "Where is your mother?"
  ((cat clause)
   (mood wh)
   (scope {^ partic location})
   (proc ((type locative)))
   (partic ((located ((cat common)
		      (possessor ((cat personal-pronoun)
				  (person second)))
		      (head === "mother")))))))



(def-test t60
  "Who is in your house?"
  ((cat clause)
   (mood wh)
   (scope {^ partic located})
   (proc ((type locative)))
   (partic ((located ((animate yes)))
	    (location ((cat pp)
		       (prep === "in")
		       (np ((cat common)
			    (possessor ((cat personal-pronoun)
					(person second)))
			    (head === "house")))))))))



(def-test t61
  "What covers the opening?"
  ((cat clause)
   (mood wh)
   (scope {^ partic located})
   (proc ((type locative) (mode equative) (lex "cover")))
   (partic ((location ((cat common) (head === "opening")))))))



(def-test t62
  "What does the seal cover?"
  ((cat clause)
   (mood wh)
   (scope {^ partic location})
   (proc ((type locative) (mode equative) (lex "cover")))
   (partic ((located ((cat common) (head === "seal")))))))



(def-test t63
  "When is the game?"
  ((cat clause)
   (mood wh)
   (scope {^ partic time})
   ;; (scope ((role time)))
   (proc ((type temporal)))
   (partic ((located ((cat common) (head === "game")))))))




(def-test t64
  "What happens then?"
  ((cat clause)
   (mood wh)
   ;; (scope ((role located)))
   (scope {^ partic located})
   (proc ((type temporal) (lex "happen")))
   (partic ((time ((cat adv) (lex "then")))))))



(store-verbs '( ("happen" "happens" "happened" "happening" "happened")) )
(def-test t64bis
  "The game happened then."
  ((cat clause)
   (tense past)
   (proc ((type temporal) (lex "happen")))
   (partic ((located ((cat np) (head === game)))
	    (time ((cat adv) (lex "then")))))))


;; JR note: equative and circumstance-as-process flavors seem difficult
;; without specifying a time unit in the question (e.g. "What day is the
;; 10th?" "How long does the fair last?")

(def-test t65
  "How is your sister?"
  ((cat clause)
   (mood wh)
   ;; (scope ((role attribute)))
   (scope {^ partic attribute})
   (proc ((type ascriptive)))
   (partic ((carrier ((cat common)
		      (possessor ((cat personal-pronoun)
				  (person second)))
		      (head === "sister")))))))



(def-test t66
  "With whom is your sister?"
  ((cat clause)
   (mood wh)
   (proc ((type accompaniment)))
   (scope {^ partic location})
   (partic ((location ((animate yes)))
	    (located ((cat common)
		      (possessor ((cat personal-pronoun)
				  (person second)))
		      (head === "sister")))))))



(def-test t67
  "Who has a PhD?"
  ((cat clause)
   (mood wh)
   (scope {^ partic possessor})
   (proc ((type possessive)))
   (partic ((possessor ((animate yes)))
	    (possessed ((cat common) (definite no) (head === "PhD")))))))



(def-test t68
  "What does she have?"
  ((cat clause)
   (mood wh)
   (proc ((type possessive)))
   (scope {^ partic possessed})
   (partic ((possessor ((cat personal-pronoun)
			(gender feminine)
			(number singular)
			(person third)))))))


(def-test t68bis
  "What does she sometimes have?"
  ((cat clause)
   (mood wh)
   (adverb === "sometimes")
   (proc ((type possessive)))
   (scope {^ partic possessed})
   (partic ((possessor ((cat personal-pronoun)
			(gender feminine)
			(number singular)
			(person third)))))))

(def-test t69
  "Who owns this book?"
  ((cat clause)
   (mood wh)
   (proc ((type possessive) (mode equative)))
   (scope {^ partic possessor})
   (partic ((possessor ((animate yes)))
	    (possessed ((cat common)
			(distance near)
			(head === "book")))))))


(def-test t69bis
  "Who never owned this book?"
  ((cat clause)
   (mood wh)
   (adverb === never)
   (proc ((type possessive) (mode equative) (tense past)))
   (scope {^ partic possessor})
   (partic ((possessor ((animate yes)))
	    (possessed ((cat common)
			(distance near)
			(head === "book")))))))


(def-test t69ter
  "Who did not finally own this book?"
  ((cat clause)
   (mood wh)
   (adverb === finally)
   (polarity negative)
   (proc ((type possessive) (mode equative) (tense past)))
   (scope {^ partic possessor})
   (partic ((possessor ((animate yes)))
	    (possessed ((cat common)
			(distance near)
			(head === "book")))))))

(def-test t70
  "Which does she own?"
  ((cat clause)
   (mood wh)
   (proc ((type possessive) (mode equative)))
   (scope {^ partic possessed})
   (partic ((possessed ((restrictive yes)))    ;; which vs. what.
	    (possessor ((cat personal-pronoun)
			(gender feminine)
			(person third)
			(number singular)))))))


(def-test t70bis
  "Which does she possibly own?"
  ((cat clause)
   (mood wh)
   (adverb === possibly)
   (proc ((type possessive) (mode equative)))
   (scope {^ partic possessed})
   (partic ((possessed ((restrictive yes)))    ;; which vs. what.
	    (possessor ((cat personal-pronoun)
			(gender feminine)
			(person third)
			(number singular)))))))

(def-test t70ter
  "Which does not she possibly own?"
  ((cat clause)
   (mood wh)
   (adverb === possibly)
   (polarity negative)
   (proc ((type possessive) (mode equative)))
   (scope {^ partic possessed})
   (partic ((possessed ((restrictive yes)))    ;; which vs. what.
	    (possessor ((cat personal-pronoun)
			(gender feminine)
			(person third)
			(number singular)))))))

(def-test t71
  "Who gives a blue book to Mary?"
  ((cat clause)
   (mood wh)
   (scope {^ partic agent})
   (proc ((type composite)
	  (relation-type possessive)
	  (lex "give")))
   (partic ((agent ((animate yes)))
	    (possessed ((lex "book")
			(cat common)
			(definite no)
			(describer === "blue")))
	    (affected ((lex "Mary") (cat proper)))
	    (possessor {^ affected})))))



(def-test t72
  "What will John give to Mary?"
  ((cat clause)
   (mood wh)
   (tense future)
   (scope {^ partic possessed})
   (proc ((type composite)
	  (relation-type possessive)
	  (dative-prep "to")
	  (lex "give")))
   (partic ((agent ((lex "John") (cat proper)))
	    (affected ((lex "Mary") (cat proper)))
	    (possessor {^ affected})))))



(def-test t73
  "To whom will John give a blue book?"
  ((cat clause)
   (mood wh)
   (tense future)
   (scope {^ partic possessor})
   (proc ((type composite)
	  (relation-type possessive)
	  (lex "give")))
   (partic ((agent ((lex "John") (cat proper)))
	    (possessor ((animate yes)))
	    (possessed ((lex "book")
			(cat common)
			(definite no)
			(describer === "blue")))))))
;; Note that scope being DOUBLE role, e.g. here Af/Pr
;; since af and pr unify into affected-carrier.
;; can choose either one in your input.

(def-test t73bis
  "To whom will John eventually give a blue book?"
  ((cat clause)
   (mood wh)
   (adverb === eventually)
   (tense future)
   (scope {^ partic possessor})
   (proc ((type composite)
	  (relation-type possessive)
	  (lex "give")))
   (partic ((agent ((lex "John") (cat proper)))
	    (possessor ((animate yes)))
	    (possessed ((lex "book")
			(cat common)
			(definite no)
			(describer === "blue")))))))

(def-test t73ter
  "To whom will not John eventually give a blue book?"
  ((cat clause)
   (mood wh)
   (polarity negative)
   (adverb === eventually)
   (tense future)
   (scope {^ partic possessor})
   (proc ((type composite)
	  (relation-type possessive)
	  (lex "give")))
   (partic ((agent ((lex "John") (cat proper)))
	    (possessor ((animate yes)))
	    (possessed ((lex "book")
			(cat common)
			(definite no)
			(describer === "blue")))))))
(def-test t74
  "What does Deborah prefer?"
  ((cat clause)
   (mood wh)
   (scope {^ partic phenomenon})
   (proc ((type mental) (lex "prefer")))
   (partic ((processor ((cat proper) (lex "Deborah")))))))



(def-test t75
  "Who worships Baal?"
  ((cat clause)
   (mood wh)
   (scope {^ partic processor})
   (proc ((type mental) (lex "worship")))
   (partic ((processor ((animate yes)))
	    (phenomenon ((cat proper) (lex "Baal")))))))


(def-test t76
  ;; Test reason role with a clause
  "Why does he do it?"
  ((cat clause)
   (mood wh)
   (scope {^ circum reason})
   (proc ((type material)
	  (lex "do")))
   (partic ((agent ((cat personal-pronoun)
		    (animate yes)
		    (gender masculine)
		    (person third)
		    (number singular)))
	    (affected ((cat personal-pronoun)
		       (gender neuter)
		       (person third)
		       (number singular)))))))


(def-test t77
  "From where does the SIG-display marker move to the right?"
  ((cat clause)
   (mood wh)
   (scope {^ pred-modif origin})
   (proc ((type composite)
	  (relation-type locative)
	  (agentive no)
	  (lex "move")))
   (partic ((affected ((cat common) (lex "sig-display marker")))
	    (located {^ affected})
	    (location ((cat pp) (prep === to) (np ((lex "right")))))))))


(store-verbs '( ("win" "wins" "won" "winning" "won")) )

(def-test t77bis
  "You think that John won the prize."
  ((cat clause)
   (proc ((type mental)
	  (object-clause that)
	  (lex "think")))
   (partic ((processor ((cat personal-pronoun)
			(animate yes)
			(person second)
			(number singular)))
	    ;; Win interpreted as "get": Ag/Ca+Pos
	    (phenomenon ((cat clause)
			 (proc ((type composite)
				(relation-type possessive)
				(effective no)
				(tense past)
				(lex "win")))
			 (partic ((possessor ((cat basic-proper)
					      (lex "John")))
				  (agent {^ possessor})
				  (possessed ((cat common)
					      (lex "prize")))))))))))

(def-test t77bisp
  "You think that the prize was won by John."
  ((cat clause)
   (proc ((type mental)
	  (object-clause that)
	  (lex "think")))
   (partic ((processor ((cat personal-pronoun)
			(animate yes)
			(person second)
			(number singular)))
	    ;; Win interpreted as "get": Ag/Ca+Pos
	    (phenomenon ((cat clause)
			 (proc ((type composite)
				(relation-type possessive)
				(effective no)
				(tense past)
				(voice passive)
				(agentless no)
				(lex "win")))
			 (partic ((possessor ((cat basic-proper)
					      (lex "John")))
				  (agent {^ possessor})
				  (possessed ((cat common)
					      (lex "prize")))))))))))

(def-test t77ter
  "Do you think that John won the prize?"
  ((cat clause)
   (mood yes-no)
   (proc ((type mental)
	  (object-clause that)
	  (lex "think")))
   (partic ((processor ((cat personal-pronoun)
			(animate yes)
			(person second)
			(number singular)))
	    ;; Win interpreted as "get": Ag/Ca+Pos
	    (phenomenon ((cat clause)
			 (proc ((type composite)
				(effective no)
				(relation-type possessive)
				(tense past)
				(lex "win")))
			 (partic ((possessor ((cat basic-proper)
					      (lex "John")))
				  (agent {^ possessor})
				  (possessed ((cat common)
					      (lex "prize")))))))))))


;; LONG DISTANCE DEPENDENCY:
(def-test t77quad
  "Who do you think won the prize?"
  ((cat clause)
   (mood wh)
   (scope {^ partic phenomenon partic possessor})
   (proc ((type mental)
	  (object-clause that)
	  (lex "think")))
   (partic ((processor ((cat personal-pronoun)
			(animate yes)
			(person second)
			(number singular)))
	    ;; Win interpreted as "get": Ag/Pd+Pos
	    (phenomenon ((cat clause)
			 (proc ((type composite)
				(effective no)
				(relation-type possessive)
				(tense past)
				(lex "win")))
			 (partic ((possessor ((animate yes)))
				  (agent {^ possessor})
				  (possessed ((cat common)
					      (lex "prize")))))))))))


(def-test t77quin
  "Who do you really think won the prize?"
  ((cat clause)
   (mood wh)
   (adverb === really)
   (scope {^ partic phenomenon partic possessor})
   (proc ((type mental)
	  (object-clause that)
	  (lex "think")))
   (partic ((processor ((cat personal-pronoun)
			(animate yes)
			(person second)
			(number singular)))
	    ;; Win interpreted as "get": Ag/Pd+Pos
	    (phenomenon ((cat clause)
			 (proc ((type composite)
				(relation-type possessive)
				(tense past)
				(lex "win")))
			 (partic ((possessor ((animate yes)))
				  (agent {^ possessor})
				  (possessed ((cat common)
					      (lex "prize")))))))))))


(def-test t77sex
  "Who do you think really won the prize?"
  ((cat clause)
   (mood wh)
   (scope {^ partic phenomenon partic possessor})
   (proc ((type mental)
	  (object-clause that)
	  (lex "think")))
   (partic ((processor ((cat personal-pronoun)
			(animate yes)
			(person second)
			(number singular)))
	    ;; Win interpreted as "get": Ag/Pd+Pos
	    (phenomenon ((cat clause)
			 (adverb === really)
			 (proc ((type composite)
				(effective no)
				(relation-type possessive)
				(tense past)
				(lex "win")))
			 (partic ((possessor ((animate yes)))
				  (agent {^ possessor})
				  (possessed ((cat common)
					      (lex "prize")))))))))))


(def-test t77sept
  "Who do not you honestly think did not really win the prize?"
  ((cat clause)
   (mood wh)
   (polarity negative)
   (adverb === honestly)
   (scope {^ partic phenomenon partic possessor})
   (proc ((type mental)
	  (object-clause that)
	  (lex "think")))
   (partic ((processor ((cat personal-pronoun)
			(animate yes)
			(person second)
			(number singular)))
	    ;; Win interpreted as "get": Ag/Pd+Pos
	    (phenomenon ((cat clause)
			 (adverb === really)
			 (polarity negative)
			 (proc ((type composite)
				(effective no)
				(relation-type possessive)
				(tense past)
				(lex "win")))
			 (partic ((possessor ((animate yes)))
				  (agent {^ possessor})
				  (possessed ((cat common)
					      (lex "prize")))))))))))

;; ======================================================================
;; test RELATIVE CLAUSES
;; ======================================================================

(def-test t78
  "The box, which is expensive."
  ((cat common)
   (head === box)
   (qualifier ((cat clause)
	       (restrictive no)
	       (scope {^ partic carrier})
	       (proc ((type ascriptive)))
	       (partic ((attribute === "expensive")))))))



(def-test t79
  "The box that is a classic."
  ((cat common)
   (head === box)
   (animate no)
   (qualifier ((cat clause)
	       (restrictive yes)
	       (proc ((type ascriptive)))
	       (scope {^ partic carrier})
	       ;; (scope ((role carrier)))
	       (partic ((attribute ((cat common)
				    (definite no)
				    (lex "classic")))))))))



(def-test t80
  ("The man who is your father."
   "The man that is your father.")
  ((cat common)
   (head === man)
   (animate yes)
   (qualifier ((cat clause)
	       (scope {^ partic identified})
	       ;; (scope ((role identified)))
	       (proc ((type ascriptive) (mode equative)))
	       (partic ((identifier ((cat common)
				     (possessor ((cat personal-pronoun)
						 (person second)))
				     (head === "father")))))))))



(def-test t81
  "The man that your father is."
  ((cat common)
   (head === man)
   (animate yes)
   (qualifier ((cat clause)
	       (scope {^ partic identifier})
	       ;; (scope ((role identifier)))
	       (proc ((type ascriptive) (mode equative)))
	       (partic ((identified ((cat common)
				     (possessor ((cat personal-pronoun)
						 (person second)))
				     (head === "father")))))))))




(def-test t82
  "The house where your mother is."
  ((cat common)
   (head === house)
   (qualifier ((cat clause)
	       ;; (scope ((role location)))
	       (scope {^ partic location})
	       (proc ((type locative)))
	       (partic ((located ((cat common)
				  (possessor ((cat personal-pronoun)
					      (person second)))
				  (head === "mother")))))))))


(def-test t82bis
  "The house where your mother currently is."
  ((cat common)
   (head === house)
   (qualifier ((cat clause)
	       (scope {^ partic location})
	       (proc ((type locative)))
	       (adverb === currently)
	       (partic ((located ((cat common)
				  (possessor ((cat personal-pronoun)
					      (person second)))
				  (head === "mother")))))))))



(def-test t83
  "The man, who is in your house."
  ((cat common)
   (head === man)
   (animate yes)
   (qualifier ((cat clause)
	       (restrictive no)
	       (scope {^ partic located})
	       (proc ((type locative)))
	       (partic ((location ((cat pp)
				   (prep === "in")
				   (np ((cat common)
					(possessor ((cat personal-pronoun)
						    (person second)))
					(head === "house")))))))))))




(def-test t84
  "The plate, which covers the opening."
  ((cat common)
   (head === plate)
   (qualifier ((cat clause)
	       (restrictive no)
	       (scope {^ partic located})
	       (proc ((type locative) (mode equative) (lex "cover")))
	       (partic ((location ((cat common)
				   (head === "opening")))))))))




(def-test t85
  "The hole, which the seal covers."
  ((cat common)
   (head === hole)
   (animate no)
   (qualifier ((cat clause)
	       (restrictive no)
	       (scope {^ partic location})
	       (proc ((type locative) (mode equative) (lex "cover")))
	       (partic ((located ((cat common)
				  (head === "seal")))))))))



(def-test t86
  "The time when the game starts."
  ((cat common)
   (head === time)
   (qualifier ((cat clause)
	       (scope {^ partic time})
	       (proc ((type temporal)
		      (lex "start")))
	       (partic ((located ((cat common)
				  (head === "game")))))))))



(def-test t87
  "The thing that happens then."
  ((cat common)
   (head === thing)
   (qualifier ((cat clause)
	       (restrictive yes)
	       (scope {^ partic located})
	       (proc ((type temporal) (lex "happen")))
	       (partic ((time ((cat adv) (lex "then")))))))))




(def-test t88
  "The position from which the SIG-display marker moves to the right."
  ((cat common)
   (head === position)
   (qualifier ((cat clause)
	       (scope {^ pred-modif origin})
	       (proc ((type composite)
		      (relation-type locative)
		      (agentive no)
		      (lex "move")))
	       (partic ((affected ((cat common) (lex "sig-display marker")))
			(located {^ affected})
			(location ((cat pp)
				   (prep === to)
				   (np ((lex "right")))))))))))


(def-test t89
  "The way that your sister is."
  ((cat common)
   (head === way)
   (qualifier ((cat clause)
	       (scope {^ partic attribute})
	       (proc ((type ascriptive)))
	       (partic ((carrier ((cat common)
				  (possessor ((cat personal-pronoun)
					      (person second)))
				  (head === "sister")))))))))




(def-test t90
  "The person with whom your sister is."
  ((cat common)
   (head === person)
   (animate yes)
   (qualifier ((cat clause)
	       (proc ((type accompaniment)))
	       (scope {^ partic location})
	       (partic ((location ((cat pp)))
			(located ((cat common)
				  (possessor ((cat personal-pronoun)
					      (person second)))
				  (head === "sister")))))))))




(def-test t91
  "The person, who has a PhD."
  ((cat common)
   (head === person)
   (animate yes)
   (qualifier ((cat clause)
	       (restrictive no)
	       (scope {^ partic possessor})
	       (proc ((type possessive)))
	       (partic ((possessed ((cat common)
				    (definite no)
				    (head === "PhD")))))))))




(def-test t92
  "The box, which she has."
  ((cat common)
   (head === box)
   (animate no)
   (qualifier ((cat clause)
	       (restrictive no)
	       (proc ((type possessive)))
	       (scope {^ partic possessed})
	       (partic ((possessor ((cat personal-pronoun)
				    (gender feminine)
				    (number singular)
				    (person third)))))))))



(def-test t93
  ;; THE RULE FOR RESTRICTIVE RELATIVE CLAUSES:
  ;; that = restrictive / which = non-restrictive
  ;; who/whom = either (not that).
  ;; Not critical
  ("The person that owns this book."
   "The person who owns this book.")
  ((cat common)
   (head === person)
   (animate yes)
   (qualifier ((cat clause)
	       (proc ((type possessive) (mode equative)))
	       (scope {^ partic possessor})
	       (partic ((possessed ((cat common)
				    (distance near)
				    (head === "book")))))))))


(def-test t93bis
  "The person by whom this book is owned."
  ((cat common)
   (head === person)
   (animate yes)
   (qualifier ((cat clause)
	       (proc ((type possessive) (mode equative)
		      (voice passive)))
	       (scope {^ partic possessor})
	       (partic ((possessed ((cat common)
				    (distance near)
				    (head === "book")))))))))


(def-test t94
  "The box that she owns."
  ((cat common)
   (head === box)
   (qualifier ((cat clause)
	       (restrictive yes)
	       (proc ((type possessive) (mode equative)))
	       (scope {^ partic possessed})
	       (partic ((possessor ((cat personal-pronoun)
				    (gender feminine)
				    (person third)
				    (number singular)))))))))


(def-test t95
  "The person, who gives a blue book to Mary."
  ((cat common)
   (head === person)
   (animate yes)
   (qualifier ((cat clause)
	       (restrictive no)
	       (scope {^ partic agent})
	       (proc ((type composite)
		      (relation-type possessive)
		      (dative-move no)
		      (dative-prep "to")
		      (lex "give")))
	       (partic ((possessed ((lex "book")
				    (cat common)
				    (definite no)
				    (describer === "blue")))
			(affected ((lex "Mary") (cat proper)))
			(possessor {^ affected})))))))


(def-test t96
  "The box, which John will give to Mary."
  ((cat common)
   (head === box)
   (qualifier ((cat clause)
	       (restrictive no)
	       (tense future)
	       (scope {^ partic possessed})
	       (proc ((type composite)
		      (relation-type possessive)
		      (dative-move no)
		      (dative-prep "to")
		      (lex "give")))
	       (partic ((agent ((lex "John") (cat proper)))
			(affected ((lex "Mary") (cat proper)))
			(possessor {^ affected})))))))


(def-test t97
  "The person to whom John will give a blue book."
  ((cat common)
   (head === person)
   (animate yes)
   (qualifier ((cat clause)
	       (tense future)
	       (scope {^ partic possessor})
	       (proc ((type composite)
		      (relation-type possessive)
		      (dative-prep "to")
		      (lex "give")))
	       (partic ((agent ((lex "John") (cat proper)))
			(possessed ((lex "book")
				    (cat common)
				    (definite no)
				    (describer === "blue")))))))))

(def-test t97bis
  "The person by whom a blue book will possibly be given to John."
  ;; With dative-move yes will get:
  ;; The person by whom John will be given a blue book.
  ;; Should force dative-move always no when displaced constituent
  ((cat common)
   (head === person)
   (animate yes)
   (qualifier ((cat clause)
	       (tense future)
	       (scope {^ partic agent})
	       (adverb === possibly)
	       (proc ((type composite)
		      (relation-type possessive)
		      (dative-move no)
		      (dative-prep "to")
		      (voice passive)
		      (lex "give")))
	       (partic ((possessor ((lex "John") (cat proper)))
			(possessed ((lex "book")
				    (cat common)
				    (definite no)
				    (describer === "blue")))))))))


(def-test t97ter
  "The person by whom a blue book will not finally be given to John."
  ;; Can't currently generate:
  ;; The person by whom John will be given a blue book.
  ;; due to treatment of dative-move always no when displaced constituent
  ;; is given.  This makes life much easier and is not such a restriction.
  ((cat common)
   (head === person)
   (animate yes)
   (qualifier ((cat clause)
	       (tense future)
	       (scope {^ partic agent})
	       (adverb === finally)
	       (polarity negative)
	       (proc ((type composite)
		      (relation-type possessive)
		      (dative-prep "to")
		      (dative-move no)
		      (voice passive)
		      (lex "give")))
	       (partic ((possessor ((lex "John") (cat proper)))
			(possessed ((lex "book")
				    (cat common)
				    (definite no)
				    (describer === "blue")))))))))


(def-test t98
  "The box that Deborah prefers."
  ((cat common)
   (head === box)
   (qualifier ((cat clause)
	       (scope {^ partic phenomenon})
	       (proc ((type mental) (lex "prefer")))
	       (partic ((processor ((cat proper)
				    (lex "Deborah")))))))))



(def-test t99
  "The person, who worships Baal."
  ((cat common)
   (head === person)
   (animate yes)
   (qualifier ((cat clause)
	       (restrictive no)
	       (scope {^ partic processor})
	       (proc ((type mental) (lex "worship")))
	       (partic ((phenomenon ((cat proper)
				     (lex "Baal")))))))))



;; Long distance dependency for relatives
(def-test t99bis
  "The person, who you think won the prize."
  ((cat common)
   (head === person)
   (animate yes)
   (qualifier
    ((cat clause)
     (restrictive no)
     (proc ((type mental)
	    (object-clause that)
	    (lex "think")))
     (scope {^ partic phenomenon partic possessor})
     (partic ((processor ((cat personal-pronoun)
			  (animate yes)
			  (person second)
			  (number singular)))
	      ;; Win interpreted as "get": Ag/Ca+Pos
	      (phenomenon ((cat clause)
			   (proc ((type composite)
				  (effective no)
				  (relation-type possessive)
				  (tense past)
				  (lex "win")))
			   (partic ((agent {^ possessor})
				    (possessed ((cat common)
						(lex "prize")))))))))))))

(def-test t99ter
  "The person by whom you think the prize was finally won."
  ((cat common)
   (head === person)
   (animate yes)
   (qualifier
    ((cat clause)
     (proc ((type mental)
	    (object-clause that)
	    (lex "think")))
     (scope {^ partic phenomenon partic possessor})
     (partic ((processor ((cat personal-pronoun)
			  (animate yes)
			  (person second)
			  (number singular)))
	      ;; Win interpreted as "get": Ag/Ca+Pos
	      (phenomenon ((cat clause)
			   (adverb === finally)
			   (proc ((type composite)
				  (effective no)
				  (voice passive)
				  (relation-type possessive)
				  (tense past)
				  (lex "win")))
			   (partic ((agent {^ possessor})
				    (possessed ((cat common)
						(lex "prize")))))))))))))


;; ======================================================================
;; test of ALL clause patterns of the new transitvity system:
;; ======================================================================
;; simple process
;; event
;; material
;; agentive non-effective: Ag


(def-test t100
  "Bo runs."
  ((cat clause)
   (proc ((type material)
	  (effective no)
	  ;;	     (agentive yes)  is default
	  (lex "run")))
   (partic ((agent ((cat proper) (lex "Bo")))))))



;; agentive dispositive: Ag + Af
(def-test t101
  "Bo drinks protein shakes."
  ((cat clause)
   (proc ((type material)
	  (lex "drink")))
   (partic ((agent ((cat proper) (lex "Bo")))
	    (affected ((cat common)
		       (number plural)
		       (definite no)
		       (classifier === "protein")
		       (head === "shake")))))))



;; agentive creative: Ag + Cr
(def-test t102
  "Bo cooks dinner."
  ((cat clause)
   (proc ((type material) (effect-type creative) (lex "cook")))
   (partic ((agent ((cat proper) (lex "Bo")))
	    (created ((cat common) (lex "dinner") (denotation meal)))))))



;; agentive with range: Ag + Rg
(def-test t103
  "Bo takes a walk."
  ((cat clause)
   (proc ((type material)
	  (effective no)
	  (event-as participant)
	  (lex "take")))
   (partic ((agent ((cat proper) (lex "Bo")))
	    (range ((cat common) (definite no) (lex "walk")))))))



(def-test t104
  "Bo climbs the mountain."
  ((cat clause)
   (proc ((type material)
	  (effective no)
	  (event-as process)
	  (lex "climb")))
   (partic ((agent ((cat proper) (lex "Bo")))
	    (range ((cat common) (lex "mountain")))))))



;; non-agentive dispositive: Af
(def-test t105
  "The window opens."
  ((cat clause)
   (proc ((type material) (agentive no) (lex "open")))
   (partic ((affected ((cat common) (lex "window")))))))



;; non-agentive creative: Cr
(def-test t106
  "The window pops."
  ((cat clause)
   (proc ((type material)
	  (agentive no)
	  (effect-type creative)
	  (lex "pop")))
   (partic ((created ((cat common) (lex "window")))))))



;; mental non-transitive: Pr
(def-test t107
  "Bo knows."
  ((cat clause)
   (proc ((type mental) (transitive no) (lex "know")))
   (partic ((processor ((cat proper) (lex "Bo")))))))



;; mental transitive: Pr + Ph
(def-test t108
  "Bo knows FUF."
  ((cat clause)
   (proc ((type mental)
	  (lex "know")))
   (partic ((processor ((cat proper) (lex "Bo")))
	    (phenomenon ((cat proper) (lex "FUF")))))))



;; verbal: still on the todo list

;; relation
;; ascriptive attributive: Ca + At
(def-test t109
  "Bo is strong."
  ((cat clause)
   (proc ((type ascriptive)
	  (mode attributive)))
   (partic ((carrier ((cat proper) (lex "Bo")))
	    (attribute ((cat ap) (lex "strong")))))))




;; ascriptive equative: Id + Ir
(def-test t110
  "Bo is the greatest."
  ((cat clause)
   (proc ((type ascriptive) (mode equative)))
   (partic ((identified ((cat proper) (lex "Bo")))
	    (identifier ((cat common) (lex "greatest")))))))



;; possessive attributive: Pr + Pd
(def-test t111
  "Bo has sneakers."
  ((cat clause)
   (proc ((type possessive)))
   (partic ((possessor ((cat proper) (lex "Bo")))
	    (possessed ((cat common)
			(number plural)
			(definite no)
			(lex "sneaker")))))))



;; possesive equative: Id + Ir
(def-test t112
  "Bo owns sneakers."
  ((cat clause)
   (proc ((type possessive) (mode equative)))
   (partic ((identified ((cat proper) (lex "Bo")))
	    (identifier ((cat common)
			 (number plural)
			 (definite no)
			 (lex "sneaker")))))))



;; locative spatial atributive: Ld + Ln
(def-test t113
  "Bo is here."
  ((cat clause)
   (proc ((type spatial)))
   (partic ((located ((cat proper) (lex "Bo")))
	    (location ((cat adv) (lex "here")))))))



;; locative spatial equative: Id + Ir
(def-test t114
  "Bo occupies the left flank."
  ((cat clause)
   (proc ((type spatial) (mode equative) (lex "occupy")))
   (partic ((located ((cat proper) (lex "Bo")))
	    (location ((cat common)
		       (classifier === "left")
		       (head === "flank")))))))



;; locative temporal attributive: Ld + Tm
(def-test t115
  "The race is now."
  ((cat clause)
   (proc ((type temporal)))
   (partic ((located ((cat common) (lex "race")))
	    (time ((cat adv) (lex "now")))))))



;; locative spatial equative: Id + Ir
(def-test t116
  "The steeplechase follows the high jump."
  ((cat clause)
   (proc ((type temporal) (mode equative) (lex "follow")))
   (partic ((identified ((cat common) (lex "steeplechase")))
	    (identifier ((cat common)
			 (classifier === "high")
			 (head === "jump")))))))



;; locative accompaniment (always equative): Ld/Id + Ac/Ir
(def-test t117
  "Bo is with Mo."
  ((cat clause)
   (proc ((type accompaniment)))
   (partic ((located ((cat proper) (lex "Bo")))
	    (location ((cat pp)
		       (prep === with)
		       (np ((cat proper) (lex "Mo")))))))))



(def-test t118
  "Bo accompanies Mo."
  ((cat clause)
   (proc ((type accompaniment) (mode equative) (lex "accompany")))
   (partic ((identified ((cat proper) (lex "Bo")))
	    (identifier ((cat proper) (lex "Mo")))))))



;; locative existential: Ld
(def-test t119
  "There is a bug."
  ((cat clause)
   (proc ((type existential)))
   (partic ((located ((cat common) (definite no) (lex "bug")))))))



;; locative natural-phenom: no participant
(def-test t120
  "It rains."
  ((cat clause) (proc ((type natural-phenom) (lex "rain")))))




;; composite process
;; agentive dispositive ascriptive attributive: Ag + Af/Ca + At
(def-test t121
  "Nike made Bo rich."
  ((cat clause)
   (tense past)
   (proc ((type composite)
	  (relation-type ascriptive)
	  ;;	     (agentive yes)         is default
	  ;;           (effective yes)        is default
	  ;;           (effect-type affected) is default
	  ;;           (mode attributive)     is default
	  (lex "make")))
   (partic ((agent ((cat proper) (lex "Nike")))
	    (affected ((cat proper) (lex "Bo")))
	    (carrier {^ affected})
	    (attribute ((cat ap) (lex "rich")))))))



;; agentive dispositive ascriptive equative: Ag + Af/Id + At
(def-test t122
  "The deal makes Bo the richest."
  ((cat clause)
   (proc ((type composite)
	  (relation-type ascriptive)
	  (mode equative)
	  (lex "make")))
   (partic ((agent ((cat common) (lex "deal")))
	    (affected ((cat proper) (lex "Bo")))
	    (identified {^ affected})
	    (identifier ((cat common) (lex "richest")))))))



;; agentive dispositive possessive attributive: Ag + Af/Pr + Pd
(def-test t123
  "Bo gave the Raiders the victory."
  ((cat clause)
   (tense past)
   (proc ((type composite) (relation-type possessive) (lex "give")))
   (partic ((agent ((cat proper) (lex "Bo")))
	    (affected ((cat proper) (lex "Raider") (number plural)))
	    (possessor {^ affected})
	    (possessed ((cat common) (lex "victory")))))))



;; agentive dispositive possessive attributive: Ag + Af/Pr + Pd
(def-test t123-bis
  "Bo gave the victory to the Raiders."
  ((cat clause)
   (tense past)
   (dative-move no)
   (proc ((type composite) (relation-type possessive) (lex "give")))
   (partic ((agent ((cat proper) (lex "Bo")))
	    (affected ((cat proper) (lex "Raider") (number plural)))
	    (possessor {^ affected})
	    (possessed ((cat common) (lex "victory")))))))



;; agentive dispositive locative attributive: Ag + Af/Ld + Ln
(def-test t124
  "Bo lifted the Raiders to a victory."
  ((cat clause)
   (tense past)
   (proc ((type composite) (relation-type locative) (lex "lift")))
   (partic ((agent ((cat proper) (lex "Bo")))
	    (affected ((cat proper) (lex "Raider") (number plural)))
	    (located {^ affected})
	    (location ((cat pp)
		       (prep ((lex "to")))
		       (np ((cat common)
			    (definite no)
			    (lex "victory")))))))))



;; agentive creative locative attributive: Ag + Cr/Ld + Ln
(def-test t125
  "It pops a window on the screen."
  ((cat clause)
   (proc ((type composite)
	  (relation-type locative)
	  (effect-type creative)
	  (lex "pop")))
   (partic ((agent ((cat personal-pronoun)
		    (person third)
		    (gender neuter)
		    (number singular)))
	    (created ((cat common) (definite no) (lex "window")))
	    (located {^ created})
	    (location ((cat pp)
		       (prep ((lex "on")))
		       (np ((cat common) (lex "screen")))))))))



;; agentive non-effective ascriptive attributive: Ag/Ca + At
(def-test t126
  "Bo got tough."
  ((cat clause)
   (tense past)
   (proc ((type composite)
	  (relation-type ascriptive)
	  (effective no)
	  (lex "get")))
   (partic ((agent ((cat proper) (lex "Bo")))
	    (carrier {^ agent})
	    (attribute ((cat ap) (lex "tough")))))))



;; agentive non-effective ascriptive equative: Ag/Id + Ir
(def-test t127
  "Bo became the best."
  ((cat clause)
   (tense past)
   (proc ((type composite)
	  (relation-type ascriptive)
	  (mode equative)
	  (effective no)
	  (lex "become")))
   (partic ((agent ((cat proper) (lex "Bo")))
	    (identified {^ agent})
	    (identifier ((cat common) (lex "best")))))))



;; agentive non-effective possessive attributive: Ag/Pr + Pd
(def-test t128
  "Bo bought sneakers."
  ((cat clause)
   (tense past)
   (proc ((type composite)
	  (relation-type possessive)
	  (effective no)
	  (lex "buy")))
   (partic ((agent ((cat proper) (lex "Bo")))
	    (possessor {^ agent})
	    (possessed ((cat common)
			(definite no)
			(number plural)
			(lex "sneaker")))))))



;; agentive non-effective locative attributive: Ag/Ld + Ln
(def-test t129
  "Bo ran home."
  ((cat clause)
   (tense past)
   (proc ((type composite)
	  (relation-type locative)
	  (effective no)
	  (lex "run")))
   (partic ((agent ((cat proper) (lex "Bo")))
	    (located {^ agent})
	    (location ((cat adv) (lex "home")))))))

(store-verbs '(("run" "runs" "ran" "running" "ran")))


;; non-agentive dispositive ascriptive attributive: Af/Ca + At
(def-test t130
  "Bo grew old."
  ((cat clause)
   (tense past)
   (proc ((type composite)
	  (agentive no)
	  (relation-type ascriptive)
	  (lex "grow")))
   (partic ((affected ((cat proper) (lex "Bo")))
	    (carrier {^ affected})
	    (attribute ((cat ap) (lex "old")))))))



;; non-agentive dispositive possessive attributive: Af/Pr + Pd
(def-test t131
  "Bo received the ball."
  ((cat clause)
   (tense past)
   (proc ((type composite)
	  (agentive no)
	  (relation-type possessive)
	  (lex "receive")))
   (partic ((affected ((cat proper) (lex "Bo")))
	    (possessor {^ affected})
	    (possessed ((cat common) (lex "ball")))))))



;; non-agentive dispositive locative attributive: Af/Ld + Ln
(def-test t132
  "Bo fell down."
  ((cat clause)
   (tense past)
   (proc ((type composite)
	  (agentive no)
	  (relation-type locative)
	  (lex "fall")))
   (partic ((affected ((cat proper) (lex "Bo")))
	    (located {^ affected})
	    (location ((cat adv) (lex "down")))))))



;; non-agentive creative asciptive attributive: Cr/Ca + At
(def-test t133
  "The window popped wide."
  ((cat clause)
   (tense past)
   (proc ((type composite)
	  (agentive no)
	  (effect-type creative)
	  (relation-type ascriptive)
	  (lex "pop")))
   (partic ((created ((cat common) (lex "window")))
	    (carrier {^ created})
	    (attribute ((cat ap) (lex "wide")))))))



;; non-agentive creative locative attributive: Cr/Ld + Ln
(def-test t134
  "The window popped on the screen."
  ((cat clause)
   (tense past)
   (proc ((type composite)
	  (agentive no)
	  (effect-type creative)
	  (relation-type locative)
	  (lex "pop")))
   (partic ((created ((cat common) (lex "window")))
	    (located {^ created})
	    (location ((cat pp)
		       (prep ((lex "on")))
		       (np ((cat common) (lex "screen")))))))))




;; ----------------------------------------------------------------------
;; TEST LEXICAL ROLES
;; ----------------------------------------------------------------------

;; PROBLEMS WITH PASSIVE... CHECK HPSG's TREATMENT OF PASSIVE
;; AS A DIFFERENT LEXICAL ITEM GENERATED BY A LEXICAL RULE...
(def-test t140
  "The customer persuaded the programmer that there is a bug."
  ((cat clause)
   (tense past)
   (proc ((type lexical)
	  (lex "persuade")
	  (subcat ((1 {^3 lex-roles persuader})
		   (2 {^3 lex-roles persuadee})
		   (3 {^3 lex-roles soa})
		   (1 ((cat np)))
		   (2 ((cat np)))
		   (3 ((cat clause)
		       (mood bound-nominal-declarative)
		       (binder === that)))))))
   (lex-roles ((persuader === customer)
	       (persuadee === programmer)
	       (soa ((proc ((type existential)))
		     (partic ((located ((cat common)
					(definite no)
					(lex "bug")))))))))))

(def-test t141
  "The customer persuaded the programmer to revise the code."
  ((cat clause)
   (tense past)
   (proc ((type lexical)
	  (lex "persuade")
	  (subcat ((1 {^3 lex-roles persuader})
		   (2 {^3 lex-roles persuadee})
		   (3 {^3 lex-roles soa})
		   (1 ((cat np)))
		   (2 ((cat np)))
		   (3 ((cat clause)
		       (mood to-infinitive)
		       (controlled {^ oblique 1})))))))
   (lex-roles
    ((persuader === customer)
     (persuadee === programmer)
     (soa ((proc ((type material)
		  (lex "revise")))
	   (partic ((affected === code)))))))))

;; With a more general approach ala HPSG/S&P91
;; Roles influence/influencer/soa
(def-test t142
  "The customer wants him to do it."
  ((cat clause)
   (tense present)
   (proc ((type lexical)
	  (lex "want")
	  (voice active)
	  (subcat ((1 {^3 lex-roles influence})
		   (2 {^3 lex-roles influenced})
		   (3 {^3 lex-roles soa})
		   (3 ((cat clause)
		       (mood to-infinitive)
		       (controlled {^ oblique 1})))))))
   (lex-roles
    ((influence === customer)
     (influenced ((cat personal-pronoun)
		  (person third)
		  (gender masculine)
		  (number singular)))
     (soa ((proc ((type material)
		  (lex "do")))
	   (partic ((affected ((cat personal-pronoun)
			       (person third)
			       (gender neuter)
			       (number singular)))))))))))

;; ------------------------------------------------------------
;; Test of determiner sequence
;; ------------------------------------------------------------

(def-test t160
  "The car."
  ((cat np)
   (definite yes)
   (lex "car")))

(def-test t161
  "The cars."
  ((cat np)
   (definite yes)
   (number plural)
   (lex "car")))

(def-test t162
  "A car."
  ((cat np)
   (definite no)
   (lex "car")))

(def-test t163
  "Cars."
  ((cat np)
   (definite no)
   (number plural)
   (lex "car")))

(def-test t164
  "Which car."
  ((cat np)
   (interrogative yes)
   (lex "car")))

(def-test t165
  "What car."
  ((cat np)
   (interrogative yes)
   (selective no)
   (lex "car")))

(def-test t166
  "Whose car."
  ((cat np)
   (interrogative yes)
   (possessive yes)
   (lex "car")))

(def-test t167
  "My car."
  ((cat np)
   (lex "car")
   (possessor ((cat personal-pronoun)
	       (person first)))))

(def-test t168
  "John's car."
  ((cat np)
   (lex "car")
   (possessor ((cat proper)
	       (lex "John")))))

(def-test t169
  "The man's cars."
  ((cat np)
   (number plural)
   (lex "car")
   (possessor ((cat np)
	       (definite yes)
	       (lex "man")))))

(def-test t170
  "Which man's car."
  ((cat np)
   (interrogative yes)
   (lex "car")
   (possessor ((cat np)
	       (lex "man")))))


(def-test t171
  "This car."
  ((cat common)
   (distance near)
   (lex "car")))

(def-test t172
  "That car."
  ((cat common)
   (distance far)
   (lex "car")))


;; Test quantifier det

(def-test t173
  "All cars."
  ((cat common)
   (number plural)
   (definite no)
   (total +)
   (lex "car")))

(def-test t174
  "No car."
  ((cat common)
   (number singular)
   (total -)
   (lex "car")))

(def-test t175
  "No cars."
  ((cat common)
   (number plural)
   (total -)
   (lex "car")))

(def-test t176
  ("Each car."
   "Every car.")
  ((cat common)
   (lex "car")
   (number singular)
   (total +)))

(def-test t177
  "Both cars."
  ((cat common)
   (lex "car")
   (definite no)
   (number dual)
   (total +)))

(def-test t178
  "Neither car."
  ((cat common)
   (lex "car")
   (reference-number dual)
   (total -)))

(def-test t179
  "One car."
  ((cat common)
   (lex "car")
   (total none)
   (selective yes)
   (number singular)))

(def-test t180
  "Either car."
  ((cat common)
   (lex "car")
   (total none)
   (reference-number dual)
   (selective yes)))

(def-test t181
  "Some cars."
  ((cat common)
   (lex "car")
   (number plural)
   (total none)
   (selective yes)))

(def-test t182
  "Two-thirds of the cars."
  ((cat common)
   (lex "car")
   (fraction ((num 2)
	      (den 3)))
   (definite yes)
   (number plural)))

(def-test t183
  "2/3 of the car."
  ((cat common)
   (lex "car")
   (fraction ((num 2) (den 3) (digit yes)))
   (definite yes)))

(def-test t184
  "Three times the butter."
  ((cat common)
   (lex "butter")
   (countable no)
   (multiplier ((value 3) (digit no)))
   (definite yes)))

(def-test t185
  ("Twice his salary."
   "Double his salary.")
  ((cat common)
   (lex "salary")
   (denotation quantity)
   (multiplier ((value 2)))
   (possessor ((cat personal-pronoun)
	       (person third)
	       (gender masculine)))))

(def-test t186
  "Two-fifths of the 10 cars."
  ((cat common)
   (lex "car")
   (cardinal ((value 10) (digit yes)))
   (fraction ((num 2) (den 5)))
   (definite yes)))

(def-test t186bis
  "Two-fifths of the at most 10 cars."
  ((cat common)
   (lex "car")
   (cardinal ((value 10) (digit yes)
              (comparative bound)
              (orientation -)))
   (fraction ((num 2) (den 5)))
   (definite yes)))

(def-test t186ter
  "Two-fifths of the no more than 10 cars."
  ((cat common)
   (lex "car")
   (cardinal ((value 10) (digit yes)
              (comparative comparative)
              (adverb ((lex "no")))
              (orientation +)))
   (fraction ((num 2) (den 5)))
   (definite yes)))

(def-test t186quad
  "Two-fifths of over 10 cars."
  ((cat common)
   (lex "car")
   (cardinal ((value 10) (digit yes)
              (comparative comparative)
              (adverb ((lex "no")))
              (orientation +)))
   (fraction ((num 2) (den 5)))
   (definite yes)))

(def-test t187
  "He cooks breakfast."
  ((cat clause)
   (proc ((type material) (effect-type creative) (lex "cook")))
   (partic ((agent ((cat personal-pronoun) (gender masculine)))
	    (created ((cat common)
		      (lex "breakfast")
		      (denotation meal)
		      (definite yes)))))))

(def-test t188
  "He is eating dinner."
  ((cat clause)
   (proc ((type material) (lex "eat")))
   (tense present-progressive)
   (partic ((agent ((cat personal-pronoun) (gender masculine)))
	    (affected ((cat common)
		       (lex "dinner")
		       (denotation meal)))))))

(def-test t189
  "He is at school."
  ((cat clause)
   (proc ((type locative)))
   (partic ((located ((cat personal-pronoun) (gender masculine)))
	    (location ((cat pp) (prep ((lex "at")))
		       (np ((lex "school")
			    (cat common)
			    (denotation institution)))))))))

(store-verbs '(("build" "builds" "built" "building" "built")))

(def-test t190
  "He built the school."
  ;; Note this is not an institution but a building -> use the
  ((cat clause)
   (proc ((type material) (effect-type creative) (lex "build")))
   (tense past)
   (partic ((agent ((cat personal-pronoun) (gender masculine)))
	    (created ((cat common)
		      (lex "school")
		      (definite yes)))))))

(def-test t191
  "All of me."
  ((cat personal-pronoun)
   (reference-number singular)
   (total +)
   (partitive yes)
   (person first)))

(def-test t192
  "You both."
  ((cat personal-pronoun)
   (total +)
   (person second)
   (number dual)))


(def-test t193
  ("The many cars."
   "The several cars.")
  ((cat common)
   (lex "car")
   (exact no)
   (number plural)
   (definite yes)
   (degree +)
   (orientation +)))

(def-test t194
  "The few cars."
  ((cat common)
   (lex "car")
   (exact no)
   (definite yes)
   (number plural)
   (orientation -)))

(def-test t195
  ("A great many cars."
   "A good many cars."
   "Several cars."
   "Plenty of cars."
   "Lots of cars."
   "A lot of cars."
   "Many cars.")
  ((cat common)
   (lex "car")
   (definite no)
   (number plural)
   (degree +)
   (orientation +)))

(def-test t196
  "The more cars."
  ((cat common)
   (lex "car")
   (definite yes)
   (number plural)
   (orientation +)
   (comparative yes)))

(def-test t197
  "The most cars."
  ((cat common)
   (lex "car")
   (definite yes)
   (number plural)
   (orientation +)
   (superlative yes)))

(def-test t198
  "Enough cars."
  ((cat common)
   (lex "car")
   (definite no)
   (number plural)
   (evaluative yes)
   (evaluation +)
   (orientation +)))

(def-test t199
  "Too many cars."
  ((cat common)
   (lex "car")
   (definite no)
   (number plural)
   (evaluative yes)
   (evaluation -)
   (orientation +)))

(def-test t200
  "Some butter."
  ((cat common)
   (countable no)
   (lex "butter")
   (definite no)
   (orientation +)))

(def-test t200bis
  ("A lot of butter."
   "Lots of butter."
   "Much butter."
   "Plenty of butter.")
  ((cat common)
   (countable no)
   (lex "butter")
   (definite no)
   (degree +)
   (orientation +)))

(def-test t200ter
  "A little butter."
  ((cat common)
   (countable no)
   (lex "butter")
   (definite no)
   (degree -)
   (orientation +)))

(def-test t201
  "John."
  ((cat proper)
   (lex "John")))

(def-test t202
  "The Johns."
  ((cat proper)
   (number plural)
   (lex "John")))

(def-test t203
  "He."
  ((cat personal-pronoun)
   (person third)
   (number singular)
   (gender masculine)))

(def-test t204
  "They."
  ((cat personal-pronoun)
   (person third)
   (number plural)
   (gender masculine)))

(def-test t205
  "Enough of him."
  ((cat personal-pronoun)
   (countable no)
   (person third)
   (gender masculine)
   (evaluative yes)))

(def-test t206
  ("The same two individuals."
   "The identical two individuals.")
  ((cat common)
   (lex "individual")
   (cardinal ((value 2) (digit no)))
   (status same)))


(def-test t207
  ("The other two individuals."
   "The different two individuals.")
  ((cat common)
   (lex "individual")
   (cardinal ((value 2) (digit no)))
   (status different)))

(def-test t208
  ("The above chapter."
   "The aforementioned chapter."
   "The given chapter.")
  ((cat common)
   (lex "chapter")
   (status mentioned)))

(def-test t209
  ("This particular spice."
   "This certain spice."
   "This specific spice."
   "This original spice."
   "This special spice.")
  ((cat common)
   (lex "spice")
   (distance near)
   (status specific)))

(def-test t210
  ("His usual walk."
   "His well-known walk."
   "His normal walk."
   "His typical walk."
   "His habitual walk."
   "His expected walk.")
  ((cat common)
   (lex "walk")
   (possessor ((cat personal-pronoun)
	       (person third)
	       (gender masculine)))
   (status usual)))

(def-test t211
  ("His entire life."
   "His whole life."
   "His complete life.")
  ((cat common)
   (lex "life")
   (possessor ((cat personal-pronoun)
	       (person third)
	       (gender masculine)))
   (status entire)))

(def-test t212
  "What."
  ((cat question-pronoun)))


;; Verb AND subject ellipsis.
(def-test t213
  "This DLC refinement activates CSA 4703 in 1992Q1 and CSA 4704 in 1992Q2."
  ((cat clause)
   (complex conjunction)
   (distinct
    ~(((process ((type material) (lex "activate")))
       (partic ((agent ((cat common)
			(semantics ((index ((concept DLC-R)))))
			(classifier ((lex "DLC")))
			(lex "refinement")
			(animate no)
			(distance near)
			(number singular)))
		(affected ((cat proper)
			   (semantics ((index ((concept CSA4703)))))
			   (lex "CSA 4703")))))
       (circum ((time ((cat pp)
		       (prep ((lex "in")))
		       (np ((cat date) (year ((value "1992Q1"))))))))))
      ((process ((type material) (lex "activate")))
       (partic ((agent ((cat common)
			(semantics ((index ((concept DLC-R)))))
			(classifier ((lex "DLC")))
			(lex "refinement")
			(animate no)
			(distance near)
			(number singular)))
		(affected ((cat proper)
			   (semantics ((index ((concept CSA4704)))))
			   (lex "CSA 4704")))))
       (circum ((time ((cat pp)
		       (prep ((lex "in")))
		       (np ((cat date) (year ((value "1992Q2"))))))))))))))


;; Problem of circum on conjunction
(def-test mine3a
 "In 1986, you filed late."
  ((cat clause)
   (circum ((time ((cat pp)
		   (prep ((lex "in")))
		   (position front)
		   (np ((cat date) (year ((value "1986")))))))))
   (proc ((type material) (lex "file") (tense past) (effective no)))
   (partic ((agent ((cat personal-pronoun)
                    (semantics ((index ((concept James)))))
                    (person second)))))
   (pred-modif ((time ((cat adv) (lex "late")))))))

(def-test mine3
 "In 1986, you filed late and failed to file an extension."
  ((cat clause)
   (complex conjunction)
   (circum ((time ((cat pp)
		   (prep ((lex "in")))
		   (position front)
		   (np ((cat date) (year ((value "1986")))))))))
   (distinct
    ~(((proc ((type material) (lex "file") (tense past) (effective no)))
       (partic ((agent ((cat personal-pronoun)
			(semantics ((index ((concept James)))))
			(person second)))))
       (circum ((time ((cat adv) (lex "late"))))))
      ((proc ((lex "fail") (type material) (tense past) (effective no)))
       (partic ((agent ((cat personal-pronoun)
			(semantics ((index ((concept James)))))
			(person second)))))
       (circum ((purpose ((cat clause)
			  (binder none)
			  (position end)
			  (proc ((lex "file")
				 (effect-type creative)
				 (type material)))
			  (partic ((created ((lex "extension")
					     (definite no)
					     (cat common))))))))))))))



(def-test t214
  "Migraine abortive treatment is used to abort migraine."
  ((cat clause)
   (process ((lex "use")
	     (type material)))
   (partic  ((affected
	      ((cat proper)
	       (lex "migraine abortive treatment")))
	     (agent none)))
   (circum  ((purpose ((cat clause)
		       (binder none)
		       (position end)
		       (process ((lex "abort")
				 (effect-type creative)
				 (type material)))
		       (partic ((created ((lex "migraine")
					  (countable no)
					  (cat common)))))))))))


(def-test t215
  "Migraine abortive treatment requires you to take a drug at the immediate onset of headaches."
  ((cat clause)
   (process ((lex "require")
	     (subcat ((1 {^3 lex-roles influence})
		      (2 {^3 lex-roles influenced})
		      (3 {^3 lex-roles soa})
		      (1 ((cat np)))
		      (2 ((cat np)))
		      (3 ((cat clause)
			  (mood to-infinitive)
			  (controlled {^ oblique 1})))))
	     (type lexical)))
   (lex-roles ((influence
		((lex "migraine abortive treatment") (cat proper)))
	       (influenced ((cat personal-pronoun) (person second)))
	       (soa ((circum ((time ((cat pp)
				     (prep ((lex "at")))
				     (np ((cat np)
					  (describer ((lex "immediate")))
					  (lex "onset")
					  (qualifier ((cat pp)
						      (np ((lex "headache")
							   (definite no)
							   (number plural)))))
					  (definite yes)))))))
		     (cat clause)
		     (process-type material)
		     (process ((lex "take")))
		     (partic ((affected ((lex "drug") (definite no)
					 (cat common)))))))))))

(def-test t216
  "Logic makes AI difficult."
  ((cat clause)
   (process ((type composite)
	     (relation-type ascriptive)))
   (participants ((carrier ((cat basic-proper) (lex "AI")))
		  (attribute ((cat ap) (lex "difficult")))
		  (affected {^ carrier})
		  (agent ((cat basic-proper) (lex "Logic")))))))

(def-test t217
  "This DLC refinement activated ALL-DLC for CSA 2119 in 1992Q1."
  ((cat clause)
   (mess-attrs ((message-class refinement)
		(admin ((LEIS-message-name RDA)
			(SEQ-NUM 3)
			(runid dcl_2104)
			(prev-runid no_dcl)
			(saved yes)))
		(refinement-type dlc)
		(action activation)
		(equipment-type ALL-DLC)
		(csa-site 2119)
		(date ((year 1992)
		       (quarter 1)))))
   (process ((semr {^2 mess-attrs action})
	     (type material)
	     (tense past)
	     (lex "activate")))
   (partic ((agent ((semr {^3 mess-attrs message-class})
		    (distance near)
		    (lex "refinement")
		    (classifier ((semr {^4 mess-attrs refinement-type})
				 (lex "DLC")))))
	    (affected ((semr {^3 mess-attrs equipment-type})
		       (cat proper)
		       (lex "ALL-DLC")))))
   (circum ((behalf ((cat pp)
		     (np ((cat basic-proper)
			  (lex "CSA 2119")))))
	    (time ((cat pp)
		   (position end)
		   (prep ((lex "in")))
		   (np ((cat basic-proper)
			(lex "1992Q1")))))))))

(def-test t218
  "This DLC refinement demanded ALL-DLC to be activated for CSA 2119 in 1992Q1."
  ((cat clause)
   (mess-attrs ((message-class refinement)
		(admin ((LEIS-message-name RDA)
			(SEQ-NUM 3)
			(runid dcl_2104)
			(prev-runid no_dcl)
			(saved yes)))
		(refinement-type dlc)
		(action activation)
		(equipment-type ALL-DLC)
		(csa-site 2119)
		(date ((year 1992)
		       (quarter 1)))))
   (process
    ((type lexical)
     (lex "demand")
     (tense past)
     (subcat ((1 {^3 lex-roles influence})
	      (2 {^3 lex-roles influenced})
	      (3 {^3 lex-roles soa})
	      (3 ((cat clause)
		  (mood to-infinitive)
		  (controlled {^ synt-roles subject})))))))
   (lex-roles
    ((influence ((semr {^3 mess-attrs message-class})
		 (distance near)
		 (lex "refinement")
		 (classifier ((semr {^4 mess-attrs refinement-type})
			      (lex "DLC")))))
     (influenced ((semr {^3 mess-attrs equipment-type})
		  (index ((concept all-dlc1)))
		  (cat proper)
		  (lex "ALL-DLC")))
     (soa ((process ((semr {^2 mess-attrs action})
		     (type material)
		     (tense past)
		     (voice passive)
		     (lex "activate")))
	   (partic ((affected ((index {^4 influenced index})
			       (lex "ALL-DLC")
			       (cat common)))))))))
   (circum ((behalf ((cat pp)
		     (np ((cat basic-proper)
			  (lex "CSA 2119")))))
	    (time ((cat pp)
		   (position end)
		   (prep ((lex "in")))
		   (np ((cat basic-proper)
			(lex "1992Q1")))))))))


(def-test t219
  "This DLC refinement demanded that ALL-DLC be activated for CSA 2119 in 1992Q1."
  ((cat clause)
   (mess-attrs ((message-class refinement)
		(admin ((LEIS-message-name RDA)
			(SEQ-NUM 3)
			(runid dcl_2104)
			(prev-runid no_dcl)
			(saved yes)))
		(refinement-type dlc)
		(action activation)
		(equipment-type ALL-DLC)
		(csa-site 2119)
		(date ((year 1992)
		       (quarter 1)))))
   (process
    ((type lexical)
     (lex "demand")
     (tense past)
     (subcat ((1 {^3 lex-roles influence})
	      (2 {^3 lex-roles soa})
	      (2 ((cat clause)
		  (mood bound-nominal-subjunctive)))))))
   (lex-roles
    ((influence ((semr {^3 mess-attrs message-class})
		 (distance near)
		 (lex "refinement")
		 (classifier ((semr {^4 mess-attrs refinement-type})
			      (lex "DLC")))))
     (soa ((process ((semr {^2 mess-attrs action})
		     (type material)
		     (tense past)
		     (voice passive)
		     (lex "activate")))
	   (partic ((affected ((semr {^5 mess-attrs equipment-type})
			       (lex "ALL-DLC")
			       (cat proper)))))))))
   (circum ((behalf ((cat pp)
		     (np ((cat basic-proper)
			  (lex "CSA 2119")))))
	    (time ((cat pp)
		   (position end)
		   (prep ((lex "in")))
		   (np ((cat basic-proper)
			(lex "1992Q1")))))))))



;; ============================================================
;; NEW EXPANDED NP GRAMMAR (Jacques Robin Aug 92)
;; ============================================================

(def-test t300
  "52 points."
  ((cat measure)
   (quantity ((value 52)))
   (unit ((lex "point")))))

(def-test t301
  "Five rebounds."
  ((cat measure)
   (quantity ((value 5) (digit no)))
   (unit ((lex "rebound")))))

(def-test t302
  "5 blocked shots."
  ((cat measure)
   (quantity ((value 5) (digit yes)))
   (unit ((lex "blocked shot")))))

(def-test t303
  "Six 3 point shots."
  ((cat common)
   (cardinal ((value 6) (digit no)))
   (definite no)
   (classifier ((cat measure)
		(quantity ((value 3) (digit yes)))
		(unit ((lex "point")))))
   (head ((lex "shot")))))

(def-test t304
  "Season high."
  ((cat noun-compound)
   (classifier ((lex "season")))
   (head ((lex "high")))))

(def-test t305
  "The pleasant house property tax office furniture."
  ((cat common)
   (describer ((lex "pleasant")))
   (classifier ((cat noun-compound)
		(classifier ((cat noun-compound)
			     (classifier ((cat noun-compound)
					  (classifier ((lex "house")))
					  (head ((lex "property")))))
			     (head ((lex "tax")))))
		(head ((lex "office")))))
   (head ((lex "furniture")))))

(def-test t306
  "A season high 27 points."
  ((cat common)
   (definite no)
   (classifier ((cat noun-compound)
		(classifier ((lex "season")))
		(head ((lex "high")))))
   (head ((cat measure)
	  (number plural)
	  (quantity ((value 27)))
	  (unit ((lex "point")))))))

(def-test t307
  "Stockton's season high 27 points."
  ((cat common)
   (possessor ((cat basic-proper)
	       (lex "Stockton")))
   (classifier ((cat noun-compound)
		(classifier ((lex "season")))
		(head ((lex "high")))))
   (head ((cat measure)
	  (quantity ((value 27)))
	  (unit ((lex "point")))))))

(def-test t308
  "Fourth quarter."
  ((cat measure)
   (quantity ((cat ordinal)
	      (value 4) (digit no)))
   (unit ((lex "quarter")))))

(def-test t309
  "17 fourth quarter points."
  ((cat measure)
   (quantity ((value 17)))
   (unit ((cat noun-compound)
	  (classifier ((cat measure)
		       (quantity ((cat ordinal)
				  (value 4) (digit no)))
		       (unit ((lex "quarter")))))
	  (head ((lex "point")))))))

(def-test t310
  "A playoff record six fourth quarter three pointers."
  ((cat common)
   (definite no)
   (classifier ((cat noun-compound)
		(classifier ((lex "playoff")))
		(head ((lex "record")))))
   (head ((cat measure)
	  (quantity ((value 6) (digit no)))
	  (unit ((cat noun-compound)
		 (classifier ((cat measure)
			      (quantity ((cat ordinal) (value 4) (digit no)))
			      (unit ((lex "quarter")))))
		 (head ((lex "three pointer")))))))))

(def-test t311
  "The Kings's NBA record 35th straight road loss."
  ((cat common)
   (possessor ((cat basic-proper)
	       (lex "the Kings")))
   (classifier ((cat noun-compound)
		(classifier ((lex "NBA")))
		(head ((lex "record")))))
   (head ((cat measure)
	  (quantity ((cat ordinal) (value 35)))
	  (unit ((cat noun-compound)
		 (classifier ((cat list)
			      (distinct ~(((cat adj)
					   (lex "straight"))
					  ((cat noun)
					   (lex "road"))))))
		 (head ((lex "loss")))))))))

(def-test t312
  "The Hawks, winners of six straight games and five in a row at the Omni."
  ((cat np)
   (complex apposition)
   (distinct
    ~(((cat basic-proper)
       (number plural)
       (lex  "Hawk"))
      ((cat common)
       (definite no)
       (number plural)
       (head ((lex "winner")))
       (qualifier
	((cat pp)
	 (prep ((lex "of")))
	 (np ((cat np)
	      (complex conjunction)
	      (distinct
	       ~(((cat common)
		  (definite no)
		  (cardinal ((value 6) (digit no)))
		  (classifier ((cat adj) (lex "straight")))
		  (head ((lex "game"))))
		 ((cat common)
		  (definite no)
		  (cardinal ((value 5) (digit no)))
		  (head ((gap yes)))
		  (qualifier ((cat list)
			      (distinct
			       ~(((cat pp)
				  (prep ((lex "in")))
				  (np ((cat common)
				       (definite no)
				       (head ((lex "row"))))))
				 ((cat pp)
				  (prep ((lex "at")))
				  (np ((cat common)
				       (lex "Omni"))))))))))))))))))))

(def-test t313
  "13 of 14 shots."
  ((cat partitive)
   (part ((value 13)))
   (part-of ((cat common)
	     (definite no)
	     (cardinal ((value 14)))
	     (head ((lex "shot")))))))

(def-test t314
  "13 of 14 shots."
  ((cat partitive)
   (part ((value 13)))
   (part-of ((cat measure)
	     (quantity ((value 14)))
	     (unit ((lex "shot")))))))

(store-plurals '(("percent" "percent")))

(def-test t315
  "60.5 percent of their field goal attempts."
  ((cat partitive)
   (part ((cat measure)
	  (quantity ((value 60.5)))
	  (unit ((lex "percent")))))
   (part-of ((cat common)
	     (possessor ((cat personal-pronoun)
			 (person third)
			 (number plural)))
	     (classifier ((lex "field goal")))
	     (number plural)
	     (head ((lex "attempt")))))))

(def-test t316
  "60.5 percent of their field goal attempts."
  ((cat partitive)
   (part ((cat common)
	  (definite no)
	  (cardinal ((value 60.5)))
	  (head ((lex "percent")))))
   (part-of ((cat common)
	     (possessor ((cat personal-pronoun)
			 (person third)
			 (number plural)))
	     (classifier ((lex "field goal")))
	     (number plural)
	     (head ((lex "attempt")))))))

(def-test t317
  "His NBA season high."
  ((cat common)
   (possessor ((cat personal-pronoun)
	       (person third)
	       (gender masculine)
	       (number singular)))
   (classifier ((lex "NBA")))
   (head ((cat noun-compound)
	  (classifier ((lex "season")))
	  (head ((lex "high")))))))

(def-test t318
  "His NBA season high performance of 52 points."
  ((cat common)
   (possessor ((cat personal-pronoun)
	       (person third)
	       (gender masculine)
	       (number singular)))
   (classifier ((cat noun-compound)
		(classifier ((lex "NBA")))
		(head ((cat noun-compound)
		       (classifier ((lex "season")))
		       (head ((lex "high")))))))
   (head ((lex "performance")))
   (qualifier ((cat pp)
	       (prep ((lex "of")))
	       (np ((cat common)
		    (definite no)
		    (cardinal ((value 52)))
		    (head ((lex "point")))))))))

(def-test t319
  "His NBA season high performance of 52 points."
  ((cat partitive)
   (part ((cat common)
	  (possessor ((cat personal-pronoun)
		      (person third)
		      (gender masculine)
		      (number singular)))
	  (classifier ((cat noun-compound)
		       (classifier ((lex "NBA")))
		       (head ((cat noun-compound)
			      (classifier ((lex "season")))
			      (head ((lex "high")))))))
	  (head ((lex "performance")))))
   (part-of ((cat measure)
	     (quantity ((value 52)))
	     (unit ((lex "point")))))))

(def-test t320
  "6 of an NBA team record 22 blocked shots."
  ((cat partitive)
   (part ((value 6) (digit yes)))
   (part-of ((cat common)
	     (definite no)
	     (classifier ((cat noun-compound)
			  (classifier ((lex "NBA") (a-an an)))
			  (head ((cat noun-compound)
				 (classifier ((lex "team")))
				 (head ((lex "record")))))))
	     (head ((cat measure)
		    (quantity ((value 22)))
		    (unit ((lex "blocked shot")))))))))

(def-test t321
  "A team record 47 of 53 free throws."
  ((cat common)
   (definite no)
   (classifier ((cat noun-compound)
		(classifier ((lex "team")))
		(head ((lex "record")))))
   (head ((cat partitive)
	  (part ((value 47)))
	  (part-of ((cat measure)
		    (quantity ((value 53)))
		    (unit ((lex "free throw")))))))))

(def-test t322
  "1 percent lowfat."
  ((cat ap)
   (classifier ((cat measure)
		(quantity ((value 1) (digit yes)))
		(unit ((lex "percent")))))
   (head ((lex "lowfat")))))

(def-test t323
  "Two heaping soup spoons of grade A 1 percent lowfat pasteurized milk."
  ((cat partitive)
   (part ((cat measure)
	  (quantity ((value 2) (digit no)))
	  (unit ((cat noun-compound)
		 (classifier ((cat verb)
			      (ending present-participle)
			      (lex "heap")))
		 (head ((cat noun-compound)
			(classifier ((lex "soup")))
			(head ((lex "spoon")))))))))
   (part-of ((cat common)
	     (countable no)
	     (definite no)
	     (describer ((cat list)
			 (distinct ~(((cat basic-proper)
				      (lex "grade A"))
				     ((cat ap)
				      (classifier ((cat measure)
						   (quantity ((value 1)
							      (digit yes)))
						   (unit ((lex "percent")))))
				      (head ((lex "lowfat"))))
				     ((cat verb)
				      (ending past-participle)
				      (lex "pasteurize"))))))
	     (head ((lex "milk")))))))

(def-test t324
  "All 28 of its free throws."
  ((cat partitive)
   (total +)
   (part ((value 28)))
   (part-of ((cat common)
	     (possessor ((cat personal-pronoun)
			 (person third)
			 (number singular)
			 (gender neuter)))
	     (head ((lex "free throw")))))))

(def-test t325a
  "3 point range."
  ((cat common)
   (determiner none)
   (classifier ((cat measure)
		(quantity ((value 3)
			   (digit  yes)))
		(unit ((lex "point")))))
   (head ((lex "range")))))

(def-test t325
  "Five of five from 3 point range."
  ((cat partitive)
   (part ((value 5) (digit no)))
   (part-of ((cat common)
	     (definite no)
	     (cardinal ((value 5) (digit no)))
	     (head ((gap yes)))
	     (qualifier ((cat pp)
			 (prep ((lex "from")))
			 (np ((cat common)
			      (determiner none)
			      (classifier ((cat measure)
					   (quantity ((value 3)
						      (digit  yes)))
					   (unit ((lex "point")))))
			      (head ((lex "range")))))))))))

(def-test t326
  "A perfect 12 for 12 from the line."
  ((cat common)
   (definite no)
   (describer ((cat adj) (lex "perfect")))
   (head ((cat partitive)
	  (prep ((lex "for")))
	  (part ((value 12)))
	  (part-of ((cat common)
		    (definite no)
		    (cardinal ((value 12)))
		    (head ((gap yes)))
		    (qualifier ((cat pp)
				(prep ((lex "from")))
				(np ((cat common)
				     (head ((lex "line")))))))))))))

(def-test t327
  "Stockton scored 27 points."
  ((cat clause)
   (tense past)
   (process ((type material)
	     (effect-type creative)
	     (lex "score")))
   (partic ((agent ((cat basic-proper)
		    (lex "Stockton")))
	    (created ((cat measure)
		      (quantity ((value 27)))
		      (unit ((lex "point")))))))))


(def-test t328
  "Stockton scored a season high 27 points."
  ((cat clause)
   (tense past)
   (process ((type material)
	     (effect-type creative)
	     (lex "score")))
   (partic ((agent ((cat basic-proper)
		    (lex "Stockton")))
	    (created ((cat common)
		      (definite no)
		      (classifier ((cat noun-compound)
				   (classifier ((lex "season")))
				   (head ((lex "high")))))
		      (head ((cat measure)
			     (quantity ((value 27)))
			     (unit ((lex "point")))))))))))


(def-test t329
  "Ewing scored 25 of his 29 points in the first half."
  ((cat clause)
   (tense past)
   (process ((type material)
	     (effect-type creative)
	     (lex "score")))
   (partic ((agent ((cat basic-proper)
		    (lex "Ewing")))
	    (created ((cat partitive)
		      (part ((value 25)))
		      (part-of ((cat common)
				(possessor ((cat personal-pronoun)
					    (person third)
					    (gender masculine)
					    (number singular)))
				(head ((cat measure)
				       (quantity ((value 29)))
				       (unit ((lex "point")))))))))))
   (circum ((in-loc ((cat common)
		     (ordinal ((value 1) (digit no)))
		     (head ((lex "half")))))))))

#+ignore(def-test t330
  "Six assignments is a lot."
  ((cat clause)
   (process ((type ascriptive) (mode equative)))
   (partic ((identified ((cat measure)
			 (number singular)
			 (quantity ((value 6) (digit no)))
			 (unit ((lex "assignment")))))
	    (identifier ((cat phrase) (lex "a lot")))))))

(def-test t331
  "Johnson."
  ((cat person-name)
   (last-name ((lex "Johnson")))))

(def-test t332
  "Earvin."
  ((cat person-name)
   (first-name ((lex "Earvin")))))

(def-test t333
  "Magic."
  ((cat person-name)
   (nickname ((lex "Magic")))))

(def-test t334
  "Earvin Johnson."
  ((cat person-name)
   (first-name ((lex "Earvin")))
   (last-name ((lex "Johnson")))))

(def-test t335
  "Magic Johnson."
  ((cat person-name)
   (nickname ((lex "Magic")))
   (last-name ((lex "Johnson")))))

(def-test t336
  "Earvin Magic Johnson."
  ((cat person-name)
   (first-name ((lex "Earvin")))
   (nickname ((lex "Magic")))
   (last-name ((lex "Johnson")))))

(def-test t337
  "Rufus T. Firefly."
  ((cat person-name)
   (first-name ((lex "Rufus")))
   (middle-name ((lex "T.")))
   (last-name ((lex "Firefly")))))

(def-test t338
  "Ramses II."
  ((cat person-name)
   (first-name ((lex "Ramses")))
   (dynasty ((cat ordinal) (value 2)))))

(def-test t339
  "Earvin Johnson Sr."
  ((cat person-name)
   (dynasty ((father yes)))
   (first-name ((lex "Earvin")))
   (last-name ((lex "Johnson")))))

(def-test t340
  "Earvin Johnson Jr."
  ((cat person-name)
   (dynasty ((father no)))
   (first-name ((lex "Earvin")))
   (last-name ((lex "Johnson")))))

(def-test t341
  "James Baker III."
  ((cat person-name)
   (first-name ((lex "James")))
   (last-name ((lex "Baker")))
   (dynasty ((cat ordinal) (value 3)))))

(def-test t342
  "Hugo Z. Hackenbush XXIII."
  ((cat person-name)
   (first-name ((lex "Hugo")))
   (middle-name ((lex "Z.")))
   (dynasty ((cat ordinal) (value 23)))
   (last-name ((lex "Hackenbush")))))

(def-test t343
  "Dr. Elhadad."
  ((cat person-name)
   (title ((lex "Dr.")))
   (last-name ((lex "Elhadad")))))

(def-test t344
  "Mr. Bulent Fishkin."
  ((cat person-name)
   (title ((lex "Mr.")))
   (first-name ((lex "Bulent")))
   (last-name ((lex "Fishkin")))))

(def-test t345
  "Dr. Rufus T. Firefly."
  ((cat person-name)
   (title ((lex "Dr.")))
   (first-name ((lex "Rufus")))
   (middle-name ((lex "T.")))
   (last-name ((lex "Firefly")))))

(def-test t346
  "Prof. Hugo Z. Hackenbush XXIII."
  ((cat person-name)
   (title ((lex "Prof.")))
   (first-name ((lex "Hugo")))
   (middle-name ((lex "Z.")))
   (dynasty ((cat ordinal) (value 23)))
   (last-name ((lex "Hackenbush")))))

(def-test t347a
  "Doctor Marshall President."
  ((cat list)
   (distinct ~(((lex "Doctor") (cat noun))
	       ((lex "Marshall") (cat noun))
	       ((lex "President") (cat noun))))))

(def-test t347
  "Doctor Marshall President Idi Amin Dada."
  ((cat person-name)
   (title   ((cat list)
	     (distinct ~(((lex "Doctor") (cat noun))
			 ((lex "Marshall") (cat noun))
			 ((lex "President") (cat noun))))))
   (first-name ((lex "Idi")))
   (middle-name ((lex "Amin")))
   (last-name ((lex "Dada")))))

(def-test t348
  "Dr. Rufus T. Firefly and Dr. Hugo Z. Hackenbush."
  ((cat np)
   (complex conjunction)
   (distinct ~(((cat compound-proper)
		(head ((cat person-name)
		       (title ((lex "Dr.")))
		       (first-name ((lex "Rufus")))
		       (middle-name ((lex "T.")))
		       (last-name ((lex "Firefly"))))))
	       ((cat compound-proper)
		(head ((cat person-name)
		       (title ((lex "Dr.")))
		       (first-name ((lex "Hugo")))
		       (middle-name ((lex "Z.")))
		       (last-name ((lex "Hackenbush"))))))))))

(def-test t349
  "Mr. and Ms. Fishkin."
  ((cat np)
   (complex conjunction)
   (distinct ~(((cat compound-proper)
		(head ((cat person-name)
		       (title ((lex "Mr.")))
		       (last-name ((gap yes))))))
	       ((cat compound-proper)
		(head ((cat person-name)
		       (title ((lex "Ms.")))
		       (last-name ((lex "Fishkin"))))))))))


(def-test t350
  "The great Serigne Suleyman Abdul Aziz Seck."
  ((cat compound-proper)
   (describer ((lex "great")))
   (head ((cat person-name)
	  (title ((lex "Serigne")))
	  (first-name ((lex "Suleyman")))
	  (middle-name ((lex "Abdul Aziz")))
	  (last-name ((lex "Seck")))))))

(def-test t351
  "Denver."
  ((cat team-name)
   (home ((lex "Denver")))))

(def-test t351b
  "Denver."
  ((cat compound-proper)
   (head ((cat team-name)
	  (home ((lex "Denver")))))))

(def-test t351c
  "Denver."
  ((cat basic-proper)
   (lex "Denver")))

(def-test t352
  "The Nuggets."
  ((cat compound-proper)
   (number plural)
   (head ((cat team-name)
	  (franchise ((lex "Nugget")))))))

(def-test t352b
  "The Nuggets."
  ((cat basic-proper)
   (number plural)
   (lex "Nugget")))

(def-test t353
  "The Denver Nuggets."
  ((cat compound-proper)
   (number plural)
   (head ((cat team-name)
	  (home ((lex "Denver")))
	  (franchise ((lex "Nugget")))))))

(def-test t354
  "The hapless Denver Nuggets."
  ((cat compound-proper)
   (number plural)
   (describer ((lex "hapless")))
   (head ((cat team-name)
	  (home ((lex "Denver")))
	  (franchise ((lex "Nugget")))))))

(def-test t355
  "The Denver Nuggets, who extended their losing streak to five games."
  ((cat compound-proper)
   (number plural)
   (qualifier ((cat clause)
	       (restrictive no)
	       (process ((type composite)
			 (relation-type locative)
			 (lex "extend")))
	       (mood relative)
	       (tense past)
	       (scope {^ partic agent})
	       (partic ((affected ((cat common)
				   (possessor ((cat personal-pronoun)
					       (person third)
					       (number plural)))
				   (classifier ((cat verb)
						(ending present-participle)
						(lex "lose")))
				   (head ((lex "streak")))))
			(located {^ affected})
			(location ((cat pp)
				   (prep ((lex "to")))
				   (np ((cat measure)
					(quantity ((value 5) (digit no)))
					(unit ((lex "game")))))))))))
   (head ((cat team-name)
	  (home ((lex "Denver")))
	  (franchise ((lex "Nugget")))))))

(def-test t356
  "The hapless Denver Nuggets, who extended their losing streak to six games."
  ((cat compound-proper)
   (number plural)
   (describer ((lex "hapless")))
   (qualifier ((cat clause)
	       (restrictive no)
	       (process ((type composite)
			 (relation-type locative)
			 (lex "extend")))
	       (mood relative)
	       (tense past)
	       (scope {^ partic agent})
	       (partic ((affected ((cat common)
				   (possessor ((cat personal-pronoun)
					       (person third)
					       (number plural)))
				   (classifier ((cat verb)
						(ending present-participle)
						(lex "lose")))
				   (head ((lex "streak")))))
			(located {^ affected})
			(location ((cat pp)
				   (prep ((lex "to")))
				   (np ((cat measure)
					(quantity ((value 6) (digit no)))
					(unit ((lex "game")))))))))))
   (head ((cat team-name)
	  (home ((lex "Denver")))
	  (franchise ((lex "Nugget")))))))

(def-test t357
  "John Stockton scored 27 points."
  ((cat clause)
   (tense past)
   (process ((type material)
	     (effect-type creative)
	     (lex "score")))
   (partic ((agent ((cat compound-proper)
		    (head ((cat person-name)
			   (first-name ((lex "John")))
			   (last-name ((lex "Stockton")))))))
	    (created ((cat measure)
		      (quantity ((value 27)))
		      (unit ((lex "point")))))))))

(def-test t358
  "John Stockton scored a season high 27 points."
  ((cat clause)
   (tense past)
   (process ((type material)
	     (effect-type creative)
	     (lex "score")))
   (partic ((agent ((cat compound-proper)
		    (head ((cat person-name)
			   (first-name ((lex "John")))
			   (last-name ((lex "Stockton")))))))
	    (created ((cat common)
		      (definite no)
		      (classifier ((cat noun-compound)
				   (classifier ((lex "season")))
				   (head ((lex "high")))))
		      (head ((cat measure)
			     (quantity ((value 27)))
			     (unit ((lex "point")))))))))))

(def-test t359
  "John Stockton's 27 points."
  ((cat common)
   (possessor ((cat compound-proper)
	       (head ((cat person-name)
		      (first-name ((lex "John")))
		      (last-name ((lex "Stockton")))))))
   (head ((cat measure)
	  (quantity ((value 27)))
	  (unit ((lex "point")))))))

(def-test t360
  "John Stockton's season high 27 points."
  ((cat common)
   (possessor ((cat compound-proper)
	       (head ((cat person-name)
		      (first-name ((lex "John")))
		      (last-name ((lex "Stockton")))))))
   (classifier ((cat noun-compound)
		(classifier ((lex "season")))
		(head ((lex "high")))))
   (head ((cat measure)
	  (quantity ((value 27)))
	  (unit ((lex "point")))))))

(def-test t361
  "The Sacramento Kings' NBA record 35th straight road loss."
  ((cat common)
   (possessor ((cat compound-proper)
	       (number plural)
	       (head ((cat team-name)
		      (home ((lex "Sacramento")))
		      (franchise ((lex "King")))))))
   (classifier ((cat noun-compound)
		(classifier ((lex "NBA")))
		(head ((lex "record")))))
   (head ((cat measure)
	  (quantity ((cat ordinal) (value 35)))
	  (unit ((cat noun-compound)
		 (classifier ((cat list)
			      (distinct ~(((cat adj)
					   (lex "straight"))
					  ((cat noun)
					   (lex "road"))))))
		 (head ((lex "loss")))))))))

(def-test t362
  "The Atlanta Hawks, winners of six straight games and five in a row at the Omni."
  ((cat np)
   (complex apposition)
   (distinct
    ~(((cat compound-proper)
       (number plural)
       (head ((cat team-name)
	      (home ((lex "Atlanta")))
	      (franchise ((lex "Hawk"))))))
      ((cat common)
       (definite no)
       (number plural)
       (head ((lex "winner")))
       (qualifier
	((cat pp)
	 (prep ((lex "of")))
	 (np ((cat np)
	      (complex conjunction)
	      (distinct
	       ~(((cat common)
		  (definite no)
		  (cardinal ((value 6) (digit no)))
		  (classifier ((cat adj) (lex "straight")))
		  (head ((lex "game"))))
		 ((cat common)
		  (definite no)
		  (cardinal ((value 5) (digit no)))
		  (head ((gap yes)))
		  (qualifier ((cat list)
			      (distinct
			       ~(((cat pp)
				  (prep ((lex "in")))
				  (np ((cat common)
				       (definite no)
				       (head ((lex "row"))))))
				 ((cat pp)
				  (prep ((lex "at")))
				  (np ((cat common)
				       (lex "Omni"))))))))))))))))))))


(def-test t363
  "Dr. Rufus T. Firefly gave Prof. Hugo Z. Hackenbush his best regards."
  ((cat clause)
   (tense past)
   (process ((type composite)
	     (relation-type possessive)
	     (lex "give")))
   (partic ((agent ((cat compound-proper)
		    (head ((cat person-name)
			   (title ((lex "Dr.")))
			   (first-name ((lex "Rufus")))
			   (middle-name ((lex "T.")))
			   (last-name ((lex "Firefly")))))))
	    (affected ((cat compound-proper)
		       (head ((cat person-name)
			      (title ((lex "Prof.")))
			      (first-name ((lex "Hugo")))
			      (middle-name ((lex "Z.")))
			      (last-name ((lex "Hackenbush")))))))
	    (possessor {^ affected})
	    (possessed ((cat common)
			(number plural)
			(possessor ((cat personal-pronoun)
				    (person third)
				    (number singular)
				    (gender masculine)))
			(describer ((lex "best")))
			(head ((lex "regard")))))))))

(def-test t364
  "The highly respected Dr. Rufus T. Firefly gave Prof. Hugo Z. Hackenbush, who received the Turing award yesterday, his best regards."
  ((cat clause)
   (tense past)
   (process ((type composite)
	     (relation-type possessive)
	     (lex "give")))
   (partic
    ((agent ((cat compound-proper)
	     (describer ((cat ap)
			 (modifier ((lex "highly")))
			 (head ((lex "respected")))))
	     (head ((cat person-name)
		    (title ((lex "Dr.")))
		    (first-name ((lex "Rufus")))
		    (middle-name ((lex "T.")))
		    (last-name ((lex "Firefly")))))))
     (affected ((cat compound-proper)
		(head ((cat person-name)
		       (title ((lex "Prof.")))
		       (first-name ((lex "Hugo")))
		       (middle-name ((lex "Z.")))
		       (last-name ((lex "Hackenbush")))))
		(qualifier ((cat clause)
			    (tense past)
			    (mood relative)
			    (restrictive no)
			    (voice passive)
			    (process ((type composite)
				      (agentive no)
				      (relation-type possessive)
				      (lex "receive")))
			    (scope {^ partic affected})
			    (partic
			     ((possessor {^ affected})
			      (possessed ((cat common)
					  (classifier ((cat basic-proper)
						       (lex "Turing")))
					  (head ((lex "award")))))))
			    (circum ((time ((cat adv)
					    (lex "yesterday")))))))))
     (possessor {^ affected})
     (possessed ((cat common)
		 (number plural)
		 (possessor
		  ((cat personal-pronoun)
		   (semantics ((index {^4 agent semantics index})))))
		 (describer ((lex "best")))
		 (head ((lex "regard")))))))))

(def-test t364bis
  "The seemingly unstoppable San Antonio Spurs extended their winning streak to 30 games with a 111 85 victory over the hapless Denver Nuggets, losers of 11 in a row."
  ((cat clause)
   (tense past)
   (process ((type composite)
	     (relation-type locative)
	     (lex "extend")))
   (partic
    ((agent ((cat compound-proper)
	     (number plural)
	     (describer ((cat ap)
			 (modifier ((lex "seemingly")))
			 (head ((lex "unstoppable")))))
	     (head ((cat team-name)
		    (home ((lex "San Antonio")))
		    (franchise ((lex "Spur")))))))
     (affected ((cat common)
		(possessor ((cat personal-pronoun)
			    (number plural)
			    (person third)))
		(classifier ((cat verb)
			     (ending present-participle)
			     (lex "win")))
		(head ((lex "streak")))))
     (located {^ affected})
     (location ((cat pp)
		(prep ((lex "to")))
		(np ((cat measure)
		     (quantity ((value 30)))
		     (unit ((lex "game")))))))))
   (pred-modif
    ((instrument
      ((cat pp)
       (np ((cat common)
	    (definite no)
	    (classifier ((cat list)
			 (distinct ~(((cat measure)
				      (quantity ((value 111)))
				      (unit ((gap yes))))
				     ((cat measure)
				      (quantity ((value 85)))
				      (unit ((gap yes))))))))
	    (head ((lex "victory")))
	    (qualifier
	     ((cat pp)
	      (prep ((lex "over")))
	      (np ((cat np)
		   (complex apposition)
		   (distinct
		    ~(((cat compound-proper)
		       (number plural)
		       (describer ((lex "hapless")))
		       (head ((cat team-name)
			      (home ((lex "Denver")))
			      (franchise ((lex "Nugget"))))))
		      ((cat common)
		       (definite no)
		       (number plural)
		       (head ((lex "loser")))
		       (qualifier
			((cat pp)
			 (prep ((lex "of")))
			 (np ((cat common)
			      (definite no)
			      (cardinal ((value 11)))
			      (head ((gap yes)))
			      (qualifier
			       ((cat pp)
				(prep ((lex "in")))
				(np ((cat common)
				     (definite no)
				     (head ((lex "row"))))))))))))))))))))))))))

(def-test t364ter
  "The hapless Denver Nuggets, losers of 11 in a row."
  ((cat np)
   (complex apposition)
   (distinct
    ~(((cat compound-proper)
       (number plural)
       (describer ((lex "hapless")))
       (head ((cat team-name)
	      (home ((lex "Denver")))
	      (franchise ((lex "Nugget"))))))
      ((cat common)
       (definite no)
       (number plural)
       (head ((lex "loser")))
       (qualifier
	((cat pp)
	 (prep ((lex "of")))
	 (np ((cat common)
	      (definite no)
	      (cardinal ((value 11)))
	      (head ((gap yes)))
	      (qualifier
	       ((cat pp)
		(prep ((lex "in")))
		(np ((cat common)
		     (definite no)
		     (head ((lex "row"))))))))))))))))


(def-test t364quad
  "Losers of 11 in a row."
  ((cat common)
   (definite no)
   (number plural)
   (head ((lex "loser")))
   (qualifier
    ((cat pp)
     (prep ((lex "of")))
     (np ((cat common)
	  (definite no)
	  (cardinal ((value 11)))
	  (head ((gap yes)))
	  (qualifier
	   ((cat pp)
	    (prep ((lex "in")))
	    (np ((cat common)
		 (definite no)
		 (head ((lex "row")))))))))))))


(def-test t365
  "San Antonio defeated Denver."
  ((cat clause)
   (tense past)
   (process ((lex "defeat")))
   (partic ((agent ((cat basic-proper)
		    (lex "San Antonio")))
	    (affected ((cat basic-proper)
		       (lex "Denver")))))))

(def-test t365b
  "San Antonio defeated Denver."
  ((cat clause)
   (tense past)
   (process ((lex "defeat")))
   (partic ((agent ((cat compound-proper)
		    (head ((cat team-name)
			   (home ((lex "San Antonio")))))))
	    (affected ((cat compound-proper)
		       (head ((cat team-name)
			      (home ((lex "Denver")))))))))))

(def-test t366
  "The Spurs defeated the Nuggets."
  ((cat clause)
   (tense past)
   (process ((lex "defeat")))
   (partic ((agent ((cat compound-proper)
		    (number plural)
		    (head ((cat team-name)
			   (franchise ((lex "Spur")))))))
	    (affected ((cat compound-proper)
		       (number plural)
		       (head ((cat team-name)
			      (franchise ((lex "Nugget")))))))))))

(def-test t371
  "My advisor and friend, Steve."
  ((cat np)
   (complex apposition)
   (restrictive no)
   (distinct
    ~(((cat common)
       (head ((cat noun)
	      (complex conjunction)
	      (distinct ~(((lex "advisor"))
			  ((lex "friend"))))))
       (possessor ((cat personal-pronoun)
		   (person first))))
      ((cat basic-proper)
       (lex "Steve"))))))

(def-test t372
  "Three times the butter."
  ((cat common)
   (lex "butter")
   (countable no)
   (multiplier ((value 3) (digit no)))
   (definite yes)))


(def-test t373
  "The man whose hat is on the ground."
  ((cat common)
   (lex "man")
   (animate yes)
   (qualifier ((cat clause)
	       (mood possessive-relative)
	       (process ((type locative)))
	       (scope {^ partic located})
	       (partic ((located ((cat common) (lex "hat")))
			(location ((cat pp)
				   (prep ((lex "on")))
				   (np ((cat common) (lex "ground")))))))))))


(def-test t374
  "Bowman, whose passing game is exceptional."
  ((cat compound-proper)
   (head ((cat person-name)
	  (last-name ((lex "Bowman")))))
   (animate yes)
   (qualifier ((cat clause)
	       (mood possessive-relative)
	       (restrictive no)
	       (process ((type ascriptive)))
	       (scope {^ partic carrier})
	       (partic ((carrier ((cat common)
				  (classifier ((cat verb)
					       (ending present-participle)
					       (lex "pass")))
				  (head ((lex "game")))))
			(attribute ((cat ap) (lex "exceptional")))))))))


(def-test t375
  "A 127 - 111 win over Denver sending the Nuggets to their seventh straight loss."
 ((cat common)
  (definite no)
  (classifier ((cat score)
	       (win ((value 127)))
	       (lose ((value 111)))))
  (head ((lex "win")))
  (qualifier
   ((cat list)
    (distinct ~(((cat pp)
		 (prep ((lex "over")))
		 (np ((cat compound-proper)
		      (head ((cat team-name)
			     (home ((lex "Denver"))))))))
		((cat clause)
		 (mood present-participle)
		 (process ((type composite)
			   (relation-type locative)
			   (lex "send")))
		 (controlled {^ partic agent})
		 (partic ((agent ((index {^7 index})))
			  (located ((cat compound-proper)
				    (number plural)
				    (head ((cat team-name)
					   (franchise ((lex "Nugget")))))))
			  (affected {^ located})
			  (location ((cat pp)
				     (prep ((lex "to")))
				     (np ((cat common)
					  (possessor ((cat personal-pronoun)
						      (index {^4 located index})))
					  (ordinal ((value 7) (digit no)))
					  (describer ((lex "straight")))
					  (head ((lex "loss"))))))))))))))))


(def-test t376
  "John gives Mary his book."
  ((cat clause)
   (proc ((type composite) (relation-type possessive)))
   (partic ((agent ((cat basic-proper) (lex "John") (gender masculine)))
	    (affected ((cat basic-proper) (lex "Mary") (gender feminine)))
	    (possessor {^ affected})
	    (possessed ((cat common)
			(possessor ((cat personal-pronoun)
				    (index  {^3 agent index})))
			(head ((lex "book")))))))))


(def-test t377
  "The Nuggets, who lost."
  ((cat compound-proper)
   (number plural)
   (head ((cat team-name) (franchise ((lex "Nugget")))))
   (qualifier ((cat clause)
	       (tense past)
	       (restrictive no)
	       (process ((type material) (agentive no) (lex "lose")))
	       (scope {^ partic affected})
	       (partic ((affected ((index {^4 index})))))))))
(store-verbs '(("lose" "loses" "lost" "losing" "lost")))

(def-test t378
 "666 Visconde de Piraja Street, # 910, Ipanema."
  ((cat address)
   (num ((value 666)))
   (st-name ((lex "Visconde de Piraja")))
   (st-type ((lex "Street")))
   (apt-num ((lex "910")))
   (hood ((lex "Ipanema")))))

(def-test t379
  "666 West 112th Street, # 3D, Manhattan."
  ((cat address)
   (side ((lex "West")))
   (num ((value 666)))
   (st-name ((value 112)))
   (hood ((lex "Manhattan")))
   (apt-num ((lex "3D")))
   (st-type ((lex "Street")))))

(def-test t380
  "309 Michael Elhadad Boulevard, # 1244, N.W. Bersheva."
  ((cat address)
   (num ((value 309)))
   (st-name ((cat person-name)
	     (first-name ((lex "Michael")))
	     (last-name ((lex "Elhadad")))))
   (st-type ((lex "Boulevard")))
   (apt-num ((lex "1244")))
   (quadrant ((lex "N.W.")))
   (city ((lex "Bersheva")))))

(def-test t381
  "Pelourinho, Bahia, Brazil."
  ((cat address)
   (hood ((lex "Pelourinho")))
   (state ((lex "Bahia")))
   (country ((lex "Brazil")))))

(def-test t382
  "Rio de Janeiro, RJ, Brazil."
  ((cat address)
   (city ((lex "Rio de Janeiro")))
   (state ((lex "RJ")))
   (country ((lex "Brazil")))))

(def-test t383
  "666 Visconde de Piraja Street, # 910, Ipanema, Rio de Janeiro, RJ, Brazil."
  ((cat address)
   (num ((value 666)))
   (st-name ((lex "Visconde de Piraja")))
   (st-type ((lex "Street")))
   (apt-num ((lex "910")))
   (hood ((lex "Ipanema")))
   (city ((lex "Rio de Janeiro")))
   (state ((lex "RJ")))
   (country ((lex "Brazil")))))

(def-test t384
  "309 Michael Elhadad Boulevard, # 1244, N.W. Bersheva, The Negev, 84120 Israel."
  ((cat address)
   (num ((value 309)))
   (st-name ((cat person-name)
	     (first-name ((lex "Michael")))
	     (last-name ((lex "Elhadad")))))
   (st-type ((lex "Boulevard")))
   (apt-num ((lex "1244")))
   (quadrant ((lex "N.W.")))
   (city ((lex "Bersheva")))
   (state ((lex "The Negev")))
   (zip ((value 84120)))
   (country ((lex "Israel")))))

(def-test t385
  "544 Faidherbe Prolongee Avenue, Medina, P.O. Box 3275, Dakar, Senegal."
  ((cat address)
   (num ((value 544)))
   (st-name ((lex "Faidherbe Prolongee")))
   (st-type ((lex "Avenue")))
   (hood ((lex "Medina")))
   (po-box-num ((value 3275)))
   (city ((lex "Dakar")))
   (country ((lex "Senegal")))))

(def-test t386
  "San Antonio, Texas."
  ((cat address) (city ((lex "San Antonio"))) (state ((lex "Texas")))))


(def-test t387
  "Friday."
  ((cat date) (day-name ((lex "Friday")))))

(def-test t388
  "Friday night."
  ((cat date) (day-name ((lex "Friday"))) (day-part ((lex "night")))))

(def-test t389
  "June 1999."
  ((cat date) (month ((lex "June"))) (year ((value "1999")))))

;; "night" is a special symbol - as a string it would give "in the night".
(def-test t390
  "Friday the 13th, at night."
  ((cat date)
   (day-name ((lex "Friday")))
   (day-num ((value 13)))
   (day-part ((lex night)))))

(def-test t391
  "Friday 6 / 13 / 1999, in the evening."
  ((cat date)
   (day-name ((lex "Friday")))
   (day-num ((value 13)))
   (month ((value 6)))
   (year ((value 1999)))
   (day-part ((lex "evening")))))

(def-test t392
  ("Friday the 13th of June 1999, in the morning."
   "The morning of Friday June 13th 1999.")
  ((cat date)
   (day-name ((lex "Friday")))
   (day-num ((value 13)))
   (month ((lex "June")))
   (year ((value 1999)))
   (day-part ((lex "morning")))))


(def-test t393
  "I live 666 Visconde de Piraja Street, # 910, Ipanema, Rio de Janeiro, RJ, Brazil."
  ((cat clause)
   (process ((type locative) (lex "live")))
   (partic ((located ((cat personal-pronoun) (person first)))
	    (location ((cat address)
		       (num ((value 666)))
		       (st-name ((lex "Visconde de Piraja")))
		       (st-type ((lex "Street")))
		       (apt-num ((lex "910")))
		       (hood ((lex "Ipanema")))
		       (city ((lex "Rio de Janeiro")))
		       (state ((lex "RJ")))
		       (country ((lex "Brazil")))))))))


(def-test t394
  ("It happened the morning of Friday June 13th 1999."
   "It happened Friday the 13th of June 1999, in the morning.")
  ((cat clause)
   (tense past)
   (process ((type temporal) (lex "happen")))
   (partic ((located ((cat personal-pronoun)))
	    (time ((cat date)
		   (day-name ((lex "Friday")))
		   (day-num ((value 13)))
		   (month ((lex "June")))
		   (year ((value 1999)))
		   (day-part ((lex "morning")))))))))


(def-test t395a
  "Saturday night -- Karl Malone scored 28 points with his hands."
  ((cat clause)
   (tense past)
   (process ((type material) (effect-type creative) (lex "score")))
   (partic ((agent ((cat compound-proper)
		    (gender masculine)
		    (head ((cat person-name)
			   (first-name ((lex "Karl")))
			   (last-name ((lex "Malone")))))))
	    (created ((cat measure)
		      (quantity ((value 28)))
		      (unit ((lex "point")))))))
   (pred-modif ((instrument ((cat pp)
			     (np ((cat common)
				  (number plural)
				  (possessor ((cat personal-pronoun)
					      (index {^5 partic agent index})))
				  (head ((lex "hand")))))))))
   (circum ((time ((cat date)
		   (day-name ((lex "Saturday")))
		   (day-part ((lex "night")))
		   (position header)))))))

(def-test t395b
  "Karl Malone scored 28 points with his hands Saturday night."
  ((cat clause)
   (tense past)
   (process ((type material) (effect-type creative) (lex "score")))
   (partic ((agent ((cat compound-proper)
		    (gender masculine)
		    (head ((cat person-name)
			   (first-name ((lex "Karl")))
			   (last-name ((lex "Malone")))))))
	    (created ((cat measure)
		      (quantity ((value 28)))
		      (unit ((lex "point")))))))
   (pred-modif ((instrument ((cat pp)
			     (np ((cat common)
				  (number plural)
				  (possessor ((cat personal-pronoun)
					      (index {^5 partic agent index})))
				  (head ((lex "hand")))))))))
   (circum ((time ((cat date)
		   (position end)
		   (day-name ((lex "Saturday")))
		   (day-part ((lex "night")))))))))

(def-test t396
  "In Brazil, near Bahia."
  ((cat pp)
   (complex apposition)
   (restrictive no)
   (distinct ~(((prep ((lex "in")))
		(np ((cat basic-proper) (lex "Brazil"))))
	       ((prep ((lex "near")))
		(np ((cat basic-proper) (lex "Bahia"))))))))


(def-test t397
  "In Bahia and in Rio."
  ((cat pp)
   (complex conjunction)
   (distinct ~(((prep ((lex "in")))
		(np ((cat basic-proper) (lex "Bahia"))))
	       ((prep ((lex "in")))
		(np ((cat basic-proper) (lex "Rio"))))))))

(def-test t398
  "Align the holding battery cover plate with the holding battery compartment."
  ((cat clause)
   (mood imperative)
   (proc ((type composite)
	  (relation-type locative)
	  (lex "align")))
   (partic ((affected ((cat common) (lex "holding battery cover plate")))
	    (located {^ affected})
	    (location ((cat pp)
		       (prep ((lex "with")))
		       (np ((cat common) (lex "holding battery compartment")))))))))



;; ============================================================
;; Examples with embedded clauses and if/then constructions
;; ============================================================

(def-test t400
  "They believe that it works."
  ((cat clause)
   (proc ((type mental)
	  (object-clause that)
	  (lex "believe")))
   (partic ((processor ((cat personal-pronoun)
			(animate yes)
			(person third)
			(number plural)))
	    ;; it works
	    (phenomenon ((cat clause)
			 (proc ((type material)
				(agentive no)
				(lex "work")))
			 (partic ((affected ((animate no)
					     (cat personal-pronoun)
					     (person third)
					     (number singular)))))))))))


(def-test t401
  "They believe that it works."
  ((cat clause)
   ;; Lexical entry for "believe that"
   (proc ((type lexical)
	  (lex "believe")
	  (subcat ((1 {^3 lex-roles believer})
		   (2 {^3 lex-roles belief})
		   (1 ((cat np)))
		   (2 ((cat clause)
		       (binder ((lex "that")))
		       (mood bound)))))))
   ;; Equivalent of participants when using lexical-processes
   ;; The name of the roles is defined by the lexical entry above.
   (lex-roles ((believer ((cat personal-pronoun)
			  (animate yes)
			  (person third)
			  (number plural)))
	       ;; it works
	       (belief ((cat clause)
			(proc ((type material)
			       (agentive no)
			       (lex "work")))
			(partic ((affected ((animate no)
					    (cat personal-pronoun)
					    (person third)
					    (number singular)))))))))))


(def-test t402
  "If it rains then it rains."
  ((cat clause)
   (complex apposition)
   (restrictive yes)
   (verbal-ellipsis no)
   (subject-ellipsis no)
   (distinct ~( ((relaters ((cond ((lex "if")))))
		 (cat clause) (proc ((type natural-phenom) (lex "rain"))))
		((relaters ((cond ((lex "then")))))
		 ;; (cond-relater ((lex "then")))
		 (cat clause) (proc ((type natural-phenom) (lex "rain"))))))))


;; LIMITATION!
;;  "They believe that if it rains that then it rains."
;;                                 **** PROBLEM!
;; Why does it come out like that?
;; In complex.l, I propagate mood as one of the "common" features for
;; clauses, that is, when you conjoin 2 clauses, they must have the same mood.
;; Thus for example, it is not grammatical to have:
;; * Come here and how are you?
;; (mood imperative) with (mood interrogative) are not compatible.
;;
;; Now, in our case, we have (mood bound) (which means that the clauses are
;; embedded as complements of a verb).  So the (mood bound) propagates to
;; common, and is imposed on each conjoined clause.  So you get the extra
;; that (which is added for bound clauses).
;;
;; How do we fix it?
;; The main problem is that the construct "if A then B" is not really a
;; conjunction, but is a special construct that should be handled as such.
;; But we have no such things in SURGE 1.2 until clause complexes are dealt
;; with seriously.
;; In the mean time, we could change the grammar for conjunctions and add a
;; special case treatment for (mood bound) in conjunctions of clauses and
;; remove the that when that happens.  But that's not the right thing to do,
;; as shown by the following examples:
;; "They believe that 1 is integer and that 2 is not."
;;
;; Here the "that" must be repeated.
;; So, the problem is really in the treatment of "if A then B" when embedded,
;; not in the treatment of embedded conjunctions.
;; Since I have no "formal" trace in the input that we are dealing with such a
;; construct, I add the feature (binder ((gap yes))) in the second part of
;; the embedded construct (then B).  The resulting fd t403.

(def-test t403
  "They believe that if it rains then it rains."
  ((cat clause)
   (proc ((type mental)
	  (object-clause that)
	  (lex "believe")))
   (partic ((processor ((cat personal-pronoun)
			(animate yes)
			(person third)
			(number plural)))
	    ;; here comes t402 embedded
	    (phenomenon ((cat clause)
			 (complex apposition)
			 (restrictive yes)
			 (verbal-ellipsis no)
			 (subject-ellipsis no)
			 (distinct ~( ((relaters ((cond ((lex "if")))))
				       (cat clause)
				       (proc ((type natural-phenom)
					      (lex "rain"))))
				      ((relaters ((cond ((lex "then")))))
				       (binder ((gap yes)))
				       (cat clause)
				       (proc ((type natural-phenom)
					      (lex "rain"))))))))))))


;; ============================================================
;; Examples with conjunctions and ellipsis
;; ============================================================
;; Expansion of example t27bis.
;; A few comments on the conjunction and use of common:
;; a/ Note that I've added the features semantics/index/concept to the agents.
;; If you don't, you can get spurious ellipsis.  The concept is supposed to be
;; unique for each referring expression's referent and is used to distinguish
;; two ref.expressions from each others.  I'm starting to work on some form of
;; intra-sentence pronominalization and this plays the role of subscripts
;; (index) in normal syntax.
;; b/ Common should only be used by the grammar, and not specified in the
;; input FD.  The idea of common is that it gathers the features which MUST be
;; identical among all conjuncts because of a syntactic constraint - and not
;; features which happen to be identical in a particular conjunction.
;; Therefore, even though the verbs are identical, you shouldn't put them in
;; common.  Note that you still can have an effect of ellipsis, as shown in
;; t405. [I don't know why t404 does not do ellipsis of the verb when it is
;; embedded in the circum/temporal-background, need to check that.]

(def-test t404
  "Near is the hour when bears bite people and dogs bite cats."
  ((cat clause)
   (process ((type ascriptive)
	     (voice passive)
	     (mode equative)))
   (partic ((identifier ((cat proper)
			 (lex "near")))
	    (identified ((cat common)
			 (definite yes)
			 (lex "hour")))))
   (circum
    ((time
      ((cat clause)
       (complex conjunction)
       (common ((mood declarative)))
       (distinct
	~(((process ((type material) (lex "bite")))
	   (partic ((agent ((cat common)
			    (lex "bear")
			    (semantics ((index ((concept bears)))))
			    (number plural)
			    (definite no)))
		    (affected ((cat common)
			       (lex "people")
			       (countable no))))))
	  ((process ((type material) (lex "bite")))
	   (partic ((agent ((cat common)
			    (lex "dog")
			    (semantics ((index ((concept dog)))))
			    (number plural)
			    (definite no)))
		    (affected ((cat common)
			       (lex "cat")
			       (definite no)
			       (number plural))))))))))))))


(def-test t405
  ;; Note the ellipsis of the verb "bite"
  "Bears bite people and dogs cats."
  ((cat clause)
   (complex conjunction)
   (common ((mood declarative)))
   (distinct
    ~(((process ((type material) (lex "bite")))
       (partic ((agent ((cat common)
			(lex "bear")
			(semantics ((index ((concept bears)))))
			(number plural)
			(definite no)))
		(affected ((cat common)
			   (lex "people")
			   (countable no))))))
      ((process ((type material) (lex "bite")))
       (partic ((agent ((cat common)
			(lex "dog")
			(semantics ((index ((concept dog)))))
			(number plural)
			(definite no)))
		(affected ((cat common)
			   (lex "cat")
			   (definite no)
			   (number plural))))))))))





;; ============================================================
;; Examples with verbs with particles "is composed of", "is contained in"
;; ============================================================
;; Expansion of example t44bis
;; There are several issues here.  The first point is that you should avoid as
;; much as possible to have verbs such as "is composed of" in one string.
;; That cannot work well.  The problem is then: how to deal with them?
;; These verbs have 3 problems:
;; a/ They have a "passive" form [be -ed]
;; b/ They require a preposition in one of the complement [which is not "by"]
;; c/ It is not clear where these verbs fall in the transitivity system.
;;
;; In general, to deal with verbs like (b) which require a preposition or (c)
;; which do not fall neatly in one of the predefined transitivity classes, you
;; should use the lexical process-type as is done for example with "to deal
;; with".  This does 2 things for you: first, you don't need to force the
;; semantics of the verb into something which does not fit (eg, to say that
;; "is composed of" is a material process), and second, it allows you to
;; specify specific constraints on the syntactic realization of complements
;; which are governed by the verb.  For example, the fact that in "deal with"
;; the first complement must be a PP with prep with is specified in the subcat
;; feature of the entry for the verb.  Similarly, check the examples with
;; "A require X to Y" the form of the complement "to Y" which must be an
;; infinitive clause with a control relation to X is specified in the subcat
;; feature of the entry for "require".

;; In our particular cases, though, we can still stick to the general
;; transitivity system if we note that:
;; a/ "is composed of" and "is contained in" are passive forms of
;; "(?) wax composes a candles" and "An anther contains a pollen sac".
;; [I'm not saying that these forms are acceptable, in fact, with the lexical
;; entry I'm giving for the verbs, they cannot be generated, but this is
;; useful to analyze the verb in simple terms.]
;;
;; Now these two processes are instances of the generic relation "locative"
;; [something like "a candle contains wax" for the first case]. So we analyze
;; the processes as shown below in w1 and co1.  The final problem is (b) the
;; preposition which is not "by" as it should be in a passive form.  To fix
;; this, I've added a feature "passive-prep" in the verb entry which
;; determines which prep is used in front of the by-obj of a passive clause.
;;
;; In the case of "result in" (t407bis) we turn to a lexical process type.

(def-test t406
  ;; Something like "a candle contains wax" in terms of case roles
  ;; So we do locative, with candle as location.
  "A candle is composed of wax."
  ((cat clause)
   (process ((type locative) (mode equative)
	     (lex "compose") (voice passive)
	     (passive-prep ((lex "of")))))
    ;; Default is without agent - override it.
   (agentless no)
   (partic ((location ((cat common)
		       (definite no)
		       (lex "candle")))
	    (located ((cat common)
		      (countable no)
		      (lex "wax")))))))


;; Example of "is contained in" as a passive of "contains"
;; NOW: there is something very deep going on here concerning the semantics
;; of "contain" and the semantics of the roles "location" and "located".
;; This should be inverse: anther is location, sac is located.
;; But, the passive voice switches the roles around, and the semantics of
;; "contain" is "Location contains located".
;; SO: what is really important here is not the relation located/location
;; which is expressed by the lexical verb, but rather the discourse
;; perspective which is captured by the distinction identified/identifier
;; which is the more generic classification of equative relations.
;; So here, we use identified:anther, identifier:sac, voice:passive
;; (switches around the perspective).
(def-test t407
  "A pollen sac is contained in an anther."
  ((cat clause)
   (proc ((type locative)
	  (mode equative);; important - otw relation not passivable
	  (voice passive)
	  (lex "contain")
	  (passive-prep ((lex "in")))))
   (agentless no)
   (partic ((identifier ((cat common)
			 (definite no)
			 (classifier ((lex "pollen")))
			 (lex "sac")))
	    (identified ((cat common)
			 (definite no)
			 (lex "anther")))))))

;; Result-in
(def-test t407bis
  "The process continues, resulting in four haploid cells."
  ((cat clause)
   (process ((type material)
	     (agentive no)
	     (effective yes)
	     (lex "continue")))
   (partic ((affected ((cat common) (lex "process")))))
   (circum ((result ((cat clause)
		     (mood present-participle)
                     (controlled {^ lex-roles cause})
		     (process ((type lexical)
			       (lex "result")
			       (subcat ((1 {^3 lex-roles cause})
					(2 ((cat pp)
					    (prep ((lex "in")))
					    (np {^4 lex-roles effect})))))))
		     (lex-roles
                      ((cause ((semantics ((index ((concept matrix)))))))
                       (effect ((cat np)
                                (cardinal ((value 4)))
                                (classifier ((lex "haploid")))
                                (definite no)
                                (lex "cell")))))))))))


;; ============================================================
;; Examples on adverbs and circumstantials and adv-p
;; ============================================================

;; Note that treatment of circumstantials in SURGE 1.2 is rather weak and
;; is the main difference with SURGE 2.0 - which includes much more
;; consistent coverage of circumstantials.
;; In the following examples (provided by Charles Callaway
;; theorist@cs.utexas.edu)), the adverb is not part of the
;; verb-group, but rather of a circumstantial or participant.
;; In all these examples, you do NOT want to use the simple adverb
;; constituent which is supposed to be a modifier of the verb.
;; Instead, you want to use one of the circumstantials which
;; allows for adv as filler.  So far, in Surge 1.2, there are only 2:
;; manner and time.
;; I'll add the bracketing according to the SURGE 1.2 analysis in each
;; example:
;;
;; Death occurs [immediately after life].
;; [Process: temporal,
;;  Located: death
;;  Time:    immediately after life ]
;;
;; The sun rises soon after the sky turns pink in the east.
;; [Process: material
;;  Affected: sun
;;  Circum: Time: [soon after the sky turns pink in the east]]
;;
;; He spoke dryly.
;; [Process: verbal
;;  Agent: He
;;  Circum: Manner: dryly]
;;
;; Airplanes streak quickly across the sky.
;; [Process: material
;;  Agent: airplanes
;;  Circum: manner: quickly
;;          path: across the sky]


(def-test t408
  "He speaks dryly."
  ((cat clause)
   (process ((type verbal)
	     (lex "speak")
	     (transitive no)))
   (partic ((sayer ((cat pronoun)
		    (person third)
		    (gender masculine)))))
   (circum ((manner ((cat adv)
		     (position end)
		     (lex "dryly")))))))


(def-test t409
  "Airplanes streak quickly across the sky."
  ((cat clause)
   (process ((type material)
	     (lex "streak")))
   (partic ((agent ((lex "airplane")
		    (number plural)
		    (definite no)))))
   (pred-modif ((manner ((cat adv) (position end) (lex "quickly")))
		(path ((cat pp)
		       (prep ((lex "across")))
		       (np ((lex "sky")))))))))


;; The sun rises soon after the sky turns pink in the east.
;;[Process: material
;; Affected: sun
;; Circum: Time: [soon after the sky turns pink in the east]]
;; NOTE the use of an adverbial phrase (adv-p).
(def-test t410
  "The sun rises soon after the sky turns pink in the east."
  ((cat clause)
   (process ((type material)
	     (agentive no)
	     (lex "rise")))
   (partic ((affected ((lex "sun")))))
   (circum ((time ((cat adv-p)
		   (head ((lex "soon")))
		   (compl ((cat clause)
			   (mood bound)
			   (binder ((lex "after")))
			   (process ((agentive no)
				     (effective yes)
				     (effect-type dispositive)
				     (type composite)
				     (relation-type ascriptive)))
			   (partic ((affected ((lex "sky")))
				    (attribute ((lex "pink")
						(cat ap)))))
			   (circum ((in-loc ((lex "east")))))))))))))


;; Death, which occurs [immediately after life],...
;; [Process: temporal,
;;  Located: death
;; Time:    immediately after life ]
(def-test t411
  "Death occurs immediately after life."
  ((cat clause)
   (process ((type temporal)
	     (lex "occur")))
   (partic ((located ((cat np)
		      (lex "death")
		      (countable no)))
	    (time ((cat adv-p)
		   (head ((lex "immediately")))
		   (compl ((cat pp)
			   (prep ((lex "after")))
			   (np ((lex "life")
				(countable no)))))))))))



;; ============================================================
;; Simple focus example - more problems with binders
;; More on circumstantials and different embeddings.
;; ============================================================

;; Use of focus
;; Use of non-finite with binder
(def-test t412
  "How to generate time adjuncts in SURGE must be discovered."
  ((cat clause)
   (focus {^ partic phenomenon})
   (agentless yes)
   (process ((type mental)
             (lex "discover")
             (transitive yes)
             (deontic-modality "must")))
   (partic ((processor ((cat personal-pronoun)
                        (semantics ((index ((person first)
                                             (number plural)))))))
            (phenomenon
             ((cat clause)
              (mood infinitive)
              (binder ((lex "how")))
              (process ((type material) (lex "generate")))
              (partic
               ((agent
                 ((semantics ((index {^5 processor semantics index})))))
                (affected ((cat common)
                           (number plural)
                           (definite no)
                           (lex "adjunct")
                           (classifier ((lex "time")))))))
                             (circum ((in-loc ((cat trivial-proper)
                                               (lex "SURGE")))))))))))


;; Alternative {partic phenomenon mood}: for-to-infinitive forces passive because subject is not
;; specified with a syntactic cat.
(def-test t412b
    "How for time adjuncts to be generated in SURGE must be discovered."
  ((cat clause)
   (focus {^ partic phenomenon})
   (agentless yes)
   (process ((type mental)
             (lex "discover")
             (transitive yes)
             (deontic-modality "must")))
   (partic ((processor ((cat personal-pronoun)
                        (semantics
                         ((index ((person first)
                                  (number plural)))))))
            (phenomenon
             ((cat clause)
              (mood for-to-infinitive)
              (agentless yes)
              (binder ((lex "how")))
              (process ((type material) (lex "generate")))
              (partic
               ((agent ((semantics ((index {^5 processor semantics index})))))
                (affected ((cat common)
                           (number plural)
                           (definite no)
                           (lex "adjunct")
                           (classifier ((lex "time")))))))
              (circum ((in-loc ((cat trivial-proper)
                                (lex "SURGE")))))))))))


;; Use of time circumstance with finite clause
(def-test t413
  "Before we explain embryo sac formation, we must discover how to generate time adjuncts in SURGE."
  ((cat clause)
   (process ((type mental)
	     (lex "discover")
	     (transitive yes)
	     (deontic-modality "must")))
   (partic ((processor
             ((cat personal-pronoun)
              (semantics ((index ((person first)
                                  (number plural)))))))
            (phenomenon
             ((cat clause)
              (mood infinitive)
              (binder ((lex "how")))
              (process ((type material) (lex "generate")))
              (partic ((agent ((semantics
                                ((index {^5 processor semantics index})))))
                       (affected ((cat common)
                                  (number plural)
                                  (definite no)
                                  (lex "adjunct")
                                  (classifier ((lex "time")))))))
              (circum ((in-loc ((cat trivial-proper)
                                (lex "SURGE")))))))))
   (circum
    ((time ((binder ((lex "before")))
	    (position front)
	    (cat clause)
	    (process ((type mental) (lex "explain")))
	    (partic ((processor ((cat personal-pronoun)
				 (person first)
				 (number plural)))
		     (phenomenon ((cat common)
				  (countable no)
				  (lex "formation")
				  (classifier ((cat noun-compound)
					       (classifier ((lex "embryo")))
					       (head ((lex "sac")))))))))))))))


;; Use of time circum with non-finite present-participle clause (with binder)
(def-test t414
  "Before explaining embryo sac formation, we must discover how to generate time adjuncts in SURGE."
  ((cat clause)
   (process ((type mental)
	     (lex "discover")
	     (transitive yes)
	     (deontic-modality "must")))
   (partic ((processor ((cat personal-pronoun)
			(person first)
			(number plural)))
	    (phenomenon
             ((cat clause)
              (mood infinitive)
              (binder ((lex "how")))
              (process ((type material) (lex "generate")))
              (partic ((agent ((semantics
                                ((index {^5 processor semantics index})))))
                       (affected ((cat common)
                                  (number plural)
                                  (definite no)
                                  (lex "adjunct")
                                  (classifier ((lex "time")))))))
              (circum ((in-loc ((cat trivial-proper)
                                (lex "SURGE")))))))))
   (circum
    ((time ((binder ((lex "before")))
            (position front)
            (cat clause)
            (mood present-participle)
            (controlled {^ partic processor})
            (process ((type mental) (lex "explain")))
            (partic
             ((processor
               ((semantics ((index {^6 partic processor semantics index})))))
              (phenomenon ((cat common)
                           (countable no)
                           (lex "formation")
                           (classifier ((cat noun-compound)
                                        (classifier ((lex "embryo")))
                                        (head ((lex "sac")))))))))))))))




;; ============================================================
;; Possessive relatives
;; ============================================================

(def-test t415
  "The man whose car I wash."
  ((cat np)
   (definite yes)
   (head === man)
   (qualifier ((cat clause)
	       (mood possessive-relative)
	       (scope {^ partic affected})
	       (proc ((type material) (lex "wash")))
	       (partic ((agent ((cat personal-pronoun)
				(person first) (number singular)))
			(affected ((cat common)
				   (lex "car")))))))))

(def-test t416
  "The man whose first two cars I washed."
  ((cat np)
   (definite yes)
   (head === man)
   (qualifier ((cat clause)
	       (mood possessive-relative)
	       (scope {^ partic affected})
	       (proc ((type material) (lex "wash") (tense past)))
	       (partic ((agent ((cat personal-pronoun)
				(person first) (number singular)))
			(affected ((cat common)
				   (definite yes)
				   (cardinal ((value 2) (digit no)))
				   (ordinal ((value 1) (digit no)))
				   (lex "car")))))))))

(def-test t417
  "The man the first two of whose cars I washed."
  ((cat np)
   (definite yes)
   (head === man)
   (qualifier
    ((cat clause)
     (mood possessive-relative)
     (scope {^ partic affected})
     (proc ((type material) (lex "wash") (tense past)))
     (partic ((agent ((cat personal-pronoun)
		      (person first) (number singular)))
	      (affected ((cat partitive)
			 (part ((cat common)
				(cardinal ((value 2)))
				(ordinal ((value 1)))))
			 (part-of ((definite yes)
				   (number plural)
				   (lex "car")))))))))))


;; Tricky here with the number plural.
(def-test t418
  "The soil, whose layers are topsoil and subsoil."
  ((cat common) (definite yes)
   (lex "soil")
   (qualifier
    ((cat clause) (restrictive no)
     (mood possessive-relative)
     (scope {^ partic carrier})
     (proc ((type ascriptive)))
     (partic ((carrier
	       ((cat common)
		(lex "layer")
		(number plural)))
	      (attribute
	       ((cat common) (complex conjunction)
		(distinct ~( ((cat common)
			      (countable no)
			      (lex "topsoil"))
			     ((cat common)
			      (countable no)
			      (lex "subsoil"))))))))))))


(def-test t419a
  "The bear, whose claws are sharp, is big."
  ((cat clause)
    (proc ((type ascriptive)))
    (partic
     ((attribute ((lex "big")))
      (carrier ((cat common)
		(lex "bear")
		(qualifier ((cat clause)
			    (mood possessive-relative)
			    (restrictive no)
			    (scope {^ partic carrier})
			    (proc ((type ascriptive)))
			    (partic ((carrier ((cat common)
					       (number plural)
					       (lex "claw")))
				     (attribute ((lex "sharp")))))
			    ))))))))

(def-test t419b
  "The bear bites people and the bear's claws are sharp."
  ((cat clause)
   (complex conjunction)
   (distinct
    ~(((cat clause)
       (proc ((type material) (lex "bite")))
       (partic ((agent ((cat common) (lex "bear")))
		(affected ((cat common)
			   (countable no)
			   (lex "people"))))))
      ((cat clause)
       (proc ((type ascriptive)))
       (partic ((carrier ((cat common)
			  (number plural)
			  (lex "claw")
			  (possessor ((cat common) (lex "bear")))))
		(attribute ((lex "sharp"))))))
       ))))

;; Conjunction of qualifiers with different scopes
(def-test t419
  "The bear, which bites people and whose claws are sharp, is big."
  ((cat clause)
    (proc ((type ascriptive)))
    (partic
     ((attribute ((lex "big")))
      (carrier ((cat common)
		(lex "bear")
		(qualifier ((cat clause)
			    (complex conjunction)
			    (distinct
			     ~(((cat clause)
				(restrictive no)
				(scope {^ partic agent})
				(proc ((type material)
				       (lex "bite")))
				(partic ((affected ((cat common)
						    (countable no)
						    (lex "people"))))))
			       ;; @todo
			       ;; should be (scope {partic carrier possessive})
			       ;; and infer the mood possessive-relative
			       ((cat clause)
				(mood possessive-relative)
				(restrictive no)
				(scope {^ partic carrier})
				(proc ((type ascriptive)))
				(partic ((carrier ((cat common)
						   (number plural)
						   (lex "claw")))
					 (attribute ((lex "sharp")))))
				)))))))))))

(def-test t420
  "The bear, whose leg is heavy and whose claws are sharp, is big."
  ((cat clause)
    (proc ((type ascriptive)))
    (partic
     ((attribute ((lex "big")))
      (carrier ((cat common)
		(lex "bear")
		(qualifier ((cat clause)
			    (complex conjunction)
			    (distinct
			     ~(((cat clause)
				(mood possessive-relative)
				(restrictive no)
				(scope {^ partic carrier})
				(proc ((type ascriptive)))
				(partic ((attribute ((lex "heavy")))
					 (carrier ((cat common)
						   (lex "leg"))))))
			       ((cat clause)
				(mood possessive-relative)
				(restrictive no)
				(scope {^ partic carrier})
				(proc ((type ascriptive)))
				(partic ((carrier ((cat common)
						   (number plural)
						   (lex "claw")))
					 (attribute ((lex "sharp")))))
				)))))))))))


;; ============================================================
;; Discourse Segment example
;; ============================================================

;; Should be (mood wh-nominal) for embedded clause.
;; not yet implemented.
(def-test t421a
  "I know what love is."
  ((cat clause)
   (process ((type mental)
             (lex "know")))
   (partic ((processor ((cat personal-pronoun)
                        (person first)))
            (phenomenon ((cat clause)
                         (mood bound-nominal)
                         (binder ((lex "what")))
                         (controlled {^ partic attribute})
                         (process ((type ascriptive)))
                         (partic ((carrier ((cat common)
                                            (lex "love")
                                            (countable no)))))))))))

(def-test t421
  "I am not a smart man, but I know what love is."
  ((cat ds)
   (subordinate ((cat clause)
		 (process ((type ascriptive)))
		 (polarity negative)
		 (partic ((carrier ((cat personal-pronoun)
				    (person first)))
			  (attribute ((cat common)
				      (definite no)
				      (describer === "smart")
				      (lex "man")))))))
   (connective ((lex "but")))
   (directive ((cat clause)
	       (process ((type mental)
			 (lex "know")))
	       (partic ((processor ((cat personal-pronoun)
				    (person first)))
			(phenomenon ((cat clause)
				     (mood bound-nominal)
				     (binder ((lex "what")))
				     (controlled {^ partic attribute})
				     (process ((type ascriptive)))
				     (partic ((carrier ((cat common)
							(lex "love")
							(countable no)))))))))))))


;; ============================================================
;; HTML examples
;; ============================================================

;; HTML tags go around constituents.
;; HTML tags can have parameters (like the A tag).
(def-test t422
  "<A HREF=\"http://www.film.com\">I am not a smart man, </A>but I know <B>what love is </B>."
  ((cat ds)
   (subordinate ((cat clause)
		 (html ((a ((href "http://www.film.com")))))
		 (process ((type ascriptive)))
		 (polarity negative)
		 (partic ((carrier ((cat personal-pronoun)
				    (person first)))
			  (attribute ((cat common)
				      (definite no)
				      (describer === "smart")
				      (lex "man")))))))
   (connective ((lex "but")))
   (directive ((cat clause)
	       (process ((type mental)
			 (lex "know")))
	       (partic ((processor ((cat personal-pronoun)
				    (person first)))
			(phenomenon ((cat clause)
				     (html ((b +)))
				     (mood bound-nominal)
				     (binder ((lex "what")))
				     (controlled {^ partic attribute})
				     (process ((type ascriptive)))
				     (partic ((carrier ((cat common)
							(lex "love")
							(countable no)))
					      (attribute ((cat ap)
							  (lex "sad")))))))))))))

(def-test t423-a
  "The recipe has 3 steps:"
  ((cat clause)
   (punctuation ((after ":")))
   (process ((type possessive)))
   (partic
    ((possessor ((cat common) (lex "recipe") (definite yes)))
     (possessed ((cat common) (lex "step") (definite no)
		 (cardinal ((value 3) (digit yes)))))))))


(def-test t423
  "The recipe has 3 steps: <OL><LI>start the fire </LI><LI>burn the olives </LI><LI>eat the pits </LI></OL>."
  ((cat list)
   (distinct
    ~(
      ((cat clause)
       (punctuation ((after ":")))
       (process ((type possessive)))
       (partic
	((possessor ((cat common) (lex "recipe") (definite yes)))
	 (possessed ((cat common) (lex "step") (definite no)
		     (cardinal ((value 3) (digit yes))))))))
      ((cat list)
       (html ((ol +)))
       (distinct
	~(
	  ((cat clause)
	   (html ((li +)))
	   (mood imperative)
	   (process ((type material) (lex "start")))
	   (partic ((agent ((cat personal-pronoun) (person second)))
		    (affected ((cat common) (lex "fire"))))))
	  ((cat clause)
	   (html ((li +)))
	   (mood imperative)
	   (process ((type material) (lex "burn")))
	   (partic ((agent ((cat personal-pronoun) (person second)))
		    (affected ((cat common) (lex "olive") (number plural))))))
	  ((cat clause)
	   (html ((li +)))
	   (mood imperative)
	   (process ((type material) (lex "eat")))
	   (partic
	    ((agent ((cat personal-pronoun) (person second)))
	     (affected ((cat common) (lex "pit") (number plural)))))))))))))


(def-test t424
  "This picture contains the secret <B>for a <I>happy </I>life </B>: <IMG SRC=\"happy.gif\" ALT=\"HAPPY LIFE SECRET\">."
  ((cat list)
   (distinct
    ~(
      ((cat clause)
       (proc ((type locative) (mode equative) (lex "contain")))
       (partic
	((identified ((cat common) (lex "picture") (distance near)))
	 (identifier ((cat common) (lex "secret")
		      (qualifier ((cat pp)
				  (prep ((lex "for")))
				  (html ((b +)))
				  (np ((cat common)
				       (definite no)
				       (lex "life")
				       (describer ((html ((i +)))
						   (lex "happy")))))))))))
       (punctuation ((after ":"))))
      ((cat phrase)
       (html ((img ((src "happy.gif")
		    (end-tag none)
		    (html-alt "HAPPY LIFE SECRET")))))
       (lex ""))))))


(def-test t425a
  "An annotation is a stored textual description of a software engineer's understanding of a concept."
  ((cat clause)
   (proc ((type ascriptive) (mode equative)))
   (partic
    ((identified ((cat common) (definite no) (lex "annotation")))
     (identifier
      ((cat common) (definite no)
       (lex "description")
       (describer ((cat list)
		   (distinct ~(((cat adj) (lex "stored"))
			       ((cat adj) (lex "textual"))))))
       (qualifier ((np ((cat common)
			(lex "understanding")
			(possessor ((cat common)
				    (definite no)
				    (classifier ((lex "software")))
				    (lex "engineer")))
			(qualifier ((np ((cat common)
					 (definite no)
					 (lex "concept")))))))))))))))


;; With a clausal representation of the "understanding"
(def-test t425b
  "An annotation is a stored textual description of a software engineer's understanding of a concept."
  ((cat clause)
   (proc ((type ascriptive) (mode equative)))
   (partic
    ((identified ((cat common) (definite no) (lex "annotation")))
     (identifier
      ((cat common) (definite no)
       (lex "description")
       (describer ((cat list)
		   (distinct ~(((cat adj) (lex "stored"))
			       ((cat adj) (lex "textual"))))))
       (qualifier ((np ((cat common)
			(lex "understanding")
			(possessor ((cat common)
				    (definite no)
				    (classifier ((lex "software")))
				    (lex "engineer")))
			(qualifier ((np ((cat common)
					 (definite no)
					 (lex "concept")))))))))))))))



;; ============================================================
;; Comparative/Superlative examples (Yael)
;; ============================================================

(def-test t450
  "The greater child."
  ((cat common)
   (lex "child")
   (describer ((cat ap)
               (inflected yes)
               (comparative yes)
               (lex "great")))))


(def-test t451
  "The most beautiful boy."
  ((cat common)
   (lex "boy")
   (describer ((cat ap)
               (lex "beautiful")
               (inflected no)
               (superlative yes)))))


(def-test t452
  "The best boy."
  ((cat common)
   (lex "boy")
   (describer ((cat ap)
               (lex "good")
               (inflected yes)
               (superlative yes)))))

(def-test t453
  "Bo is the greatest."
  ((cat clause)
   (proc ((type ascriptive) (mode equative)))
   (partic ((identified ((cat proper) (lex "Bo")))
	    (identifier ((cat common)
                         (head ((gap yes)))
                         (describer ((cat ap)
                                     (lex "great")
                                     (inflected yes)
                                     (superlative yes)))))))))


(def-test t600
  "Illusion holds to the key."
  ((cat clause)
   (proc ((type lexical)
	  (lex "hold")
	  (subcat ((1 ((cat np)))
		   (2 ((cat pp) (prep ((lex "to")))))
		   (1 {^3 lex-roles agent})
		   (2 ((np {^4 lex-roles held})))))))
   (lex-roles ((agent ((cat trivial-proper)
		       (lex "Illusion")))
	       (held ((cat np)
		      (definite yes)
		      (lex "key")))))))

;;"He who knows nothing is closer to the truth than he whose mind is filled with falsehoods and errors."
;; (Thomas Jefferson)

(def-test t601a
  "He who knows nothing is happy."
  ((cat clause)
   (proc ((type ascriptive)
          (mode attributive)))
   (partic
    ((carrier ((cat personal-pronoun)
               (gender masculine)
               (person third)
               (qualifier
                ((cat clause)
                 (proc ((type mental) (lex "know")))
                 (scope {^ partic processor})
                 (partic ((phenomenon ((cat pronoun)
                                       (lex "nothing")))))))))
     (attribute ((cat ap) (lex "happy")))))))

(def-test t601b
  "He whose mind is filled with falsehoods and errors is far from the truth."
  ((cat clause)
   (proc ((type locative)))
   (partic
    ((located
      ((cat personal-pronoun)
       (gender masculine)
       (qualifier
        ((cat clause)
         (mood possessive-relative)
         (scope {^ partic location})
         (focus {^ partic location})
         (proc ((type locative)
                (lex "fill")
                (passive-prep ((lex "with")))
                (mode equative)))
         (partic ((location ((cat np) (lex "mind")))
                  (located ((complex conjunction)
                            (cat np)
                            (number plural)
                            (distinct ~( ((lex "falsehood")
                                          (definite no)
                                          (number plural))
                                         ((lex "error")
                                          (definite no)
                                          (number plural))))))))))))
     (location ((cat ap)
                (lex "far")
                (qualifier
                 ((cat pp)
                  (prep ((lex "from")))
                  (np ((lex "truth") (definite yes)))))))))))

(def-test t601
  "He who knows nothing is closer to the truth than he whose mind is filled with falsehoods and errors."
  ((cat clause)
   (complex conjunction)
   (conjunction ((cat conj) (lex "than")))
   (distinct
    ~( ((cat clause)
        (proc ((type ascriptive)
               (mode attributive)))
        (partic
         ((carrier ((cat personal-pronoun)
                    (gender masculine)
                    (person third)
                    (qualifier
                     ((cat clause)
                      (proc ((type mental) (lex "know")))
                      (scope {^ partic processor})
                      (partic ((phenomenon ((cat pronoun)
                                            (lex "nothing")))))))))
          (attribute ((cat ap)
                      (lex "close")
                      (inflected yes)
                      (comparative yes)
                      (qualifier
                       ((cat pp)
                        (prep ((lex "to")))
                        (np ((lex "truth") (definite yes))))))))))

       ;; than as conjunction
       ((cat clause)
        (proc ((type locative))) ;; ellipsis obtained by complex
        (partic
         ((located
           ((cat personal-pronoun)
            (gender masculine)
            (qualifier
             ((cat clause)
              (mood possessive-relative)
              (scope {^ partic location})
              (focus {^ partic location})
              (proc ((type locative)
                     (lex "fill")
                     (passive-prep ((lex "with")))
                     (mode equative)))
              (partic ((location ((cat np) (lex "mind")))
                       (located ((complex conjunction)
                                 (cat np)
                                 (number plural)
                                 (distinct ~( ((lex "falsehood")
                                               (definite no)
                                               (number plural))
                                              ((lex "error")
                                               (definite no)
                                               (number plural))))))))))))
          (location
           ((gap yes) ;; ellipsis of obj-comp is not computed - must specify
            (cat ap)
            (lex "close")
            (qualifier ((cat pp)
                        (prep ((lex "to")))
                        (np ((lex "truth") (definite yes))))))))))))))

(def-test t602a
  "He is tall."
  ((cat clause)
   (proc ((type ascriptive) (mode attributive)))
   (partic ((carrier ((cat personal-pronoun) (gender masculine)))
            (attribute ((cat ap) (lex "tall")))))))

(def-test t602b
  "He is taller."
  ((cat clause)
   (proc ((type ascriptive) (mode attributive)))
   (partic ((carrier ((cat personal-pronoun) (gender masculine)))
            (attribute ((cat ap) (lex "tall")
                        (comparative yes)
                        (inflected yes)))))))

(def-test t602c
  "He is less tall."
  ((cat clause)
   (proc ((type ascriptive) (mode attributive)))
   (partic ((carrier ((cat personal-pronoun) (gender masculine)))
            (attribute ((cat ap) (lex "tall")
                        (polarity negative)
                        (comparative yes)
                        (inflected yes)))))))

(def-test t602d
  "He is as tall."
  ((cat clause)
   (proc ((type ascriptive) (mode attributive)))
   (partic ((carrier ((cat personal-pronoun) (gender masculine)))
            (attribute ((cat ap) (lex "tall")
                        (polarity equal)
                        (comparative yes)
                        (inflected yes)))))))

(def-test t602e
  "He is less tall than John."
  ((cat clause)
   (proc ((type ascriptive) (mode attributive)))
   (partic ((carrier ((cat personal-pronoun) (gender masculine)))
            (attribute ((cat ap) (lex "tall")
                        (polarity negative)
                        (comparative yes)
                        (inflected yes)
                        (comparison ((np ((cat trivial-proper)
                                          (lex "John")))))))))))

(def-test t602f
  "He is as tall as John."
  ((cat clause)
   (proc ((type ascriptive) (mode attributive)))
   (partic ((carrier ((cat personal-pronoun) (gender masculine)))
            (attribute ((cat ap) (lex "tall")
                        (polarity equal)
                        (comparative yes)
                        (inflected yes)
                        (comparison ((np ((cat trivial-proper)
                                          (lex "John")))))))))))

(def-test t602g
  "He is the tallest of all men."
  ((cat clause)
   (proc ((type ascriptive) (mode equative)))
   (partic ((identified ((cat personal-pronoun) (gender masculine)))
            (identifier ((cat ap)
                         (lex "tall")
                         (polarity positive)
                         (superlative yes)
                         (inflected yes)
                         (comparison ((np ((cat common)
                                           (number plural)
                                           (definite no)
                                           (total +)
                                           (lex "man")))))))))))

(def-test t602h
  "The tallest man moved."
  ((cat clause)
   (proc ((type material) (effective no) (lex "move")))
   (tense past)
   (partic
    ((agent
      ((cat np)
       (lex "man")
       (describer
        ((lex "tall")
         (polarity positive)
         (superlative yes)
         (inflected yes)))))))))

(def-test t603
  "Ten filmmakers whose names you need to know."
  ((cat np)
   (cardinal ((value 10) (digit no)))
   (definite no)
   (lex "filmmaker")
   (qualifier
    ((cat clause)
     (mood possessive-relative)
     (scope {^ partic phenomenon})
     (proc ((type mental) (lex "know")))
     (deontic-modality "need to")
     (partic ((processor ((cat personal-pronoun) (person second)))
              (phenomenon ((cat common) (lex "name") (number plural)))))))))

(def-test t604a
  "He is on his side."
  ((cat clause)
   (proc ((type locative)))
   (partic ((located ((cat personal-pronoun) (gender masculine)))
            (location ((cat pp)
                       (prep ((lex "on")))
                       (np ((cat common)
                            (lex "side")
                            (possessor ((cat personal-pronoun)
                                        (gender masculine)))))))))))

(def-test t604b
  "The man whose side he is on."
  ((cat common)
   (lex "man")
   (qualifier
    ((cat clause)
     (mood possessive-relative)
     (scope {^ partic location np})
     (proc ((type locative)))
     (partic ((located ((cat personal-pronoun) (gender masculine)))
              (location ((cat pp)
                         (prep ((lex "on")))
                         (np ((cat common)
                              (lex "side")
                              (possessor
                               ((cat personal-pronoun)
                                (gender masculine)))))))))))))

;; @TODO: Allow relative-clauses as pronominal.
;; - I know what you think.
;; - I know where you are going.
;; - I know who thinks this.
;; - What you do is what counts.
;; - Which way you decide to take leads to where you end up.
(def-test t604
  "We know whose side he is on."
  ((cat clause)
   (proc ((type mental) (lex "know")))
   (partic ((processor ((cat personal-pronoun)
                        (person first) (number plural)))
            (phenomenon
             ((cat personal-pronoun)
              (head ((gap yes)))
              (qualifier
               ((cat clause)
                (mood possessive-relative)
                (scope {^ partic location np})
                (proc ((type locative)))
                (partic ((located ((cat personal-pronoun) (gender masculine)))
                         (location ((cat pp)
                                    (prep ((lex "on")))
                                    (np ((cat common)
                                         (lex "side")
                                         (possessor
                                          ((cat personal-pronoun)
                                           (gender masculine)))))))))))))))))

(def-test t605a
  "Suspected burglar did not really know anything."
  ((cat clause)
   (proc ((type mental) (lex "know")))
   (tense past)
   (polarity negative)
   (adverb ((lex "really")))
   ;; (pred-modif ((manner ((cat adv) (lex "really")))))
   (partic ((processor ((cat common)
                        (lex "burglar")
                        (definite no)
                        (determiner ((gap yes))) ;; News genre
                        (describer ((cat verb) (lex "suspect")))))
            (phenomenon ((cat pronoun)
                         (lex "anything")))))))

(def-test t605b
  "A burglar was breaking into his house."
  ((cat clause)
   (proc ((type lexical) (lex "break")
          (subcat ((1 ((cat np)))
                   (2 ((cat pp) (prep ((lex "into")))))
                   (1 {^3 lex-roles agent})
                   (2 ((np {^4 lex-roles place})))))))
   (tense past-progressive)
   (lex-roles ((agent ((cat common)
                       (lex "burglar")
                       (definite no)))
               (place ((cat common)
                       (lex "house")
                       (possessor ((cat personal-pronoun)
                                   (gender masculine)))))))))

(store-verbs '(("break" "breaks" "broke" "breaking" "broken")))

(def-test t605c
  "The man whose house a burglar was breaking into."
  ((cat common)
   (lex "man")
   (definite yes)
   (qualifier
    ((cat clause)
     (mood possessive-relative)
     (scope {^ lex-roles place})
     (tense past-progressive)
     (proc ((type lexical) (lex "break")
            (subcat ((1 ((cat np)))
                     (2 ((cat pp)
                         (prep ((lex "into")))))
                     (1 {^3 lex-roles agent})
                     (2 ((np {^4 lex-roles place})))))))
     (lex-roles ((agent ((cat common)
                         (lex "burglar")
                         (definite no)))
                 (place ((cat common)
                         (lex "house")
                         (possessor ((cat personal-pronoun)
                                     (gender masculine)))))))))))

;; break-in as composite [material+locative] / effective no.
(def-test t605d
  "The man whose house a burglar was breaking into."
  ((cat common)
   (lex "man")
   (definite yes)
   (qualifier
    ((cat clause)
     (mood possessive-relative)
     (scope {^ partic location np})
     (tense past-progressive)
     (proc ((type composite)
            (relation-type locative)
            (effective no)
            (lex "break")))
     (partic ((located ((cat common)
                        (lex "burglar")
                        (definite no)))
              (location
               ((cat pp)
                (prep ((lex "into")))
                (np ((cat common)
                     (lex "house")
                     (possessor ((cat personal-pronoun)
                                 (gender masculine)))))))))))))

(def-test t605
  "Suspected burglar did not really know whose house he was breaking into."
  ((cat clause)
   (proc ((type mental) (lex "know")))
   (tense past)
   (polarity negative)
   (adverb ((lex "really")))
   (partic
    ((processor ((cat common)
                 (lex "burglar")
                 (semantics ((index ((concept burglar)
                                     (gender masculine)))))
                 (definite no)
                 (determiner ((gap yes))) ;; News genre
                 (describer ((cat verb) (lex "suspect")))))
     (phenomenon
      ((cat pronoun)
       (head ((gap yes)))
       (qualifier
        ((cat clause)
         (mood possessive-relative)
         (scope {^ partic location np})
         (tense past-progressive)
         (proc ((type composite)
                (relation-type locative)
                (effective no)
                (lex "break")))
         (partic
          ((located
            ((cat personal-pronoun)
             (semantics ((index {^6 processor semantics index})))))
           (location
            ((cat pp)
             (prep ((lex "into")))
             (np ((cat common)
                  (lex "house")
                  (possessor ((cat personal-pronoun)
                              (gender masculine)))))))))))))))))

(def-test t606a
  "It is impossible to say whose voice that was."
  ((cat clause)
   (proc ((type ascriptive)))
   (partic
    ((carrier ((cat pronoun)))
     (attribute
      ((cat ap)
       (lex "impossible")
       (qualifier
        ((cat clause)
         (mood infinitive)
         (proc ((type mental) (lex "say")))
         (partic ((phenomenon
                   ((cat pronoun)
                    (head ((gap yes)))
                    (qualifier
                     ((cat clause)
                      (mood possessive-relative)
                      (scope {^ partic identifier})
                      (tense past)
                      (proc ((type ascriptive) (mode equative)))
                      (partic ((identified ((cat demonstrative-pronoun)
                                            (distance far)))
                               (identifier ((cat common)
                                            (lex "voice")))))))))))))))))))


(def-test t606b
  "He made some compelling points."
  ((cat clause)
   (proc ((type mental) (lex "make")))
   (tense past)
   (partic ((processor ((cat personal-pronoun) (gender masculine)))
            (phenomenon ((cat common)
                         (number plural)
                         (definite no)
                         (lex "point")
                         (total none)
                         (selective yes)
                         (describer ((cat verb) (lex "compell")
                                     (modifier-type subjective)))))))))

(def-test t606
  "It is impossible to say whose voice that was, but he made some compelling points."
  ((cat clause)
   (complex conjunction)
   (conjunction ((lex "but")))
   (distinct
    ~(
      ((cat clause)
       (punctuation ((after ",")))
       (proc ((type ascriptive)))
       (partic
        ((carrier ((cat pronoun)))
         (attribute
          ((cat ap)
           (lex "impossible")
           (qualifier
            ((cat clause)
             (mood infinitive)
             (proc ((type mental) (lex "say")))
             (partic ((phenomenon
                       ((cat pronoun)
                        (head ((gap yes)))
                        (qualifier
                         ((cat clause)
                          (mood possessive-relative)
                          (scope {^ partic identifier})
                          (tense past)
                          (proc ((type ascriptive) (mode equative)))
                          (partic ((identified ((cat demonstrative-pronoun)
                                                (distance far)))
                                   (identifier ((cat common)
                                                (lex "voice"))))))))))))))))))
      ((cat clause)
       (proc ((type mental) (lex "make")))
       (tense past)
       (partic ((processor ((cat personal-pronoun) (gender masculine)))
                (phenomenon ((cat common)
                             (number plural)
                             (definite no)
                             (lex "point")
                             (total none)
                             (selective yes)
                             (describer ((cat verb) (lex "compell")
                                         (modifier-type subjective))))))))
      ))))

(def-test t607a
  "The husband died at Hillsborough."
  ((cat clause)
   (proc ((type material) (agentive no) (lex "die")))
   (tense past)
   (partic ((affected ((cat common)
                       (lex "husband")))))
   (circum ((location ((cat pp)
                       (prep ((lex "at")))
                       (np ((cat trivial-proper)
                            (lex "Hillsborough")))))))))

(def-test t607
  "Mum whose husband died at Hillsborough pleads with jury."
  ((cat clause)
   (proc ((type verbal) (lex "plead")))
   (partic ((sayer ((cat common)
                    (lex "mum")
                    (determiner ((gap yes)))
                    (definite no)
                    (qualifier
                     ((cat clause)
                      (mood possessive-relative)
                      (scope {^ partic affected})
                      (proc ((type material) (agentive no) (lex "die")))
                      (tense past)
                      (partic ((affected ((cat common)
                                          (lex "husband")))))
                      (circum ((location
                                ((cat pp)
                                 (prep ((lex "at")))
                                 (np ((cat trivial-proper)
                                          (lex "Hillsborough")))))))))))
            (addressee ((cat pp)
                        (prep ((lex "with")))
                        (np ((cat common)
                             (lex "jury")
                             (definite no)
                             (countable no)))))))))

(def-test t608a
  "These are woods."
  ((cat clause)
   (proc ((type ascriptive) (mode equative)))
   (partic ((identified ((cat demonstrative-pronoun)
                         (distance near)
                         (number plural)))
            (identifier ((cat common)
                         (lex "wood")
                         (number plural)
                         (definite no)))))))

(def-test t608b
  "The man whose woods these are."
  ((cat common)
   (lex "man")
   (definite yes)
   (qualifier ((cat clause)
               (mood possessive-relative)
               (scope {^ partic identifier})
               (proc ((type ascriptive) (mode equative)))
               (partic ((identified ((cat demonstrative-pronoun)
                                     (distance near)
                                     (number plural)))
                        (identifier ((cat common)
                                     (lex "wood")
                                     (number plural)))))))))

(def-test t608c
  "I know whose woods these are."
  ((cat clause)
   (proc ((type mental) (lex "know")))
   (partic ((processor ((cat personal-pronoun) (person first)))
            (phenomenon
             ((cat pronoun)
              (head ((gap yes)))
              (qualifier
               ((cat clause)
                (mood possessive-relative)
                (scope {^ partic identifier})
                (proc ((type ascriptive) (mode equative)))
                (partic ((identified ((cat demonstrative-pronoun)
                                      (distance near)
                                      (number plural)))
                         (identifier ((cat common)
                                      (lex "wood")
                                      (number plural)))))))))))))

(def-test t608d
  "I think I know whose woods these are."
  ((cat clause)
   (proc ((type mental) (lex "think")))
   (partic
    ((processor ((cat personal-pronoun) (person first)))
     (phenomenon
      ((cat clause)
       (mood bound-nominal)
       (binder ((gap yes)))
       (proc ((type mental) (lex "know")))
       (partic ((processor ((cat personal-pronoun) (person first)))
                (phenomenon
                 ((cat pronoun)
                  (head ((gap yes)))
                  (qualifier
                   ((cat clause)
                    (mood possessive-relative)
                    (scope {^ partic identifier})
                    (proc ((type ascriptive) (mode equative)))
                    (partic ((identified ((cat demonstrative-pronoun)
                                          (distance near)
                                          (number plural)))
                             (identifier ((cat common)
                                          (lex "wood")
                                          (number plural)))))))))))))))))

(def-test t609
  "Man stabbed by relative whose name he did not know."
  ((cat common)
   (lex "man")
   (definite no)
   (determiner ((gap yes)))
   (semantics ((index ((concept man1)))))
   (qualifier
    ((cat clause)
     (mood past-participle)
     (proc ((type material) (lex "stab")))
     (controlled {^ partic affected})
     (partic
      ((affected ((semantics ((index {^5 semantics index})))))
       (agent
        ((cat common)
         (lex "relative")
         (definite no)
         (determiner ((gap yes)))
         (qualifier
          ((cat clause)
           (mood possessive-relative)
           (scope {^ partic phenomenon})
           (tense past)
           (polarity negative)
           (proc ((type mental) (lex "know")))
           (partic ((processor ((cat personal-pronoun) (gender masculine)))
                    (phenomenon ((cat common)
                                 (lex "name")))))))))))))))

(def-test t610a
  "There is a man I have come to know."
  ((cat clause)
   (proc ((type existential)))
   (partic
    ((located
      ((cat common)
       (lex "man")
       (definite no)
       (qualifier
        ((cat clause)
         (tense present-perfect)
         (relative-marker ((gap yes)))
         (scope {^ lex-roles soa partic phenomenon})
         (proc ((type lexical)
                (lex "come")
                (subcat ((1 ((cat np)))
                         (2 ((cat clause) (mood infinitive)))
                         (1 {^3 lex-roles agent})
                         (2 {^3 lex-roles soa})))))
         (lex-roles
          ((agent ((cat personal-pronoun) (person first)))
           (soa
            ((proc ((type mental) (lex "know")))
             (partic
              ((phenomenon
                ((semantics ((index {^7 semantics index})))))))))))))))))))

(def-test t610b
  "His hands are hard like ancient stone."
  ((cat clause)
   (proc ((type ascriptive)))
   (partic ((carrier ((cat common)
                      (lex "hand")
                      (number plural)
                      (possessor ((cat personal-pronoun)
                                  (gender masculine)))))
            (attribute
             ((cat ap)
              (lex "hard")
              (qualifier
               ((cat pp)
                (prep ((lex "like")))
                (np ((cat common)
                     (lex "stone")
                     (countable no)
                     (describer ((cat ap)
                                 (lex "ancient")))))))))))))

(def-test t610c
  "His eyes are gentle like a child's just before sleep."
  ((cat clause)
   (proc ((type ascriptive)))
   (partic
    ((carrier ((cat common) (lex "eye") (number plural)
               (possessor ((cat personal-pronoun) (gender masculine)))))
     (attribute
      ((cat ap)
       (lex "gentle")
       (qualifier
        ((cat pp)
         (prep ((lex "like")))
         (np ((cat common)
              (head ((gap yes)))
              (possessor ((cat common)
                          (lex "child")
                          (definite no)))
              (qualifier ((cat pp)
                          (prep ((lex "before")
                                 (adverb ((lex "just")))))
                          (np ((cat common)
                               (countable no)
                               (lex "sleep")))))))))))))))

(def-test t610
  "There is a man I have come to know, whose hands are hard like ancient stone, and whose eyes are gentle like a child's just before sleep."
  ((cat clause)
   (proc ((type existential)))
   (partic
    ((located
      ((cat common)
       (lex "man")
       (definite no)
       (qualifier
        ((cat clause)
         (complex conjunction)
         (distinct
          ~(
            ((cat clause)
             (tense present-perfect)
             (relative-marker ((gap yes)))
             (scope {^ lex-roles soa partic phenomenon})
             (proc ((type lexical)
                    (lex "come")
                    (subcat ((1 ((cat np)))
                             (2 ((cat clause) (mood infinitive)))
                             (1 {^3 lex-roles agent})
                             (2 {^3 lex-roles soa})))))
             (lex-roles
              ((agent ((cat personal-pronoun) (person first)))
               (soa
                ((proc ((type mental) (lex "know")))
                 (partic
                  ((phenomenon
                    ((semantics ((index {^7 semantics index}))))))))))))

            ((cat clause)
             (punctuation ((after ",")))
             (mood possessive-relative)
             (scope {^ partic carrier})
             (proc ((type ascriptive)))
             (partic ((carrier ((cat common)
                                (lex "hand")
                                (number plural)))
                      (attribute
                       ((cat ap)
                        (lex "hard")
                        (qualifier
                         ((cat pp)
                          (prep ((lex "like")))
                          (np ((cat common)
                               (lex "stone")
                               (countable no)
                               (describer ((cat ap)
                                           (lex "ancient"))))))))))))

            ((cat clause)
             (mood possessive-relative)
             (scope {^ partic carrier})
             (proc ((type ascriptive)))
             (partic
              ((carrier ((cat common) (lex "eye") (number plural)
                         (possessor ((cat personal-pronoun)
                                     (gender masculine)))))
               (attribute
                ((cat ap)
                 (lex "gentle")
                 (qualifier
                  ((cat pp)
                   (prep ((lex "like")))
                   (np ((cat common)
                        (head ((gap yes)))
                        (possessor ((cat common)
                                    (lex "child")
                                    (definite no)))
                        (qualifier ((cat pp)
                                    (prep ((lex "before")
                                           (adverb ((lex "just")))))
                                    (np ((cat common)
                                         (countable no)
                                         (lex "sleep"))))))))))))))
            ))))))))))

(def-test t611
  "At least, they will know by whose boat he was crushed."
  ((cat clause)
   (tense future)
   (proc ((type mental) (lex "know")))
   (circum ((concession ((cat adv) (lex "at least")
                         (position front)))))
   (partic ((processor ((cat personal-pronoun) (number plural)))
            (phenomenon
             ((cat pronoun)
              (head ((gap yes)))
              (qualifier
               ((cat clause)
                (tense past)
                (mood possessive-relative)
                (scope {^ partic agent})
                ;; (focus {^ partic affected})
                (proc ((type material)
                       (voice passive)
                       (lex "crush")))
                (partic ((agent ((cat common)
                                 (lex "boat")))
                         (affected ((cat personal-pronoun)
                                    (gender masculine)))))))))))))

;; =========================
;; Compound cardinals
;;
;; Key features:
;; comparative [none/bound/comparative]
;; orientation [none/+/-/=]
;; adverb      [none/adv]

(def-test t620
  "Five meters."
  ((cat measure)
   (quantity ((value 5)))
   (unit ((lex "meter")))))

(def-test t620a
  ("At least five meters."
   "Over five meters.")
  ((cat measure)
   (quantity ((value 5)
              (cat compound-cardinal)
              (comparative bound)
              (orientation +)))
   (unit ((lex "meter")))))

(def-test t620b
  "Over five meters."
  ((cat measure)
   (quantity ((value 5)
              (cat compound-cardinal)
              (pre-comp ((lex "over")))))
   (unit ((lex "meter")))))

(def-test t620c
  "About five meters."
  ((cat measure)
   (quantity ((value 5)
              (cat compound-cardinal)
              (adverb ((lex "about")))))
   (unit ((lex "meter")))))

(def-test t620d
  "More than five meters."
  ((cat measure)
   (quantity ((value 5)
              (cat compound-cardinal)
              (comparative comparative)
              (orientation +)))
   (unit ((lex "meter")))))

(def-test t620e
  "Less than five meters."
  ((cat measure)
   (quantity ((value 5)
              (cat compound-cardinal)
              (comparative comparative)
              (orientation -)))
   (unit ((lex "meter")))))

(def-test t620f
  "As many as five meters."
  ((cat measure)
   (quantity ((value 5)
              (cat compound-cardinal)
              (comparative bound)
              (orientation =)))
   (unit ((lex "meter")))))

(def-test t620g
  "Much more than five meters."
  ((cat measure)
   (quantity ((value 5)
              (cat compound-cardinal)
              (adverb ((lex "much")))
              (comparative comparative)
              (orientation +)))
   (unit ((lex "meter")))))

(def-test t620h
  "Not as many as five meters."
  ((cat measure)
   (quantity ((value 5)
              (cat compound-cardinal)
              (adverb ((lex "not")))
              (comparative bound)
              (orientation =)))
   (unit ((lex "meter")))))

(def-test t620i
  "No more than five meters."
  ((cat measure)
   (quantity ((value 5)
              (cat compound-cardinal)
              (adverb ((lex "no")))
              (comparative comparative)
              (orientation +)))
   (unit ((lex "meter")))))

(def-test t620j
  "No more than about five meters."
  ((cat measure)
   (quantity ((cat compound-cardinal)
              (numeral ((cat compound-cardinal)
                        (value 5)
                        (adverb ((lex "about")))))
              (adverb ((lex "no")))
              (comparative comparative)
              (orientation +)))
   (unit ((lex "meter")))))

(def-test t620j
  "Not much more than approximately five meters."
  ((cat measure)
   (quantity ((cat compound-cardinal)
              (numeral ((cat compound-cardinal)
                        (adverb ((lex "not")))
                        (numeral ((cat compound-cardinal)
                                  (numeral ((cat compound-cardinal)
                                            (value 5)
                                            (adverb ((lex "approximately")))))
                                  (adverb ((lex "much")))
                                  (comparative comparative)
                                  (orientation +)))))))
   (unit ((lex "meter")))))
