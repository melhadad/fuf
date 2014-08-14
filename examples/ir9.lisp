;;; -*- Mode:Lisp; Syntax:Common-Lisp; -*-
;;; ------------------------------------------------------------
;;; File        : IR9.LISP
;;; Description : Sample inputs to test GR9.
;;; Author      : Michael Elhadad and Jacques Robin
;;; Created     : Aug 14 91
;;; Modified    : Sep 17 91 Added relative examples
;;; Language    : Common Lisp
;;; ------------------------------------------------------------

(in-package "FUG5")

(defun isetup9 ()
  (clear-tests)

  (def-test t1
      "This car is expensive."
    ((cat clause)
     (proc ((type ascriptive)
	    (mode attributive))) ;; default
     (partic ((carrier ((lex "car")
			(cat common)
			(distance near)))
	      (attribute === "expensive")))))


  (def-test t2
      "John gives a blue book to Mary."
    ((cat clause)
     (proc ((type composite)
	    (relation-type possessive)
	    (mode attributive) ;; default
	    (agentive yes) ;; default
	    (effective yes) ;; default
	    (effect-type dispositive) ;; default
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

  (def-test t3
      "A science book is given by John to Mary."
    ((cat clause)
     (proc ((type composite) 
	    (relation-type possessive)
	    (voice passive)
	    (lex "give")
	    (dative-prep "to")))
     (dative-move no)
     (partic ((agent ((cat proper) (lex "John")))
	      (affected ((cat proper) (lex "Mary")))
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
  (def-test t7
      "Mary is thrown a heavy ball by John."
    ((cat clause)
     (proc ((type composite)
	    (relation-type possessive)
	    (dative-prep "to")
	    (lex "throw")
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
			 (lex "sig-display marker")))
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

  ;; @@ Control case - same as "I force you to eat" (explain absence of "for" in infinitive clause)
  ;; @@ See correct implementation in surge/inputs/ir.l[t13]
  #+ignore
  (def-test t13
      "The power level's increasing causes the sig-display marker to move to the right."
    ((cat clause)
     (proc ((type material) ;; mmmh really ?
	    (lex "cause")
	    (subject-clause present-participle)
	    (object-clause infinitive)))
     (partic 
      ((agent ((cat clause)
	       (proc ((type material)
		      (lex "increase")
		      (agentive no)))
	       (partic ((affected ((cat common) 
				   (classifier ((lex "power")))
				   (lex "level")))))))
       (affected ((cat clause)
		  (proc ((type composite)
			 (relation-type locative)
			 (agentive no)
			 (lex "move")))
		  (partic 
		   ((affected ((cat common)
			       (classifier ((lex "sig-display")))
			       (lex "marker")))
		    (located {^ affected})
		    (location ((cat pp)
			       (prep === "to")
			       (np ((cat common) 
				    (lex "right")))))))))))))


  (def-test t14
      "For her to do it."
    ((cat clause)
     (mood infinitive)
     (proc ((mood infinitive)
	    (lex "do")
	    (type material)))
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


  (def-test t15bis
      "For her to do it must be a bold statement."
    ((cat clause)
     (epistemic-modality inference)
     (proc ((type ascriptive)
	    (mode attributive)))
     (partic ((carrier ((cat clause)
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
      "The man whom I know."
    ((cat common)
     (head === "man")
     (animate yes)
     (qualifier ((cat clause)
		 (restrictive no)
		 (scope ((role phenomenon) (animate yes))) 
		 (proc ((type mental)
			(lex "know")))
		 (participants ((processor ((cat personal-pronoun)
					    (person first)))))))))

  (def-test t16bis
      "The man whom I may know."
    ((cat common)
     (head === "man")
     (animate yes)
     (qualifier ((cat clause)
		 (epistemic-modality "may")
		 (restrictive no)
		 (scope ((role phenomenon) (animate yes))) 
		 (proc ((type mental)
			(lex "know")
			(transitive yes)))
		 (participants
		  ((processor ((cat personal-pronoun)
			       (person first)))))))))

  (def-test t17
      "The man that I know."
    ((cat common)
     (head === "man")
     (animate yes)
     (qualifier ((cat clause)
		 (restrictive yes) ;; ***** here is the difference
		 (scope ((role phenomenon))) 
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
		 (scope ((role processor)))
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
		 (restrictive yes)
		 (scope ((role to-loc)))
		 (proc ((type material)
			(lex "move")
			(agentive no)))
		 (partic ((affected ((cat common) (head === "marker")))))))))



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



  ;; New treatment of lists: complex can be apposition or conjunction
  (def-test t24
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
	 (partic ((affected ((cat common) (head === "hammer"))))))
	((proc ((type material) (lex "hit")))
	 (partic ((affected ((cat common) (head === "nail"))))))))))


  (def-test t26bis
      "Take the hammer."
    ((cat clause)
     (mood imperative)
     (proc ((type material) (lex "take")))
     (partic ((affected ((cat common) (head === "hammer")))))))

  (def-test t26ter
      "First, take the hammer."
    ((cat clause)
     (mood imperative)
     (time-relater === "first")
     (proc ((type material) (lex "take")))
     (partic ((affected ((cat common) (head === "hammer")))))))
		   

  (def-test t27
      "First, take the hammer and then, hit the nail."
    ((cat clause)
     (complex conjunction)
     (common ((mood imperative)))
     (distinct
      ~(((proc ((type material) (lex "take")))
	 (time-relater === "first")
	 (partic ((affected ((cat common) (head === "hammer"))))))
	((proc ((type material) (lex "hit")))
	 (time-relater === "then")
	 (partic ((affected ((cat common) (head === "nail"))))))))))


  ;; t28 fails because epistemic-modality is not compatible with imperative.
  (def-test t28
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
     (partic ((affected ((cat common) (head === "box")))))
     (circum ((purpose ((proc ((type material) 
			       (lex "install")))
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
     (circum ((instrument ((cat common) (head === "hammer")))))))


  (def-test t34
      "Hit the nail using a hammer."
    ((cat clause)
     (mood imperative)
     (proc ((type material)
	    (lex "hit")
	    (instrument-prep "using")))
     (partic ((affected ((cat common) (head === nail)))))
     (circum ((instrument ((cat common) (definite no) (head === "hammer")))))))


  (def-test t35
      "Take the nail off using a screwdriver."
    ((cat clause)
     (mood imperative)
     (proc ((type material)
	    (lex "take")
	    (particle "off")
	    (instrument-prep "using")))
     (partic ((affected ((cat common) (head === nail)))))
     (circum ((instrument ((cat common)
			   (head === screwdriver)
			   (definite no)))))))


  (def-test t36
      "The nail is taken off using a screwdriver."
    ((cat clause)
     (proc ((type material)
	    (voice passive)
	    (lex "take")
	    (particle "off")
	    (instrument-prep "using")))
     (partic ((affected ((cat common) (head === nail)))))
     (circum ((instrument ((cat common)
			   (head === screwdriver)
			   (definite no)))))))


  ;; another way of specifying the prep for the role instrument.
  (def-test t37
      "Take the nail off using a screwdriver."
    ((cat clause)
     (mood imperative)
     (proc ((type material)
	    (lex "take")
	    (particle "off")))
     (partic ((affected ((cat common) (head === nail)))))
     (circum ((instrument ((cat common) 
			   (prep "using")
			   (head === screwdriver)
			   (definite no)))))))


  ;; Add an irregular verb to the lexicon with store-verbs
  ;; Treatment of non-movable particles as a single string (sort of hack)
  (store-verbs '( ("line up" "lines up" "lined up" "lining up" "lined up")) )
  (def-test t38
      "The battery lines up with the compartment."
    ((cat clause)
     (proc ((type locative) (lex "line up")))
     (partic ((location ((cat pp)
			 (prep === "with")
			 (np ((cat common) (head === compartment)))))
	      (located ((cat common) (head === battery)))))))


  ;; Test reason role
  (def-test t39
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
     (circum ((reason ((cat personal-pronoun)
		       (gender masculine)
		       (person second)
		       (number singular)))))))


  ;; Test reason role with a clause
  (def-test t40
      "He does it because he likes you."
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
     (circum ((reason ((cat clause)
		       (process ((type mental)
				 (lex "like")))
		       (partic ((processor {partic agent})
				(phenomenon ((cat personal-pronoun)
					     (person second)
					     (number singular)))))))))))


  ;; Test reason role
  (def-test t41
      "He scored thirty-nine points for the Lakers."
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
			 (cardinal ((value 39) (digit no)))
			 (lex "point")))))
     (circum ((behalf ((cat proper)
		       (lex "the Lakers")))))))


  (def-test t42
      "The holding battery cover plate covers the holding battery compartment."
    ((cat clause)
     (proc ((type locative) 
	    (mode equative)
	    (cat verb-group)
	    (lex "cover")))
     (partic ((identified ((cat np)
			   (head ((cat noun)
				  (lex "holding battery cover plate")))))
	      (identifier ((cat np)
			   (head ((cat noun)
				  (lex "holding battery compartment")))))))))


  (def-test t43
      "The holding battery compartment is in the back of the RT at the bottom left corner."
    ((cat clause)
     (proc ((type locative) (mode attributive)))
     (partic
      ((carrier ((cat np)
		 (lex "holding battery compartment")))
       (attribute ((cat pp)
		   (complex apposition)
		   (restrictive yes)
		   (distinct 
		    ~(((prep ((cat prep) (lex "in")))
		       (np 
			((cat np)
			 (head ((cat noun) (lex "back")))
			 (qualifier ((cat pp)  
				     (np ((cat np)
					  (head ((cat noun)
						 (lex "RT"))))))))))
		      ((prep ((cat prep) (lex "at")))
		       (np ((cat np)
			    (describer ((cat list) 
					(elements ~(((cat ap) (lex "bottom"))
						    ((cat ap) (lex "left"))))))
			    (head ((cat noun) (lex "corner"))))))))))))))


  ;; TEST: try your own syntactic analysis of this one and come up with 3
  ;; reasons why it is inferior to this one.
  ;; Send answers to djk@cs.columbia.edu
  (def-test t44
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
		       (np ((cat np)
			    (head ((cat noun)
				   (lex "cut-away view")))))))))))))))


  ;; Show that conjunction of different cats fails
  (def-test t45
      "<fail>"
    ((cat np)
     (complex conjunction)
     (distinct ~( ((cat common)
		   (head === "box"))
		  ((cat ap)
		   (head === "blue")) ))))



  ;; Show conjunction of low-level constituents (noun)
  (def-test t46
      "My brother and friend, Steve."
    ((cat np)
     (complex apposition)
     (restrictive no) 
     (distinct 
      ~(((cat common) 
	 (head ((cat noun)
		(complex conjunction)
		(distinct ~( ((lex "brother"))
			     ((lex "friend")) ))))
	 (possessor ((cat personal-pronoun)
		     (person first))))
	((cat proper)
	 (head === "Steve"))))))



  ;; Test ordinal
  (def-test t47
      "My first two years were happy and uneventful."
    ((cat clause)
     (proc ((type ascriptive)
	    (mode attributive)))
     (tense past)
     (partic ((carrier ((head === "year")
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
     (proc ((type possessive) (mode equative) (voice passive)))
     (tense past)
     (partic ((possessor ((cat proper) (head === "Old McDonald")))
	      (possessed ((cat common) (head === farm) (definite no)))))))



  ;; Test reason role
  (def-test t51a
      "To be innocent."
    ((cat clause)
     (mood infinitive)
     (keep-for no)
     (process ((type ascriptive)
	       (relation-type attributive)))
     (partic ((carrier ((cat personal-pronoun)
			(animate yes)
			(person second)
			(number singular)))
	      (attribute === innocent)))))

  (def-test t51
      "To be innocent, you must do it for him to eat."
    ((cat clause)
     (deontic-modality duty)
     (process ((lex "do")
	       (type material)))
     (partic ((agent ((cat personal-pronoun)
		      (animate yes)
		      (person second)
		      (number singular)))
	      (affected ((cat personal-pronoun)
			 (gender neuter)
			 (person third)
			 (number singular)))))
     (circum ((purpose ((cat clause)
			(keep-for no)
			(keep-in-order no)
			(process ((type ascriptive)
				  (relation-type attributive)))
			;; note: map semantics to avoid propagating gaps/case
			(partic ((carrier ((semantics {partic agent semantics})))
				 (attribute === innocent)))))
	      (behalf ((cat clause)
		       (process ((type material)
				 (lex "eat")))
		       (partic ((agent ((cat personal-pronoun)
					(animate yes)
					(person third)
					(gender masculine)
					(number singular)))))))))))

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


  (def-test t53a
      "Does John give a blue book to Mary?"
    ((cat clause)
     (mood yes-no)
     (dative-move no)
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

  (def-test t53
      "Will John give a blue book to Mary?"
    ((cat clause)
     (mood yes-no)
     (tense future)
     (dative-move no)
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
      "John did give a blue book to Mary."
    ((cat clause)
     (insistence yes)
     (dative-move no)
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
     (scope ((role carrier)))
     (proc ((type ascriptive)))
     (partic ((carrier ((lex "car")
			(cat common)
			(distance near)))
	      (attribute === "expensive")))))


  (def-test t56
      "What is a classic?"
    ((cat clause)	
     (mood wh)
     (proc ((type ascriptive)))
     (scope ((role carrier) (animate no)))
     (partic ((attribute ((cat common) (definite no) (lex "classic")))))))


  ;; JR Note: The scope can't simply be "attribute", since then it would be
  ;; impossible to choose the question pronoun (e.g. in t55 it would be "How"
  ;; or "How expensive", in t56 it would be "What" or "What kind of")
  ;; Also note the ambiguity of t56.
  (def-test t57
      "Who is your father?"
    ((cat clause)
     (mood wh)
     (scope ((role identified) (animate yes)))
     (proc ((type ascriptive) (mode equative)))
     (partic ((identifier ((cat common)
			   (possessor ((cat personal-pronoun)
				       (person second)))
			   (head === "father")))))))


  (def-test t58
      "Which is your father?"
    ((cat clause)
     (mood wh)
     (scope ((role identifier) (animate yes)))
     (proc ((type ascriptive) (mode equative)))
     (partic ((identified ((cat common)
			   (possessor ((cat personal-pronoun)
				       (person second)))
			   (head === "father")))))))


  (def-test t59
      "Where is your mother?"
    ((cat clause)
     (mood wh)
     (scope ((role location) (animate no)))
     (proc ((type locative))) 
     (partic ((located ((cat common)
			(possessor ((cat personal-pronoun)
				    (person second)))
			(head === "mother")))))))


  (def-test t60
      "Who is in your house?"
    ((cat clause)
     (mood wh)
     (scope ((role located) (animate yes)))
     (proc ((type locative)))
     (partic ((location ((cat pp)
			 (prep === "in")
			 (np ((cat common)
			      (possessor ((cat personal-pronoun)
					  (person second)))
			      (head === "house")))))))))

  (def-test t61a
      "The seal covers the opening."
    ((cat clause)
     (scope ((role located)))
     (proc ((type locative) (mode equative) (lex "cover")))
     (partic ((location ((cat common) (head === "opening")))
	      (located ((cat common) (head === "seal")))))))

  (def-test t61
      "What covers the opening?"
    ((cat clause)
     (mood wh)
     (scope ((role located) (animate no)))
     (proc ((type locative) (mode equative) (lex "cover")))
     (partic ((location ((cat common) (head === "opening")))))))

  (def-test t62a
      "Does the seal cover the opening?"
    ((cat clause)
     (mood yes-no)
     (scope ((role location) (animate no)))
     (proc ((type locative) (mode equative) (lex "cover")))
     (partic ((location ((cat common) (head === "opening")))
	      (located ((cat common) (head === "seal")))))))


  (def-test t62
      "What does the seal cover?"
    ((cat clause)
     (mood wh)
     (scope ((role location) (animate no)))
     (proc ((type locative) (mode equative) (lex "cover")))
     (partic ((located ((cat common) (head === "seal")))))))

  (def-test t63
      "When is the game?"
    ((cat clause)
     (mood wh)
     (scope ((role time)))
     (proc ((type temporal)))
     (partic ((located ((cat common) (head === "game")))))))

  (def-test t64
      "What happens then?"
    ((cat clause)
     (mood wh)
     (scope ((role located)))
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
      "What is your sister?"
    ((cat clause)
     (mood wh)
     (scope ((role attribute)))
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
     (scope ((role location) (animate yes)))
     (partic ((located ((cat common)
			(possessor ((cat personal-pronoun)
				    (person second)))
			(head === "sister")))))))


  (def-test t67
      "Who has a PhD.?"
    ((cat clause)
     (mood wh)
     (scope ((role possessor) (animate yes)))
     (proc ((type possessive)))
     (partic ((possessed ((cat common) (definite no) (head === "PhD.")))))))


  (def-test t68
      "What does she have?"
    ((cat clause)
     (mood wh)
     (relation-mode attributive)
     (proc ((type possessive)))
     (scope ((role possessed) (animate no)))
     (partic ((possessor ((cat personal-pronoun)
			  (gender feminine)
			  (number singular)
			  (person third)))))))

  (def-test t69
      "Who owns this book?"
    ((cat clause)
     (mood wh)
     (proc ((type possessive) (mode equative)))
     (scope ((role possessor) (animate yes)))
     (partic ((possessed ((cat common) 
			  (distance near) 
			  (head === "book")))))))

  (def-test t70
      "What does she own?"
    ((cat clause)
     (mood wh)
     (proc ((type possessive) (mode equative)))
     (scope ((role possessed) (animate no)))
     (partic ((possessor ((cat personal-pronoun)
			  (gender feminine)
			  (person third)
			  (number singular)))))))


  (def-test t71
      "Who gives a blue book to Mary?"
    ((cat clause)
     (mood wh)
     (dative-move no)
     (scope ((role agent)
	     (animate yes)))
     (proc ((type composite) 
	    (relation-type possessive)
	    (dative-prep "to") 
	    (lex "give")))
     (partic ((possessed ((lex "book") 
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
     (dative-move no)
     (scope ((role possessed) (restrictive no) (animate no)))
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
     (dative-move no)
     (scope ((role possessor) (animate yes)))
     (proc ((type composite)
	    (relation-type possessive)
	    (dative-prep "to")
	    (lex "give")))
     (partic ((agent ((lex "John") (cat proper)))
	      (possessed ((lex "book")
			  (cat common)
			  (definite no)
			  (describer === "blue")))))))

  ;; Note problem with scope being DOUBLE role, e.g. here Af/Pr
  ;; since af and pr unify into affected-carrier.
  ;; Always choose the most specific in your input.


  (def-test t74
      "What does Deborah prefer?"
    ((cat clause)
     (mood wh)
     (scope ((role phenomenon) (animate no)))
     (proc ((type mental) (lex "prefer")))
     (partic ((processor ((cat proper) (lex "Deborah")))))))


  (def-test t75
      "Who worships Baal?"
    ((cat clause)
     (mood wh)
     (scope ((role processor) (animate yes)))
     (proc ((type mental) (lex "worship")))
     (partic ((phenomenon ((cat proper) (lex "Baal")))))))

  ;; JR Question: How to make generic reference like "Who worships FIRE" or
  ;; "They want MONEY" ?

  ;; Test reason role with a clause
  (def-test t76
      "Why does he do it?"
    ((cat clause)
     (mood wh)
     (scope ((role reason)))
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
     (scope ((role from-loc)))
     (proc ((type composite)
	    (relation-type locative)
	    (agentive no)
	    (lex "move")))
     (partic ((affected ((cat common) (lex "sig-display marker")))
	      (located {^ affected})
	      (location ((cat pp) (prep === to) (np ((lex "right")))))))))




  ;; ======================================================================
  ;; test RELATIVE CLAUSES
  ;; ======================================================================

  (def-test t78
      "The box which is expensive."
    ((cat common)
     (head === box)
     (qualifier ((cat clause)
		 (restrictive no)
		 (scope ((role carrier)))
		 (proc ((type ascriptive)))
		 (partic ((attribute === "expensive")))))))


  (def-test t79
      "The box which is a classic."
    ((cat common)
     (head === box)
     (qualifier ((cat clause)	
		 (restrictive no)
		 (proc ((type ascriptive)))
		 (scope ((role carrier) (animate no)))
		 (partic ((attribute ((cat common) 
				      (definite no) 
				      (lex "classic")))))))))


  (def-test t80
      "The man who is your father."
    ((cat common)
     (head === man)
     (animate yes)
     (qualifier ((cat clause)
		 (restrictive no)
		 (scope ((role identified) (animate yes)))
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
		 (scope ((role identifier)))
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
		 (restrictive no)
		 (scope ((role location)))
		 (proc ((type locative) (mode attributive)))
		 (partic ((located ((cat common)
				    (possessor ((cat personal-pronoun)
						(person second)))
				    (head === "mother")))))))))


  (def-test t83
      "The man who is in your house."
    ((cat common)
     (head === man)
     (animate yes)
     (qualifier ((cat clause)
		 (restrictive no)
		 (scope ((role located) (animate yes)))
		 (proc ((type locative)))
		 (partic ((location ((cat pp)
				     (prep === "in")
				     (np ((cat common)
					  (possessor ((cat personal-pronoun)
						      (person second)))
					  (head === "house")))))))))))


  (def-test t84
      "The plate which covers the opening."
    ((cat common)
     (head === plate)
     (qualifier ((cat clause)
		 (restrictive no)
		 (scope ((role located)))
		 (proc ((type locative) (mode equative) (lex "cover")))
		 (partic ((location ((cat common) 
				     (head === "opening")))))))))


  (def-test t85
      "The hole which the seal covers."
    ((cat common)
     (head === hole)
     (qualifier ((cat clause)
		 (restrictive no)
		 (scope ((role location) (animate no)))
		 (proc ((type locative) (mode equative) (lex "cover")))
		 (partic ((located ((cat common) 
				    (head === "seal")))))))))

  (def-test t86
      "The time when the game starts."
    ((cat common)
     (head === time)
     (qualifier ((cat clause)
		 (scope ((role time)))
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
		 (scope ((role located)))
		 (proc ((type temporal) (lex "happen")))
		 (partic ((time ((cat adv) (lex "then")))))))))


  (def-test t89
      "The way which your sister is."
    ((cat common)
     (head === way)
     (qualifier ((cat clause)
		 (restrictive no)
		 (scope ((role attribute)))
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
		 (restrictive yes)
		 (scope ((role location) (animate yes)))
		 (partic ((located ((cat common)
				    (possessor ((cat personal-pronoun)
						(person second)))
				    (head === "sister")))))))))


  (def-test t91
      "The person who has a PhD."
    ((cat common)
     (head === person)
     (animate yes)
     (qualifier ((cat clause)
		 (restrictive no)
		 (scope ((role possessor) (animate yes)))
		 (proc ((type possessive)))
		 (partic ((possessed ((cat common) 
				      (definite no) 
				      (head === "PhD.")))))))))


  (def-test t92
      "The box which she has."
    ((cat common)
     (head === box)
     (qualifier ((cat clause)
		 (restrictive no)
		 (proc ((type possessive)))
		 (scope ((role possessed) (animate no)))
		 (partic ((possessor ((cat personal-pronoun)
				      (gender feminine)
				      (number singular)
				      (person third)))))))))

  (def-test t93
      "The person who owns this book."
    ((cat common)
     (head === person)
     (animate yes)
     (qualifier ((cat clause)
		 (restrictive no)
		 (proc ((type possessive) (mode equative)))
		 (scope ((role possessor) (animate yes)))
		 (partic ((possessed ((cat common) 
				      (distance near) 
				      (head === "book")))))))))

  (def-test t94
      "The box that she owns."
    ((cat common)
     (head === box)
     (qualifier ((cat clause)
		 (proc ((type possessive) (mode equative)))
		 (scope ((role possessed)))
		 (partic ((possessor ((cat personal-pronoun)
				      (gender feminine)
				      (person third)
				      (number singular)))))))))



  (def-test t95
      "The person who gives a blue book to Mary."
    ((cat common)
     (head === person)
     (animate yes)
     (qualifier ((cat clause)
		 (scope ((role agent) (animate yes)))
		 (dative-move no)
		 (restrictive no)
		 (proc ((type composite) 
			(relation-type possessive)
			(dative-prep "to") 
			(lex "give")))
		 (partic ((possessed ((lex "book") 
				      (cat common)
				      (definite no)
				      (describer === "blue")))
			  (affected ((lex "Mary") (cat proper)))
			  (possessor {^ affected})))))))


  (def-test t96
      "The box which John will give to Mary."
    ((cat common)
     (head === box)
     (qualifier ((cat clause)
		 (tense future)
		 (restrictive no)
		 (dative-move no)
		 (scope ((role possessed) (animate no)))
		 (proc ((type composite)
			(relation-type possessive)
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
		 (restrictive no)
		 (dative-move no)
		 (scope ((role possessor) (animate yes)))
		 (proc ((type composite)
			(relation-type possessive)
			(dative-prep "to")
			(lex "give")))
		 (partic ((agent ((lex "John") (cat proper)))
			  (possessed ((lex "book")
				      (cat common)
				      (definite no)
				      (describer === "blue")))))))))

  (def-test t97b
      "The person whom John will give a blue book."
    ((cat common)
     (head === person)
     (animate yes)
     (qualifier ((cat clause)
		 (tense future)
		 (restrictive no)
		 (dative-move yes)
		 (scope ((role possessor) (animate yes)))
		 (proc ((type composite)
			(relation-type possessive)
			(dative-prep "to")
			(lex "give")))
		 (partic ((agent ((lex "John") (cat proper)))
			  (possessed ((lex "book")
				      (cat common)
				      (definite no)
				      (describer === "blue")))))))))



  (def-test t98
      "The box which Deborah prefers."
    ((cat common)
     (head === box)
     (qualifier ((cat clause)
		 (restrictive no)
		 (scope ((role phenomenon)))
		 (proc ((type mental) (lex "prefer")))
		 (partic ((processor ((cat proper) 
				      (lex "Deborah")))))))))



  (def-test t99
      "The person who worships Baal."
    ((cat common)
     (head === person)
     (animate yes)
     (qualifier ((cat clause)
		 (restrictive no)
		 (scope ((role processor) (animate yes)))
		 (proc ((type mental) (lex "worship")))
		 (partic ((phenomenon ((cat proper) 
				       (lex "Baal")))))))))


  ;; @todo
  ;; relative roles:
  ;; role in-loc:         The city in which I grew up.
  ;; role on-loc:         The table on which I put the cheese.
  ;; role processor:      The person who thinks it is efficient.
  ;; role purpose:        The reason why I do this.
  ;; 
  ;; relative roles on composite processes

  (def-test a1
      "Yael eats pizza with Noa."
    ((cat clause)
     (proc ((type material) (lex "eat")))
     (partic ((agent ((cat proper) (lex "Yael") (animate yes)))
	      (affected ((cat common) (lex "pizza") (countable no)))))
     (circum ((accompaniment ((cat proper) (lex "Noa") (animate yes)))))))

  (def-test r1-accompaniment-e
      "The person with whom Yael eats pizza."
    ((cat common)
     (head === person)
     (animate yes)
     (qualifier ((cat clause)
		 (restrictive no)
		 (scope ((role accompaniment) (animate yes)))
		 (proc ((type material) (lex "eat")))
		 (partic ((agent ((cat proper) (lex "Yael") (animate yes)))
			  (affected ((cat common) (lex "pizza") (countable no)))))))))

  (def-test r1-accompaniment
      "The person with whom Yael eats pizza."
    ((cat common)
     (head === person)
     (animate yes)
     (qualifier ((cat clause)
		 (restrictive no)
		 (scope ((role accompaniment)))
		 (proc ((type material) (lex "eat")))
		 (partic ((agent ((cat proper) (lex "Yael") (animate yes)))
			  (affected ((cat common) (lex "pizza") (countable no)))))
		 (circum ((accompaniment ((cat proper) (lex "Noa") (animate yes)))))))))

  (def-test r1-at-time
      "The period when Yael eats pizza with Noa."
    ((cat common)
     (head === period)
     (qualifier ((cat clause)
		 (restrictive no)
		 (scope ((role at-time)))
		 (proc ((type material) (lex "eat")))
		 (partic ((agent ((cat proper) (lex "Yael") (animate yes)))
			  (affected ((cat common) (lex "pizza") (countable no)))))
		 (circum ((accompaniment ((cat proper) (lex "Noa") (animate yes)))))))))

  (def-test r1-at-loc
      "The place where Yael eats pizza with Noa."
    ((cat common)
     (head === place)
     (qualifier ((cat clause)
		 (restrictive no)
		 (scope ((role at-loc)))
		 (proc ((type material) (lex "eat")))
		 (partic ((agent ((cat proper) (lex "Yael") (animate yes)))
			  (affected ((cat common) (lex "pizza") (countable no)))))
		 (circum ((accompaniment ((cat proper) (lex "Noa") (animate yes)))))))))

  (def-test r1-from-loc
      "The place from which Yael eats pizza with Noa."
    ((cat common)
     (head === place)
     (qualifier ((cat clause)
		 (restrictive no)
		 (scope ((role from-loc)))
		 (proc ((type material) (lex "eat")))
		 (partic ((agent ((cat proper) (lex "Yael") (animate yes)))
			  (affected ((cat common) (lex "pizza") (countable no)))))
		 (circum ((accompaniment ((cat proper) (lex "Noa") (animate yes)))))))))

  (def-test r1-in-loc
      "The place in which Yael eats pizza with Noa."
    ((cat common)
     (head === place)
     (qualifier ((cat clause)
		 (restrictive no)
		 (scope ((role in-loc)))
		 (proc ((type material) (lex "eat")))
		 (partic ((agent ((cat proper) (lex "Yael") (animate yes)))
			  (affected ((cat common) (lex "pizza") (countable no)))))
		 (circum ((accompaniment ((cat proper) (lex "Noa") (animate yes)))))))))

  (def-test r1-instrument
      "The fork with which Yael eats pizza with Noa."
    ((cat common)
     (head === fork)
     (qualifier ((cat clause)
		 (restrictive no)
		 (scope ((role instrument)))
		 (proc ((type material) (lex "eat")))
		 (partic ((agent ((cat proper) (lex "Yael") (animate yes)))
			  (affected ((cat common) (lex "pizza") (countable no)))))
		 (circum ((accompaniment ((cat proper) (lex "Noa") (animate yes)))))))))

  (def-test r1-manner
      "The way how Yael eats pizza with Noa."
    ((cat common)
     (head === way)
     (qualifier ((cat clause)
		 (restrictive no)
		 (scope ((role manner)))
		 (proc ((type material) (lex "eat")))
		 (partic ((agent ((cat proper) (lex "Yael") (animate yes)))
			  (affected ((cat common) (lex "pizza") (countable no)))))
		 (circum ((accompaniment ((cat proper) (lex "Noa") (animate yes)))))))))

  (def-test r1-on-loc
      "The mountain on which Yael eats pizza with Noa."
    ((cat common)
     (head === mountain)
     (qualifier ((cat clause)
		 (restrictive no)
		 (scope ((role on-loc)))
		 (proc ((type material) (lex "eat")))
		 (partic ((agent ((cat proper) (lex "Yael") (animate yes)))
			  (affected ((cat common) (lex "pizza") (countable no)))))
		 (circum ((accompaniment ((cat proper) (lex "Noa") (animate yes)))))))))


  (def-test r1-purpose
      "The reason for which Yael eats pizza with Noa."
    ((cat common)
     (head === reason)
     (animate yes)
     (qualifier ((cat clause)
		 (restrictive no)
		 (scope ((role purpose)))
		 (proc ((type material) (lex "eat")))
		 (partic ((agent ((cat proper) (lex "Yael") (animate yes)))
			  (affected ((cat common) (lex "pizza") (countable no)))))
		 (circum ((accompaniment ((cat proper) (lex "Noa") (animate yes)))))))))

  
  (def-test r1-behalf
      "The person for whom Yael eats pizza with Noa."
    ((cat common)
     (head === person)
     (animate yes)
     (qualifier ((cat clause)
		 (restrictive no)
		 (scope ((role behalf) (animate yes) (restrictive no)))
		 (proc ((type material) (lex "eat")))
		 (partic ((agent ((cat proper) (lex "Yael") (animate yes)))
			  (affected ((cat common) (lex "pizza") (countable no)))))
		 (circum ((accompaniment ((cat proper) (lex "Noa") (animate yes)))))))))


  (def-test r1-affected
      "The food which Yael eats with Noa."
    ((cat common)
     (head === food)
     (qualifier ((cat clause)
		 (restrictive no)
		 (scope ((role affected)))
		 (proc ((type material) (lex "eat")))
		 (partic ((agent ((cat proper) (lex "Yael") (animate yes)))
			  (affected ((cat common) (lex "pizza") (countable no)))))
		 (circum ((accompaniment ((cat proper) (lex "Noa") (animate yes)))))))))

  (def-test q1-yes-no
      "Does Yael eat pizza with Noa?"
    ((cat clause)
     (proc ((type material) (lex "eat")))
     (mood yes-no)
     (scope ((role accompaniment) (animate yes)))
     (partic ((agent ((cat proper) (lex "Yael") (animate yes)))
	      (affected ((cat common) (lex "pizza") (countable no)))))
     (circum ((accompaniment ((cat proper) (lex "Noa") (animate yes)))))))

  (def-test q1-accompaniment-e
      "With whom does Yael eat pizza?"
    ((cat clause)
     (proc ((type material) (lex "eat")))
     (mood wh)
     (scope ((role accompaniment) (animate yes)))
     (partic ((agent ((cat proper) (lex "Yael") (animate yes)))
	      (affected ((cat common) (lex "pizza") (countable no)))))))

  (def-test q1-accompaniment
      "With whom does Yael eat pizza?"
    ((cat clause)
     (proc ((type material) (lex "eat")))
     (mood wh)
     (scope ((role accompaniment)))
     (partic ((agent ((cat proper) (lex "Yael") (animate yes)))
	      (affected ((cat common) (lex "pizza") (countable no)))))
     (circum ((accompaniment ((cat proper) (lex "Noa") (animate yes)))))))

  (def-test q1-agent
      "Who eats pizza with Noa?"
    ((cat clause)
     (mood wh)
     (scope ((role agent)))
     (proc ((type material) (lex "eat")))
     (partic ((agent ((cat proper) (lex "Yael") (animate yes)))
	      (affected ((cat common) (lex "pizza") (countable no)))))
     (circum ((accompaniment ((cat proper) (lex "Noa") (animate yes)))))))

  (def-test q1-affected
      "What does Yael eat with Noa?"
    ((cat clause)
     (mood wh)
     (scope ((role affected)))
     (proc ((type material) (lex "eat")))
     (partic ((agent ((cat proper) (lex "Yael") (animate yes)))
	      (affected ((cat common) (lex "pizza") (countable no)))))
     (circum ((accompaniment ((cat proper) (lex "Noa") (animate yes)))))))

  (def-test q1-at-loc
      "Where does Yael eat pizza with Noa?"
    ((cat clause)
     (mood wh)
     (scope ((role at-loc)))
     (proc ((type material) (lex "eat")))
     (partic ((agent ((cat proper) (lex "Yael") (animate yes)))
	      (affected ((cat common) (lex "pizza") (countable no)))))
     (circum ((accompaniment ((cat proper) (lex "Noa") (animate yes)))))))

  (def-test q1-at-time
      "When does Yael eat pizza with Noa?"
    ((cat clause)
     (mood wh)
     (scope ((role at-time)))
     (proc ((type material) (lex "eat")))
     (partic ((agent ((cat proper) (lex "Yael") (animate yes)))
	      (affected ((cat common) (lex "pizza") (countable no)))))
     (circum ((accompaniment ((cat proper) (lex "Noa") (animate yes)))))))

  (def-test q1-behalf
      "For whom does Yael eat pizza with Noa?"
    ((cat clause)
     (mood wh)
     (scope ((role behalf) (animate yes)))
     (proc ((type material) (lex "eat")))
     (partic ((agent ((cat proper) (lex "Yael") (animate yes)))
	      (affected ((cat common) (lex "pizza") (countable no)))))
     (circum ((accompaniment ((cat proper) (lex "Noa") (animate yes)))))))

  (def-test q1-from-loc
      "From where does Yael eat pizza with Noa?"
    ((cat clause)
     (mood wh)
     (scope ((role from-loc)))
     (proc ((type material) (lex "eat")))
     (partic ((agent ((cat proper) (lex "Yael") (animate yes)))
	      (affected ((cat common) (lex "pizza") (countable no)))))
     (circum ((accompaniment ((cat proper) (lex "Noa") (animate yes)))))))

  (def-test q1-in-loc
      "In what does Yael eat pizza with Noa?"
    ((cat clause)
     (mood wh)
     (scope ((role in-loc)))
     (proc ((type material) (lex "eat")))
     (partic ((agent ((cat proper) (lex "Yael") (animate yes)))
	      (affected ((cat common) (lex "pizza") (countable no)))))
     (circum ((accompaniment ((cat proper) (lex "Noa") (animate yes)))))))

  (def-test q1-instrument
      "With what does Yael eat pizza with Noa?"
    ((cat clause)
     (mood wh)
     (scope ((role instrument)))
     (proc ((type material) (lex "eat")))
     (partic ((agent ((cat proper) (lex "Yael") (animate yes)))
	      (affected ((cat common) (lex "pizza") (countable no)))))
     (circum ((accompaniment ((cat proper) (lex "Noa") (animate yes)))))))

  (def-test q1-on-loc
      "On what does Yael eat pizza with Noa?"
    ((cat clause)
     (mood wh)
     (scope ((role on-loc)))
     (proc ((type material) (lex "eat")))
     (partic ((agent ((cat proper) (lex "Yael") (animate yes)))
	      (affected ((cat common) (lex "pizza") (countable no)))))
     (circum ((accompaniment ((cat proper) (lex "Noa") (animate yes)))))))

  (def-test q1-purpose
      "For what does Yael eat pizza with Noa?"
    ((cat clause)
     (mood wh)
     (scope ((role purpose)))
     (proc ((type material) (lex "eat")))
     (partic ((agent ((cat proper) (lex "Yael") (animate yes)))
	      (affected ((cat common) (lex "pizza") (countable no)))))
     (circum ((accompaniment ((cat proper) (lex "Noa") (animate yes)))))))

  (def-test q1-reason
      "Why does Yael eat pizza with Noa?"
    ((cat clause)
     (mood wh)
     (scope ((role reason)))
     (proc ((type material) (lex "eat")))
     (partic ((agent ((cat proper) (lex "Yael") (animate yes)))
	      (affected ((cat common) (lex "pizza") (countable no)))))
     (circum ((accompaniment ((cat proper) (lex "Noa") (animate yes)))))))

  (def-test q1-to-loc
      "Where does Yael eat pizza with Noa?"
    ((cat clause)
     (mood wh)
     (scope ((role to-loc)))
     (proc ((type material) (lex "eat")))
     (partic ((agent ((cat proper) (lex "Yael") (animate yes)))
	      (affected ((cat common) (lex "pizza") (countable no)))))
     (circum ((accompaniment ((cat proper) (lex "Noa") (animate yes)))))))



  (def-test a2
      "Noa eats pasta at the restaurant."
    ((cat clause)
     (proc ((type material) (lex "eat")))
     (partic ((agent ((cat proper) (lex "Noa")))
	      (affected ((cat common) (lex "pasta") (countable no)))))
     (circum ((at-loc ((cat common) (lex "restaurant")))))))

  (def-test r2-at-loc-e
      "The restaurant where Noa eats pasta."
    ((cat common)
     (head === restaurant)
     (qualifier ((cat clause)
		 (scope ((role at-loc)))
		 (proc ((type material) (lex "eat")))
		 (partic ((agent ((cat proper) (lex "Noa")))
			  (affected ((cat common) (lex "pasta") (countable no)))))))))

  ;; with full scope
  (def-test r2-at-loc
      "The restaurant where Noa eats pasta."
    ((cat common)
     (head === restaurant)
     (qualifier ((cat clause)
		 (scope ((role at-loc)))
		 (proc ((type material) (lex "eat")))
		 (partic ((agent ((cat proper) (lex "Noa")))
			  (affected ((cat common) (lex "pasta") (countable no)))))
		 (circum ((at-loc ((cat common) (lex "restaurant")))))))))

  (def-test r2-agent
      "The person who eats pasta at the restaurant."
    ((cat common)
     (head === person)
     (qualifier ((cat clause)
		 (scope ((role agent) (animate yes)))
		 (restrictive no)
		 (proc ((type material) (lex "eat")))
		 (partic ((agent ((cat proper) (lex "Noa")))
			  (affected ((cat common) (lex "pasta") (countable no)))))
		 (circum ((at-loc ((cat common) (lex "restaurant")))))))))

  (def-test r2-affected
      "The food which Noa eats at the restaurant."
    ((cat common)
     (head === food)
     (qualifier ((cat clause)
		 (restrictive no)
		 (scope ((role affected)))
		 (proc ((type material) (lex "eat")))
		 (partic ((agent ((cat proper) (lex "Noa")))
			  (affected ((cat common) (lex "pasta") (countable no)))))
		 (circum ((at-loc ((cat common) (lex "restaurant")))))))))

  (def-test q2-at-loc-e
      "Where does Noa eat pasta?"
    ((cat clause)
     (proc ((type material) (lex "eat")))
     (mood wh)
     (scope ((role at-loc)))
     (partic ((agent ((cat proper) (lex "Noa")))
	      (affected ((cat common) (lex "pasta") (countable no)))))))

  (def-test q2-yes-no
      "Does Noa eat pasta at the restaurant?"
    ((cat clause)
     (proc ((type material) (lex "eat")))
     (mood yes-no)
     (scope ((role at-loc)))
     (partic ((agent ((cat proper) (lex "Noa")))
	      (affected ((cat common) (lex "pasta") (countable no)))))
     (circum ((at-loc ((cat common) (lex "restaurant")))))))


  (def-test q2-at-loc
      "Where does Noa eat pasta?"
    ((cat clause)
     (proc ((type material) (lex "eat")))
     (mood wh)
     (scope ((role at-loc)))
     (partic ((agent ((cat proper) (lex "Noa")))
	      (affected ((cat common) (lex "pasta") (countable no)))))
     (circum ((at-loc ((cat common) (lex "restaurant")))))))

  (def-test q2-agent
      "Who eats pasta at the restaurant?"
    ((cat clause)
     (proc ((type material) (lex "eat")))
     (mood wh)
     (scope ((role agent) (animate yes)))
     (partic ((agent ((cat proper) (lex "Noa")))
	      (affected ((cat common) (lex "pasta") (countable no)))))
     (circum ((at-loc ((cat common) (lex "restaurant")))))))

  (def-test q2-affected
      "What does Noa eat at the restaurant?"
    ((cat clause)
     (proc ((type material) (lex "eat")))
     (mood wh)
     (scope ((role affected)))
     (partic ((agent ((cat proper) (lex "Noa")))
	      (affected ((cat common) (lex "pasta") (countable no)))))
     (circum ((at-loc ((cat common) (lex "restaurant")))))))



  (def-test a3
      "Noa goes to school at 7:30."
    ((cat clause)
     (proc ((type material) (lex "go")))
     (partic ((agent ((cat proper) (lex "Noa")))))
     (circum ((to-loc ((cat common) (lex "school") (countable no)))
	      (at-time ((cat common) (lex "7:30") (countable no)))))))

  (def-test r3a
      "The time when Noa goes to school."
    ((cat common)
     (lex "time")
     (qualifier ((cat clause)
		 (proc ((type material) (lex "go")))
		 (scope ((role at-time)))
		 (partic ((agent ((cat proper) (lex "Noa")))))
		 (circum ((to-loc ((cat common) (lex "school") (countable no)))))))))

  (def-test r3b
      "The place to which Noa goes at 7:30."
    ((cat common)
     (lex "place")
     (qualifier ((cat clause)
		 (proc ((type material) (lex "go")))
		 (scope ((role to-loc)))
		 (partic ((agent ((cat proper) (lex "Noa")))))
		 (circum ((at-time ((cat common) (lex "7:30") (countable no)))))))))

  (def-test q3-to-loc-e
      "Where does Noa go at 7:30?"
    ((cat clause)
     (mood wh)
     (scope ((role to-loc)))
     (proc ((type material) (lex "go")))
     (partic ((agent ((cat proper) (lex "Noa")))))
     (circum (
	      (at-time ((cat common) (lex "7:30") (countable no)))))))

  (def-test q3-yes-no
      "Does Noa go to school at 7:30?"
    ((cat clause)
     (mood yes-no)
     (proc ((type material) (lex "go")))
     (partic ((agent ((cat proper) (lex "Noa")))))
     (circum ((to-loc ((cat common) (lex "school") (countable no)))
	      (at-time ((cat common) (lex "7:30") (countable no)))))))

  (def-test q3-to-loc
      "Where does Noa go at 7:30?"
    ((cat clause)
     (mood wh)
     (scope ((role to-loc)))
     (proc ((type material) (lex "go")))
     (partic ((agent ((cat proper) (lex "Noa")))))
     (circum ((to-loc ((cat common) (lex "school") (countable no)))
	      (at-time ((cat common) (lex "7:30") (countable no)))))))


  (def-test q3-at-time
      "When does Noa go to school?"
    ((cat clause)
     (mood wh)
     (scope ((role at-time)))
     (proc ((type material) (lex "go")))
     (partic ((agent ((cat proper) (lex "Noa")))))
     (circum ((to-loc ((cat common) (lex "school") (countable no)))
	      (at-time ((cat common) (lex "7:30") (countable no)))))))


  (def-test a4
      "I wrote this grammar for Hannah."
    ((cat clause)
     (tense past)
     (proc ((type material) (lex "write")))
     (partic ((agent ((cat personal-pronoun) (person first)))
	      (created ((cat common) (lex "grammar") (distance near)))))
     (circum ((behalf ((cat proper) (lex "Hannah") (animate yes)))))))

  (def-test q4-yes-no
      "Did I write this grammar for Hannah?"
    ((cat clause)
     (tense past)
     (mood yes-no)
     (proc ((type material) (lex "write")))
     (partic ((agent ((cat personal-pronoun) (person first)))
	      (created ((cat common) (lex "grammar") (distance near)))))
     (circum ((behalf ((cat proper) (lex "Hannah") (animate yes)))))))

  (def-test q4-behalf
      "For whom did I write this grammar?"
    ((cat clause)
     (tense past)
     (mood wh)
     (scope ((role behalf)))
     (proc ((type material) (lex "write")))
     (partic ((agent ((cat personal-pronoun) (person first)))
	      (created ((cat common) (lex "grammar") (distance near)))))
     (circum ((behalf ((cat proper) (lex "Hannah") (animate yes)))))))

  (def-test q4-agent
      "Who wrote this grammar for Hannah?"
    ((cat clause)
     (tense past)
     (mood wh)
     (scope ((role agent) (animate yes)))
     (proc ((type material) (lex "write")))
     (partic ((agent ((cat personal-pronoun) (person first)))
	      (affected ((cat common) (lex "grammar") (distance near)))))
     (circum ((behalf ((cat proper) (lex "Hannah") (animate yes)))))))

  (def-test q4-affected
      "What did I write for Hannah?"
    ((cat clause)
     (tense past)
     (mood wh)
     (scope ((role affected)))
     (proc ((type material) (lex "write")))
     (partic ((agent ((cat personal-pronoun) (person first)))
	      (affected ((cat common) (lex "grammar") (distance near)))))
     (circum ((behalf ((cat proper) (lex "Hannah") (animate yes)))))))



  ;; role in-loc:         The city in which you lived.
  (def-test a5
      "You lived in the city."
    ((cat clause)
     (tense past)
     (proc ((type ascriptive) (lex "live")))
     (partic ((carrier ((cat personal-pronoun) (person second)))))
     (circum ((in-loc ((cat common) (lex "city")))))))

  (def-test q5-yes-no
      "Did you live in the city?"
    ((cat clause)
     (mood yes-no)
     (tense past)
     (proc ((type ascriptive) (lex "live")))
     (partic ((carrier ((cat personal-pronoun) (person second)))))
     (circum ((in-loc ((cat common) (lex "city")))))))

  (def-test q5-carrier
      "Who lived in the city?"
    ((cat clause)
     (mood wh)
     (scope ((role carrier)))
     (tense past)
     (proc ((type ascriptive) (lex "live")))
     (partic ((carrier ((cat personal-pronoun) (person second)))))
     (circum ((in-loc ((cat common) (lex "city")))))))

  (def-test q5-in-loc
      "In what did you live?"
    ((cat clause)
     (mood wh)
     (scope ((role in-loc)))
     (tense past)
     (proc ((type ascriptive) (lex "live")))
     (partic ((carrier ((cat personal-pronoun) (person second)))))
     (circum ((in-loc ((cat common) (lex "city")))))))

  (def-test q5-manner
      "How did you live in the city?"
    ((cat clause)
     (mood wh)
     (scope ((role manner)))
     (tense past)
     (proc ((type ascriptive) (lex "live")))
     (partic ((carrier ((cat personal-pronoun) (person second)))))
     (circum ((in-loc ((cat common) (lex "city")))))))

  (def-test q5-reason
      "Why did you live in the city?"
    ((cat clause)
     (mood wh)
     (scope ((role reason)))
     (tense past)
     (proc ((type ascriptive) (lex "live")))
     (partic ((carrier ((cat personal-pronoun) (person second)))))
     (circum ((in-loc ((cat common) (lex "city")))))))


  (def-test r5-in-loc
      "The city in which you lived."
    ((cat np)
     (lex "city")
     (qualifier ((cat clause)
		 (tense past)
		 (scope ((role in-loc)))
		 (proc ((type ascriptive) (lex "live")))
		 (partic ((carrier ((cat personal-pronoun) (person second)))))
		 (circum ((in-loc ((cat common) (lex "city")))))))))
  
  (def-test r5-carrier
      "The person who lived in the city."
    ((cat np)
     (lex "person")
     (qualifier ((cat clause)
		 (tense past)
		 (scope ((role carrier) (animate yes) (restrictive no)))
		 (proc ((type ascriptive) (lex "live")))
		 (partic ((carrier ((cat personal-pronoun) (person second)))))
		 (circum ((in-loc ((cat common) (lex "city")))))))))

  (def-test r5-reason
      "The reason why you lived in the city."
    ;; @@ Should it be "for which"?
    ((cat np)
     (lex "reason")
     (qualifier ((cat clause)
		 (tense past)
		 (scope ((role reason)))
		 (proc ((type ascriptive) (lex "live")))
		 (partic ((carrier ((cat personal-pronoun) (person second)))))
		 (circum ((in-loc ((cat common) (lex "city")))))))))

  (store-verbs '( ("hit" "hits" "hit" "hitting" "hitten")) )

  (def-test a6
      "I hit the nail with a hammer."
    ((cat clause)
     (proc ((type material) (lex "hit")))
     (partic ((agent ((cat personal-pronoun) (person first)))
	      (affected ((cat common) (lex "nail")))))
     (circum ((instrument ((cat common) (lex "hammer") (definite no)))))))

  
  (def-test q6-yes-no
      "Did I hit the nail with a hammer?"
    ((cat clause)
     (tense past)
     (mood yes-no)
     (proc ((type material) (lex "hit")))
     (partic ((agent ((cat personal-pronoun) (person first)))
	      (affected ((cat common) (lex "nail")))))
     (circum ((instrument ((cat common) (lex "hammer") (definite no)))))))

  (def-test q6-agent
      "Who hit the nail with a hammer?"
    ((cat clause)
     (tense past)
     (mood wh)
     (scope ((role agent) (animate yes)))
     (proc ((type material) (lex "hit")))
     (partic ((agent ((cat personal-pronoun) (person first)))
	      (affected ((cat common) (lex "nail")))))
     (circum ((instrument ((cat common) (lex "hammer") (definite no)))))))

  (def-test q6-agent
      "Who hit the nail with a hammer?"
    ((cat clause)
     (tense past)
     (mood wh)
     (scope ((role agent) (animate yes)))
     (proc ((type material) (lex "hit")))
     (partic ((agent ((cat personal-pronoun) (person first)))
	      (affected ((cat common) (lex "nail")))))
     (circum ((instrument ((cat common) (lex "hammer") (definite no)))))))

  (def-test q6-affected
      "What did I hit with a hammer?"
    ((cat clause)
     (tense past)
     (mood wh)
     (scope ((role affected)))
     (proc ((type material) (lex "hit")))
     (partic ((agent ((cat personal-pronoun) (person first)))
	      (affected ((cat common) (lex "nail")))))
     (circum ((instrument ((cat common) (lex "hammer") (definite no)))))))

  (def-test q6-instrument
      "With what did I hit the nail?"
    ((cat clause)
     (tense past)
     (mood wh)
     (scope ((role instrument)))
     (proc ((type material) (lex "hit")))
     (partic ((agent ((cat personal-pronoun) (person first)))
	      (affected ((cat common) (lex "nail")))))
     (circum ((instrument ((cat common) (lex "hammer") (definite no)))))))


  (def-test a7
      "The lady drinks tea slowly."
    ((cat clause)
     (proc ((type material) (lex "drink")))
     (partic ((agent ((cat common) (lex "lady")))
	      (affected ((cat common) (lex "tea") (countable no)))))
     (circum ((manner ((cat adv) (lex "slowly")))))))

  (def-test q7-yes-no
      "Does the lady drink tea slowly?"
    ((cat clause)
     (mood yes-no)
     (proc ((type material) (lex "drink")))
     (partic ((agent ((cat common) (lex "lady")))
	      (affected ((cat common) (lex "tea") (countable no)))))
     (circum ((manner ((cat adv) (lex "slowly")))))))

  (def-test q7-agent
      "Who drinks tea slowly?"
    ((cat clause)
     (mood wh)
     (scope ((role agent) (animate yes)))
     (proc ((type material) (lex "drink")))
     (partic ((agent ((cat common) (lex "lady")))
	      (affected ((cat common) (lex "tea") (countable no)))))
     (circum ((manner ((cat adv) (lex "slowly")))))))

  (def-test q7-affected
      "What does the lady drink slowly?"
    ((cat clause)
     (mood wh)
     (scope ((role affected)))
     (proc ((type material) (lex "drink")))
     (partic ((agent ((cat common) (lex "lady")))
	      (affected ((cat common) (lex "tea") (countable no)))))
     (circum ((manner ((cat adv) (lex "slowly")))))))

  (def-test q7-manner
      "How does the lady drink tea?"
    ((cat clause)
     (mood wh)
     (scope ((role manner)))
     (proc ((type material) (lex "drink")))
     (partic ((agent ((cat common) (lex "lady")))
	      (affected ((cat common) (lex "tea") (countable no)))))
     (circum ((manner ((cat adv) (lex "slowly")))))))

  (def-test r7-agent
      "The lady who drinks tea slowly."
    ((cat common)
     (lex "lady")
     (qualifier ((cat clause)
		 (scope ((role agent) (animate yes) (restrictive no)))
		 (proc ((type material) (lex "drink")))
		 (partic ((agent ((cat common) (lex "lady")))
			  (affected ((cat common) (lex "tea") (countable no)))))
		 (circum ((manner ((cat adv) (lex "slowly")))))))))

  (def-test r7-affected
      "The drink that the lady drinks slowly."
    ((cat common)
     (lex "drink")
     (qualifier ((cat clause)
		 (scope ((role affected) (restrictive yes)))
		 (proc ((type material) (lex "drink")))
		 (partic ((agent ((cat common) (lex "lady")))
			  (affected ((cat common) (lex "tea") (countable no)))))
		 (circum ((manner ((cat adv) (lex "slowly")))))))))


  ;; ======================================================================
  ;; test of ALL clause patterns of the new transitvity system:
  ;; ======================================================================
  ;; simple process
  ;; event
  ;; material
  ;; agentive non-effective: Ag
  (store-verbs '( ("run" "runs" "ran" "running" "run")) )
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
	    ;; (effective yes)           is default
	    ;; (effect-type dispositive) is default
	    (lex "drink")))
     (partic ((agent ((cat proper) (lex "Bo")))
	      (affected ((cat common) 
			 (number plural)
			 (definite no)
			 (classifier === "protein") 
			 (head === "shake")))))))


  ;; agentive creative: Ag + Cr
  (def-test t102
      "Bo cooks diner."
    ((cat clause)
     (proc ((type material) (effect-type creative) (lex "cook")))
     (partic ((agent ((cat proper) (lex "Bo")))
	      (created ((cat common) (countable no) (lex "diner")))))))


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
     (partic ((possessor ((cat proper) (lex "Bo")))
	      (possessed ((cat common)
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
	    ;;         (effective yes)        is default
	    ;;         (effect-type affected) is default
	    ;;         (mode attributive)     is default          
	    (lex "make")))
     (partic ((agent ((cat proper) (lex "Nike")))
	      (affected ((cat proper) (lex "Bo")))
	      (carrier {^ affected})
	      (attribute ((cat ap) (lex "rich")))))))


  ;; @@@018 Bo makes the richest. @@COMPOSITE
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
	      (affected ((cat proper) (number plural) (lex "the Raider")))
	      (possessor {^ affected})
	      (possessed ((cat common) (lex "victory")))))))


  ;; agentive dispositive possessive attributive: Ag + Af/Pr + Pd
  (def-test t123bis
      "Bo gave the victory to the Raiders."
    ((cat clause)
     (tense past)
     (dative-move no)
     (proc ((type composite) (relation-type possessive) (lex "give")))
     (partic ((agent ((cat proper) (lex "Bo")))
	      (affected ((cat proper) (number plural) (lex "the Raider")))
	      (possessor {^ affected})
	      (possessed ((cat common) (lex "victory")))))))

       
  ;; agentive dispositive locative attributive: Ag + Af/Ld + Ln
  (def-test t124 
      "Bo lifted the Raiders to a victory."
    ((cat clause)
     (tense past)
     (proc ((type composite) (relation-type locative) (lex "lift")))
     (partic ((agent ((cat proper) (lex "Bo")))
	      (affected ((cat proper) (lex "the Raiders")))
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


  (format t "~%ir9 installed. 213 tests~%")
  (values)
  )