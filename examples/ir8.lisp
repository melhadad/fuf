;;; -*- Mode:Lisp; Syntax:Common-Lisp; -*-
;;; ------------------------------------------------------------
;;; File        : IR8.L
;;; Description : Sample inputs to test GR8.
;;; Author      : Michael Elhadad and Jacques Robin
;;; Created     : 19 Apr 91
;;; Modified    : 08 May 91 - Added wh and yes-no examples
;;; Language    : Common Lisp
;;; ------------------------------------------------------------

(in-package "FUG5")

(defun isetup8 ()

  ;; Extended and new defaults:
  ;; defaults for (cat clause):
  ;;	        (mood declarative)
  ;;	        (tense present)

  ;; defaults for process:
  ;;              (voice-class non-middle)
  ;;              (transitive-class transitive)

  ;; defaults for (cat np):
  ;;              (countable yes)
  ;;              (number singular)
  ;;		(definite yes)
  (clear-tests)

  (def-test t1
      "This car is expensive."
    ((cat clause)
     (process-type ascriptive)
     (relation-mode attributive)
     (carrier ((lex "car")
	       (cat common)
	       (distance near)))
     (attribute === "expensive")))


  (def-test t2
      "John gives a blue book to Mary."
    ((cat clause)
     (process-type material)
     (agent ((lex "John")
	     (cat proper)))
     (medium ((lex "book")
	      (cat common)
	      (definite no)
	      (describer === "blue")))
     (benef ((lex "Mary")
	     (cat proper)))
     (process ((process-class material)
	       (transitive-class bitransitive)
	       (dative-prep "to")
	       (lex "give"))))) 


  (def-test t3
      "A science book is given by John to Mary."
    ((cat clause)
     (process-type material)
     (focus {medium})
     (agent ((lex "John")
	     (cat proper)))
     (medium ((lex "book")
	      (cat common)
	      (definite no)
	      (classifier === "science")))
     (benef ((lex "Mary")
	     (cat proper)))
     (process ((process-class material)
	       (transitive-class bitransitive) 
	       (dative-prep "to")
	       (lex "give"))))) 

  (def-test t4
      "John likes Mary."
    ((cat clause)
     (process-type mental)
     (processor ((lex "John")
		 (cat proper)))
     (phenomenon ((lex "Mary")
		  (cat proper)))
     (process ((process-class mental)
	       (lex "like")))))


  (def-test t5
      "John is the teacher."
    ((cat clause)
     (process-type ascriptive)
     (relation-mode equative)
     (identified ((lex "John")
		  (cat proper)))
     (identifier ((lex "teacher")
		  (cat common)))))


  (def-test t6
      "The president is John."
    ((cat clause)
     (process-type ascriptive)
     (relation-mode equative)
     (focus {identifier})
     (identified ((lex "John")
		  (cat proper)))
     (identifier ((lex "president")
		  (cat common)))))


  (def-test t7
      "Mary is thrown a heavy ball by John."
    ((cat clause)
     (process-type material)
     (agent ((lex "John")
	     (cat proper)))
     (medium ((lex "ball")
	      (cat common)
	      (definite no)
	      (describer === "heavy")))
     (benef ((lex "Mary")
	     (cat proper)))
     (focus {benef})
     (process ((process-class material)
	       (transitive-class bitransitive)
	       (dative-prep "to")
	       (lex "throw")))))


  (def-test t8
      "John's book is beautiful."
    ((cat clause)
     (process-type ascriptive)
     (relation-mode attributive)
     (carrier ((lex "book")
	       (cat common)
	       (possessor ((lex "John")
			   (cat proper)))))
     (attribute === "beautiful")))


  (def-test t9
      "The SIG-display marker moves."
    ((cat clause)
     (process
      ((lex "move")
       (voice-class middle)))
     (process-type material) 
     (medium ((cat common)
	      (lex "SIG-display marker")))))


  (def-test t10
      "The SIG-display marker moves to the right."
    ((cat clause)
     (process
      ((lex "move")
       (voice-class middle)))
     (process-type material)  
     (medium ((cat common)
	      (lex "sig-display marker")))
     (to-loc ((cat common) (lex "right")))))
  ;; JR comment: "to the right" really is a participant of the clause, NOT
  ;; a circumstance. To revise using Talmy & Fawcett is the next version.
  ;; Idem for the t11, t12, t19 and t30.

	
  (def-test t11
      "The SIG-display marker moves from the left."
    ((cat clause)
     (process
      ((lex "move")
       (voice-class middle)))
     (process-type material)  
     (medium ((cat common)
	      (lex "sig-display marker")))
     (from-loc ((cat common) (lex "left")))))

	
  (def-test t12
      "The SIG-display marker moves from the left to the right."
    ((cat clause)
     (process
      ((lex "move")
       (voice-class middle)))
     (process-type material)
     (medium ((cat common)
	      (lex "sig-display marker")))
     (to-loc ((cat common) (lex "right")))
     (from-loc ((cat common) (lex "left")))))


	
  (def-test t13
      "the power level's increasing causes the sig-display marker to move to the right."
    ((speech-act assertive)
     (cat clause)
     (process
      ((lex "cause")
       (subject-clause present-participle)
       (object-clause infinitive)))
     (process-type material)
     (agent ((cat clause)
	     (verb ((lex "increase")
		    (voice-class middle)))
	     (process-type material)
	     (medium ((cat common)
		      (lex "power level")))))
     (medium ((cat clause)
	      (verb ((lex "move")
		     (voice-class middle) 
		     (transitive-class intransitive)))
	      (process-type material)  
	      (medium ((cat common)
		       (lex "sig-display marker")))
	      (to-loc ((cat common)
		       (lex "right")))))))


  (def-test t14
      "For her to do it."
    ((cat clause)
     (mood infinitive)
     (process-type material)
     (agent ((cat personal-pronoun)
	     (gender feminine)))
     (process ((lex "do")))
     (medium ((cat personal-pronoun)
	      (gender neuter)))))


  (def-test t15
      "For her to do it is a bold statement."
    ((cat clause)
     (process-type ascriptive)
     (relation-mode attributive)
     (carrier ((cat clause)
	       (process-type material)
	       (agent ((cat personal-pronoun)
		       (gender feminine)))
	       (process === "do")
	       (medium ((cat personal-pronoun)
			(gender neuter)))))
     (attribute ((cat common)
		 (definite no)
		 (lex "statement")
		 (describer === "bold")))))


  (def-test t16
      "The man whom I know."
    ((cat common)
     (head === "man")
     (animate yes)
     (qualifier ((cat clause)
		 (restrictive no)
		 (scope ((role phenomenon))) 
		 (process-type mental)
		 (process ((lex "know")
			   (transitive-class transitive)))
		 (processor ((cat personal-pronoun)
			     (person first)))))))


  (def-test t17
      "The man that I know."
    ((cat common)
     (head === "man")
     (qualifier ((cat clause)
		 (restrictive yes) ;; ***** here is the difference
		 (scope ((role phenomenon)))
		 (process-type mental)
		 (process ((lex "know")
			   (transitive-class transitive)))
		 (processor ((cat personal-pronoun)
			     (person first)))))))



  (def-test t18
      "The man that knows me."
    ((cat common)
     (head === "man")
     (qualifier ((cat clause)
		 (restrictive yes)
		 (scope ((role processor)))
		 (process-type mental)
		 (process ((lex "know")
			   (transitive-class transitive)))
		 (phenomenon ((cat personal-pronoun)
			      (person first)))))))


  (def-test t19
      "The position to which the marker moves."
    ((cat common)
     (head === "position")
     (qualifier ((cat clause)
		 (restrictive yes)
		 (scope ((role to-loc)))
		 (process-type material)
		 (process ((lex "move")
			   (voice-class middle)))
		 (medium ((cat common)
			  (head === "marker")))
		 (to-loc ((cat common)
			  (head === "right")))))))


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
      ~(((process-type material)
	 (process === "take")
	 (medium ((cat common)
		  (head === "hammer"))))
	((process-type material)
	 (process === "hit")
	 (medium ((cat common)
		  (head === "nail"))))))))

		    
  (def-test t27
      "In order to install the battery, clean the box."
    ((cat clause)
     (mood imperative)
     (process-type material)
     (process === "clean")
     (medium ((cat common)
	      (head === "box")))
     (purpose ((process-type material)
	       (verb === "install")
	       (medium ((cat common)
			(head === "battery")))))))




  ;; New treatment of relational processes - 
  ;; ascriptive, circumstantial or possessive 
  ;; Relation-mode is either attributive or equative
  (def-test t28
      "For her to do it must be a bold statement."
    ((cat clause)
     (process-type ascriptive)
     (relation-mode attributive)
     (epistemic-modality inference)
     (carrier ((cat clause)
	       (process-type material)
	       (agent ((cat personal-pronoun)
		       (gender feminine)))
	       (process === "do")
	       (medium ((cat personal-pronoun)
			(gender neuter)))))
     (attribute ((cat common)
		 (definite no)
		 (head === "statement")
		 (describer === "bold")))))



  (def-test t29
      "The man whom I may know."
    ((cat common)
     (head === man)
     (animate yes)
     (qualifier ((cat clause)
		 (restrictive no)
		 (scope ((role phenomenon)))
		 (process-type mental)
		 (epistemic-modality "may")
		 (process === "know")
		 (processor ((cat personal-pronoun)
			     (person first)))))))


  ;; Cf comment on t11 about move and to-loc
  (def-test t30
      "The SIG-display marker can move to the right."
    ((cat clause)
     (epistemic-modality possible)
     (process ((lex "move")
	       (voice-class middle)))
     (process-type material)  
     (medium ((cat common)
	      (lex "sig-display marker")))
     (to-loc ((cat common) (lex "right")))))


  (def-test t31
      "First, take the hammer and then, hit the nail."
    ((cat clause)
     (complex conjunction) 
     (common ((mood imperative)))
     (distinct
      ~(((time-relater === "first")
	 (process-type material)
	 (process === "take")
	 (medium ((cat common)
		  (head === "hammer"))))
	((time-relater === "then")
	 (process-type material)
	 (process === "hit")
	 (medium ((cat common)
		  (head === "nail"))))))))

  ;; t32 fails because epistemic-modality is not compatible with imperative.
  (def-test t32
      "<fail>"
    ((cat clause)
     (complex conjunction) 
     (common ((mood imperative)))
     (distinct
      ~(((epistemic-modality possible)
	 (process-type material)
	 (process === "take")
	 (medium ((cat common)
		  (head === "hammer"))))
	((process === "hit")
	 (process-type material)
	 (medium ((cat common)
		  (head === "nail"))))))))
		    

  (def-test t33
      "Hit the nail with the hammer."
    ((process-type material)
     (cat clause)
     (mood imperative)
     (instrument ((cat common)
		  (head === "hammer")))
     (process ===  "hit")
     (medium ((cat common)
	      (head === "nail")))))


  (def-test t34
      "Hit the nail using a hammer."
    ((process-type material)
     (cat clause)
     (mood imperative)
     (instrument ((cat common)
		  (head === hammer)
		  (definite no)))
     (process ((lex "hit")
	       (instrument-prep "using")))
     (medium ((cat common)
	      (head === nail)))))


  (def-test t35
      "Take the nail off using a screwdriver."
    ((process-type material)
     (cat clause)
     (mood imperative)
     (instrument ((cat common)
		  (head === screwdriver)
		  (definite no)))
     (process ((lex "take")
	       (particle "off")
	       (instrument-prep "using")))
     (medium ((cat common)
	      (head === nail)))))


  (def-test t36
      "The nail is taken off using a screwdriver."
    ((process-type material)
     (cat clause)
     (focus {medium})
     (instrument ((cat common)
		  (head === screwdriver)
		  (definite no)))
     (process ((lex "take")
	       (particle "off")
	       (instrument-prep "using")))
     (medium ((cat common)
	      (head === nail)))))


  ;; another way of specifying the prep for the role instrument.
  (def-test t37
      "Take the nail off using a screwdriver."
    ((process-type material)
     (cat clause)
     (mood imperative)
     (instrument ((cat common) 
		  (prep "using")
		  (head === screwdriver)
		  (definite no)))
     (process ((lex "take")
	       (particle "off")))
     (medium ((cat common)
	      (head === nail)))))



  ;; Add an irregular verb to the lexicon with store-verbs
  ;; Treatment of non-movable particles as a single string (sort of hack)
  ;; Test accompaniment role
  (store-verbs '( ("line up" "lines up" "lined up" "lining up" "lined up")) )
  (def-test t38
      "The battery lines up with the compartment."
    ((process-type material)
     (cat clause)
     (accompaniment ((cat common)
		     (head === compartment)))
     (process ((lex "line up")
	       (voice-class middle)))
     (medium ((cat common)
	      (head === battery)))))


  ;; Test reason role
  (def-test t39
      "He does it because of you."
    ((process-type material)
     (cat clause)
     (process === "do")
     (agent ((cat personal-pronoun)
	     (animate yes)
	     (gender masculine)
	     (person third)
	     (number singular)))
     (medium ((cat personal-pronoun)
	      (gender neuter)
	      (person third)
	      (number singular)))
     (reason ((cat personal-pronoun)
	      (gender masculine)
	      (person second)
	      (number singular)))))


  ;; Test reason role with a clause
  (def-test t40
      "He does it because he likes you."
    ((process-type material)
     (cat clause)
     (process === "do")
     (agent ((cat personal-pronoun)
	     (animate yes)
	     (gender masculine)
	     (person third)
	     (number singular)))
     (medium ((cat personal-pronoun)
	      (gender neuter)
	      (person third)
	      (number singular)))
     (reason ((cat clause)
	      (process-type mental)
	      (processor {agent})
	      (verb === "like")
	      (phenomenon ((cat personal-pronoun)
			   (person second)
			   (number singular)))))))


  ;; Test reason role
  (def-test t41
      "He scored 39 points for the Lakers."
    ((process-type material)
     (cat clause)
     (tense past)
     (process ((cat verb-group)
	       (lex "score")))
     (agent ((cat personal-pronoun)
	     (animate yes)
	     (gender masculine)
	     (person third)
	     (number singular)))
     (medium ((cat common)
	      (definite no)
	      (cardinal ((value 39)))
	      (lex "point")))
     (behalf ((cat proper)
	      (lex "the Lakers")))))


  ;; Test cardinal with a string
  (def-test t41bis
      "He scored 39 points for the Lakers."
    ((process-type material)
     (cat clause)
     (tense past)
     (process ((cat verb-group)
	       (lex "score")))
     (agent ((cat personal-pronoun)
	     (animate yes)
	     (gender masculine)
	     (person third)
	     (number singular)))
     (medium ((cat common)
	      (definite no)
	      (number plural) ;; cannot understand that "39" is plural!
	      (cardinal ((value "39")))
	      (lex "point")))
     (behalf ((cat proper)
	      (lex "the Lakers")))))



  ;; More and more on relational processes 
  ;; relation-mode equative when relation is symmetrical (can be passivized)
  ;; circumstance-as process|participant 
  ;; typically what expresses the relation is the verb (full verb) otherwise,
  ;; the relation (circumstance) is expressed in the participant and the verb
  ;; is "to be" (empty).  In such case, the participants are realized by a
  ;; PP (in general).
  (def-test t42
      "The holding battery cover plate covers the holding battery compartment."
    ((cat clause)
     (process-type circumstantial)
     (relation-mode equative)
     (circumstance-as process)
     (process ((cat verb-group)
	       (lex "cover")))
     (focus {identified})
     ;; Don't analyze the complicated np (macro-code it).
     (identified ((cat np)
		  (head ((cat noun)
			 (lex "holding battery cover plate")))))
     (identifier ((cat np)
		  (head ((cat noun)
			 (lex "holding battery compartment")))))))



  ;; Here is a circumstance-as participant for a location relation.
  ;; In addition, the location is an apposition of PPs.
  (def-test t43
      "The holding battery compartment is in the back of the RT, at the bottom left corner."
    ((cat clause)
     (process-type circumstantial)
     (relation-mode attributive)
     (circumstance-as participant)
     (focus {carrier})
     (carrier ((cat np)
	       (lex "holding battery compartment")))
     (attribute ((cat pp)
		 (complex apposition)
		 (restrictive no)
		 (distinct 
		  ~(((prep ((cat prep) (lex "in")))
		     (np ((cat np)
			  (head ((cat noun) (lex "back")))
			  (qualifier ((cat pp) ;; of is default
				      (np ((cat np)
					   (head ((cat noun)
						  (lex "RT"))))))))))
		    ((prep ((cat prep) (lex "at")))
		     (np ((cat np)
			  ;; until we get a real semantic description of
			  ;; NPs modifiers a flat list is best for several
			  ;; describers or qualifiers.
			  (describer ((cat list) 
				      (elements ~(((cat ap) (lex "bottom"))
						  ((cat ap) (lex "left"))))))
			  (head ((cat noun) (lex "corner"))))))))))))



  ;; TEST: try your own syntactic analysis of this one and come up with 3
  ;; reasons why it is inferior to this one.
  ;; Send answers to robin@cs.columbia.edu
  (def-test t44
      "The holding battery compartment contains the old holding battery shown in the cut-away view."
    ((cat clause)
     (process-type circumstantial)
     (relation-mode equative)
     (circumstance-as process)
     (process ((cat verb-group) (lex "contain")))
     (focus {identified})
     (identified ((cat np)
		  (head ((cat noun) 
			 (lex "holding battery compartment")))))
     (identifier
      ((cat np)
       (head ((cat noun) (lex "old holding battery")))
       (qualifier 
	((cat ap) ;; JR comment: ap = Adjectival Phrase
	 (head === "shown")
	 (qualifier ((cat pp)
		     (prep ((cat prep) (lex "in")))
		     (np ((cat np)
			  (head ((cat noun)
				 (lex "cut-away view")))))))))))))


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
     (process-type ascriptive)
     (relation-mode attributive)
     (carrier ((head === "year")
	       (ordinal ((value 1) (digit no)))
	       (cardinal ((value 2) (digit no)))
	       (possessor ((cat personal-pronoun)
			   (number singular)
			   (person first)))))
     (tense past)
     (attribute ((cat ap)
		 (complex conjunction)
		 (distinct ~( ((head === "happy"))
			      ((head === "uneventful")) ))))))


  ;; Test possessive clauses
  (def-test t48
      "Old McDonald had a farm."
    ((cat clause)
     (process-type possessive)
     (relation-mode attributive)
     (tense past)
     (possessor ((cat proper)
		 (head === "Old McDonald")))
     (possessed ((cat common)
		 (head === farm)
		 (definite no)))))


  ;; Equative attributive
  (def-test t49
      "Old McDonald owned a farm."
    ((cat clause)
     (process-type possessive)
     (relation-mode equative)
     (tense past)
     (possessor ((cat proper)
		 (head === "Old McDonald")))
     (possessed ((cat common)
		 (head === farm)
		 (definite no)))))

  
  ;; Equative attributive
  (def-test t50
      "A farm was owned by Old McDonald."
    ((cat clause)
     (process-type possessive)
     (relation-mode equative)
     (focus {possessed})
     (tense past)
     (possessor ((cat proper)
		 (head === "Old McDonald")))
     (possessed ((cat common)
		 (head === farm)
		 (definite no)))))


  ;; Test reason role
  (def-test t51
      ("To be innocent, you must do it for him to eat."
       "In order for you to be innocent, you must do it for him to eat.")
    ((process-type material)
     (cat clause)
     (deontic-modality duty)
     (process === do)
     (agent ((cat personal-pronoun)
	     (animate yes)
	     (person second)
	     (number singular)))
     (medium ((cat personal-pronoun)
	      (gender neuter)
	      (person third)
	      (number singular)))
     (purpose ((cat clause)
	       (process-type ascriptive)
	       (relation-type attributive)
	       (carrier {agent})
	       (attribute === innocent)))
     (behalf ((cat clause)
	      (process-type material)
	      (agent ((cat personal-pronoun)
		      (animate yes)
		      (person third)
		      (gender masculine)
		      (number singular)))
	      (process === eat)))))


  ;; Test questions 

  (def-test t52
      "Is this car expensive?"
    ((cat clause)
     (mood yes-no)
     (process-type ascriptive)
     (relation-mode attributive)
     (carrier ((lex "car")
	       (cat common)
	       (distance near)))
     (attribute === "expensive")))


  (def-test t53
      "Will John give a blue book to Mary?"
    ((cat clause)
     (process-type material)
     (mood yes-no)
     (tense future)
     (agent ((lex "John")
	     (cat proper)))
     (medium ((lex "book")
	      (cat common)
	      (definite no)
	      (describer === "blue")))
     (benef ((lex "Mary")
	     (cat proper)))
     (process ((process-class material)
	       (transitive-class bitransitive)
	       (dative-prep "to")
	       (lex "give")))))


  (def-test t54
      "John did give a blue book to Mary."
    ((cat clause)
     (process-type material)
     (tense past)
     (insistence yes)
     (agent ((lex "John")
	     (cat proper)))
     (medium ((lex "book")
	      (cat common)
	      (definite no)
	      (describer === "blue")))
     (benef ((lex "Mary")
	     (cat proper)))
     (process ((process-class material)
	       (transitive-class bitransitive)
	       (dative-prep "to")
	       (lex "give")))))


  (def-test t55
      "What is expensive?"
    ((cat clause)
     (mood wh)
     (process-type ascriptive)
     (relation-mode attributive)
     (scope ((role carrier)
	     (animate no)))
     (attribute === "expensive")))



  (def-test t56
      "What is a classic?"
    ((cat clause)
     (mood wh)
     (process-type ascriptive)
     (relation-mode attributive)
     (scope ((role carrier)
	     (animate no)))
     (attribute ((cat common)
		 (definite no)
		 (lex "classic")))))

  ;; JR Note: The scope can't simply be "attribute", since then it would be
  ;; impossible to choose the question pronoun (e.g. in t55 it would be "How"
  ;; or "How expensive", in t56 it would be "What" or "What kind of")
  ;; Also note the ambiguity of t56.


  (def-test t57
      "Who is your father?"
    ((cat clause)
     (mood wh)
     (process-type ascriptive)
     (relation-mode equative)
     (scope ((role identified)
	     (animate yes)))
     (identifier ((cat common)
		  (possessor ((cat personal-pronoun)
			      (person second)))
		  (head === "father")))))

		       

  (def-test t58
      "Who is your father?"
    ((cat clause)
     (mood wh)
     (process-type ascriptive)
     (relation-mode equative)
     (scope ((role identifier)
	     (animate yes)))
     (identified ((cat common)
		  (possessor ((cat personal-pronoun)
			      (person second)))
		  (head === "father")))))

  (def-test t58bis
      "Which is your father?"
    ((cat clause)
     (mood wh)
     (process-type ascriptive)
     (relation-mode equative) 
     (scope ((role identifier)
	     (restrictive yes)
	     (animate yes)))
     (identified ((cat common)
		  (possessor ((cat personal-pronoun)
			      (person second)))
		  (head === "father")))))


  (def-test t59
      "Where is your mother?"
    ((cat clause)
     (mood wh)
     (process-type locative)
     (relation-mode attributive)
     (circumstance-as participant)
     (scope ((role location)
	     (animate no)))
     (located ((cat common)
	       (possessor ((cat personal-pronoun)
			   (person second)))
	       (head === "mother")))))



  (def-test t60
      "Who is in your house?"
    ((cat clause)
     (mood wh)
     (process-type locative)
     (relation-mode attributive)
     (circumstance-as participant)
     (scope ((role located)
	     (animate yes)))
     (location ((cat pp)
		(prep === "in")
		(np ((cat common)
		     (possessor ((cat personal-pronoun)
				 (person second)))
		     (head === "house")))))))



  (def-test t61
      "What covers the opening?"
    ((cat clause)
     (mood wh)
     (process-type locative)
     (relation-mode equative)
     (circumstance-as process)
     (scope ((role located)
	     (animate no)))
     (process === "cover")
     (location ((cat common)
		(head === "opening")))))



  (def-test t62
      "What does the seal cover?"
    ((cat clause)
     (mood wh)
     (process-type locative)
     (relation-mode equative)
     (circumstance-as process)
     (scope ((role location)
	     (animate no)))
     (process === "cover")
     (located ((cat common)
	       (head === "seal")))))



  (def-test t63
      "When is the game?"
    ((cat clause)
     (mood wh)
     (process-type temporal)
     (relation-mode attributive)
     (circumstance-as participant)
     (scope ((role time)))
     (located ((cat common)
	       (head === "game")))))


  ;; JR note: how to do "At what time is the game?" ?!?!?!?!?


  (def-test t64
      "What happens then?"
    ((cat clause)
     (mood wh)
     (process-type temporal)
     (relation-mode attributive)
     (circumstance-as participant)
     (process === happen)
     (scope ((role located)))
     (time ((cat adv) (lex "then")))))


  (store-verbs '( ("happen" "happens" "happened" "happening" "happened")) )
  (def-test t64bis
      "The game happened then."
    ((cat clause)
     (process-type temporal)
     (relation-mode attributive)
     (circumstance-as participant)
     (tense past)
     (process === happen)
     (located ((cat np)
	       (head === game)))
     (time ((cat adv) (lex "then")))))



  ;; JR note: equative and circumstance-as-process flavors seem difficult
  ;; without specifying a time unit in the question (e.g. "What day is the
  ;; 10th?" "How long does the fair last?")


  (def-test t65
      "How is your sister?"
    ((cat clause)
     (mood wh)
     (process-type circumstantial)
     (relation-mode attributive)
     (circumstance-as participant)
     (scope ((role attribute)))
     (carrier ((cat common)
	       (possessor ((cat personal-pronoun)
			   (person second)))
	       (head === "sister")))))


  (def-test t66
      "With whom is your sister?"
    ((cat clause)
     (mood wh)
     (process-type circumstantial)
     (relation-mode attributive)
     (circumstance-as participant)
     (scope ((role accompaniment)
	     (animate yes)))
     (attribute {accompaniment-comp}) ;; not nice ...
     (carrier ((cat common)
	       (possessor ((cat personal-pronoun)
			   (person second)))
	       (head === "sister")))))


  ;; JR note: before doing all circumstantial roles (and in all flavors) in
  ;; relation, decide whether we take Halliday's stance on the issue rather
  ;; than Fawcett.


  (def-test t67
      "Who has a PhD.?"
    ((cat clause)
     (mood wh)
     (process-type possessive)
     (relation-mode attributive)
     (scope ((role possessor)
	     (animate yes)))
     (possessed ((cat common) 
		 (definite no)
		 (head === "PhD.")))))



  (def-test t68
      "What does she have?"
    ((cat clause)
     (mood wh)
     (process-type possessive)
     (relation-mode attributive)
     (scope ((role possessed)
	     (animate no)))
     (possessor ((cat personal-pronoun)
		 (gender feminine)
		 (number singular)
		 (person third)))))

  

  (def-test t69
      "Who owns this book?"
    ((cat clause)
     (mood wh)
     (process-type possessive)
     (relation-mode equative)
     (scope ((role possessor)
	     (animate yes)))
     (possessed ((cat common) 
		 (distance near)
		 (head === "book")))))



  (def-test t70
      "What does she own?"
    ((cat clause)
     (mood wh)
     (process-type possessive)
     (relation-mode equative)
     (scope ((role possessed)
	     (animate no)))
     (possessor ((cat personal-pronoun)
		 (gender feminine)
		 (person third)
		 (number singular)))))

  (def-test t70bis
      "Which does she own?"
    ((cat clause)
     (mood wh)
     (process-type possessive)
     (relation-mode equative)
     (scope ((role possessed)
	     (restrictive yes)
	     (animate no)))
     (possessor ((cat personal-pronoun)
		 (gender feminine)
		 (person third)
		 (number singular)))))


  (def-test t71
      "Who gives a blue book to Mary?"
    ((cat clause)
     (process-type material)
     (mood wh)
     (scope ((role agent)
	     (animate yes)))
     (medium ((lex "book")
	      (cat common)
	      (definite no)
	      (describer === "blue")))
     (benef ((lex "Mary")
	     (cat proper)))
     (process ((process-class material)
	       (transitive-class bitransitive)
	       (dative-prep "to")
	       (lex "give")))))



  (def-test t72
      "What will John give to Mary?"
    ((cat clause)
     (process-type material)
     (mood wh)
     (tense future)
     (scope ((role medium)
	     (animate no)))
     (agent ((lex "John")
	     (cat proper)))
     (benef ((lex "Mary")
	     (cat proper)))
     (process ((process-class material)
	       (transitive-class bitransitive)
	       (dative-prep "to")
	       (lex "give")))))



  (def-test t73
      "To whom will John give a blue book?"
    ((cat clause)
     (process-type material)
     (mood wh)
     (tense future)
     (scope ((role benef)
	     (animate yes)))
     (agent ((lex "John")
	     (cat proper)))
     (medium ((lex "book")
	      (cat common)
	      (definite no)
	      (describer === "blue")))
     (process ((process-class material)
	       (transitive-class bitransitive)
	       (dative-prep "to")
	       (lex "give")))))



  (def-test t74
      "What does Deborah prefer?"
    ((cat clause)
     (process-type mental)
     (mood wh)
     (scope ((role phenomenon)
	     (animate no)))
     (process === "prefer")
     (processor ((cat proper) (lex "Deborah")))))



  (def-test t75
      "Who worships Baal?"
    ((cat clause)
     (process-type mental)
     (mood wh)
     (scope ((role processor)
	     (animate yes)))
     (process === "worship")
     (phenomenon ((cat proper) (lex "Baal")))))


  ;; JR Question: How to make generic reference like "Who worships FIRE" or
  ;; "They want MONEY" ?


  ;; Add test for every role in relative clauses

  ;; Test reason role with a clause
  (def-test t76
      "Why does he do it?"
    ((process-type material)
     (mood wh)
     (cat clause)
     (process === "do")
     (agent ((cat personal-pronoun)
	     (animate yes)
	     (gender masculine)
	     (person third)
	     (number singular)))
     (medium ((cat personal-pronoun)
	      (gender neuter)
	      (person third)
	      (number singular)))
     (scope ((role reason)))))

  (def-test t77
      "From where does the SIG-display marker move to the right?" 
    ((cat clause)
     (mood wh)
     (process ((lex "move")
	       (voice-class middle)))
     (process-type material)
     (medium ((cat common)
	      (lex "sig-display marker")))
     (to-loc ((cat common) (lex "right")))
     (scope ((role from-loc)))))

  (format t 
	  "
ir8 installed. 81 tests.
=================================================================
Use function (test) to try examples.
Use function (do-tenses fd :from 1 :to 36 :passive nil :polarity t :question t)
to test tense formation, passivation, negation and interrogation.
=================================================================
NOTE: some examples do not work with gr8 but work with gr9 which is
an improved version of gr8.
=================================================================

")
  (values))

(defun do-tenses (fd &key (from 1) (to 36)
		     (passive t)  (polarity t) (question t))
  "Take a clause fd and generate it at all tenses between from and to.
   If passive is non-nil, each tense is done active and passive.
   If polarity is non-nil, each tense is done both positive and negative."
  (do* ((i from (1+ i))
	(tense (or (find-symbol (format nil "TENSE-~s" i))
		   (find-symbol (format nil "tense-~s" i)))
	       (or (find-symbol (format nil "TENSE-~s" i))
		   (find-symbol (format nil "tense-~s" i)))))
       ((> i to) 
	(values))
       (format t "~%=======================~%")
       (format t "~s~%" tense)
       (let ((tfd (cons `(tense ,tense) fd)))
	 (uni tfd :limit 1000)
	 (when passive
	   (uni (cons '(voice receptive) tfd) :limit 1000))
	 (when polarity
	   (uni (cons '(polarity negative) tfd) :limit 1000))
	 (when question
	   (uni (cons '(mood yes-no) tfd) :limit 1000))
	 (when (and polarity passive)
	   (uni (cons '(voice receptive)
		      (cons '(polarity negative) tfd)) :limit 1000))
	 (when (and polarity question)
	   (uni (cons '(mood yes-no) 
		      (cons '(polarity negative) tfd)) :limit 1000))
	 (when (and polarity passive question)
	   (uni (cons '(mood yes-no) 
		      (cons '(polarity negative) 
			    (cons '(voice receptive) tfd))) :limit 1000)))))









