;;; -*- Mode:Lisp; Syntax:Common-Lisp; -*-
;;; ------------------------------------------------------------
;;; File        : IR6.L
;;; Description : Simple inputs to test GR6.
;;; Author      : Michael Elhadad
;;; Created     : 25-Jun-90
;;; Modified    : 09-Jan-91  (New circ. roles)
;;;               25 Jun 93  (Use def-test)
;;; Language    : Common Lisp
;;; Package     : FUG5
;;; ------------------------------------------------------------

(in-package "FUG5")

(defun isetup6 ()
  (clear-tests)

  (def-test t1
      "This car is expensive."
    ((cat clause)
     (process-type attributive)
     (carrier ((lex "car")
	       (cat common)
	       (distance near)))
     (attribute ((lex "expensive")
		 (cat adj)))))


  (def-test t2
      "John gives a blue book to Mary."
    ((cat clause)
     (process-type action)
     (agent ((lex "John")
	     (cat proper)))
     (medium ((lex "book")
	      (cat common)
	      (definite no)
	      (describer === "blue")))
     (benef ((lex "Mary")
	     (cat proper)))
     (verb ((process-class action)
	    (voice-class non-middle)
	    (transitive-class bitransitive)
	    (dative-prep "to")
	    (lex "give"))))) 


  (def-test t3
      "A science book is given by John to Mary."
    ((cat clause)
     (process-type action)
     (focus {medium})
     (agent ((lex "John")
	     (cat proper)))
     (medium ((lex "book")
	      (cat common)
	      (definite no)
	      (classifier === "science")))
     (benef ((lex "Mary")
	     (cat proper)))
     (verb ((process-class action)
	    (voice-class non-middle)
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
     (verb ((process-class mental)
	    (voice-class non-middle)
	    (lex "like")))))


  (def-test t5
      "John is the teacher."
    ((cat clause)
     (process-type equative)
     (identified ((lex "John")
		  (cat proper)))
     (identifier ((lex "teacher")
		  (cat common)))))


  (def-test t6
      "The president is John."
    ((cat clause)
     (process-type equative)
     (focus {identifier})
     (identified ((lex "John")
		  (cat proper)))
     (identifier ((lex "president")
		  (cat common)))))


  (def-test t7
      "Mary is thrown a heavy ball by John."
    ((cat clause)
     (process-type action)
     (agent ((lex "John")
	     (cat proper)))
     (medium ((lex "ball")
	      (cat common)
	      (definite no)
	      (describer === heavy)))
     (benef ((lex "Mary")
	     (cat proper)))
     (focus {benef})
     (verb ((process-class action)
	    (voice-class non-middle)
	    (transitive-class bitransitive)
	    (dative-prep "to")
	    (lex "throw")))))


  (def-test t8
      "John's book is beautiful."
    ((cat clause)
     (process-type attributive)
     (carrier ((lex "book")
	       (cat common)
	       (determiner ((lex "John")
			    (case possessive)
			    (cat proper)))))
     (attribute ((lex "beautiful")
		 (cat adj)))))


  (def-test t9
      "The SIG-display marker moves."
    ((cat clause)
     (mood finite)
     (process
      ((lex "move")
       (voice-class middle) 
       (transitive-class intransitive)))
     (process-type action) 
     (medium ((definite yes)
	      (cat common)
	      (lex "sig-display marker")))))

	
  (def-test t10
      "The SIG-display marker moves to the right."
    ((cat clause)
     (mood finite)
     (process
      ((lex "move")
       (voice-class middle) 
       (transitive-class intransitive)))
     (process-type action)  
     (medium ((definite yes)
	      (cat common)
	      (lex "sig-display marker")))
     (to-loc ((definite yes) (cat common) (lex "right")))))

	
  (def-test t11
      "The SIG-display marker moves from the left."
    ((cat clause)
     (mood finite)
     (process
      ((lex "move")
       (voice-class middle) 
       (transitive-class intransitive)))
     (process-type action)  
     (medium ((definite yes)
	      (cat common)
	      (lex "sig-display marker")))
     (from-loc ((definite yes) (cat common) (lex "left")))))

	
  (def-test t12
      "The SIG-display marker moves from the left to the right."
    ((cat clause)
     (mood finite)
     (process
      ((lex "move")
       (voice-class middle) 
       (transitive-class intransitive)))
     (process-type action)
     (medium ((definite yes)
	      (cat common)
	      (lex "sig-display marker")))
     (to-loc ((definite yes) (cat common) (lex "right")))
     (from-loc ((definite yes) (cat common) (lex "left")))))


	
  (def-test t13
      "The power level's increasing causes the sig-display marker to move to the right."
    ((speech-act assertive)
     (cat clause)
     (mood finite)
     (verb
      ((lex "cause")
       (voice-class non-middle)           
       (transitive-class transitive)
       (subject-clause present-participle)
       (object-clause infinitive)))
     (process-type action)
     (agent ((cat clause)
	     (verb ((lex "increase")
		    (voice-class middle)
		    (transitive-class transitive)))
	     (process-type action)
	     (medium ((definite yes)
		      (cat common)
		      (lex "power level")))))
     (medium ((cat clause)
	      (verb ((lex "move")
		     (voice-class middle) 
		     (transitive-class intransitive)))
	      (process-type action)  
	      (medium ((definite yes)
		       (cat common)
		       (lex "sig-display marker")))
	      (to-loc ((definite yes)
		       (cat common)
		       (lex "right")))))))


  (def-test t14
      "For her to do it."
    ((cat clause)
     (mood infinitive)
     (process-type action)
     (agent ((cat personal-pronoun)
	     (gender feminine)))
     (verb ((lex "do")
	    (voice-class non-middle)
	    (transitive-class transitive)))
     (medium ((cat personal-pronoun)
	      (gender neuter)))))


  (def-test t15
      "For her to do it is a bold statement."
    ((cat clause)
     (process-type attributive)
     (carrier ((cat clause)
	       (process-type action)
	       (agent ((cat personal-pronoun)
		       (gender feminine)))
	       (verb ((lex "do")
		      (voice-class non-middle)
		      (transitive-class transitive)))
	       (medium ((cat personal-pronoun)
			(gender neuter)))))
     (attribute ((cat common)
		 (definite no)
		 (lex "statement")
		 (describer === bold)))))


  (def-test t16
      "The man whom I know."
    ((cat common)
     (head === man)
     (definite yes)
     (animate yes)
     (qualifier ((cat clause)
		 (restrictive no)
		 (scope ((role phenomenon)))
		 (process-type mental)
		 (verb ((lex "know")
			(voice-class non-middle)
			(transitive-class transitive)))
		 (processor ((cat personal-pronoun)
			     (person first)))))))


  (def-test t17
      "The man that I know."
    ((cat common)
     (head === man)
     (definite yes)
     (qualifier ((cat clause)
		 (restrictive yes) ;; ***** here is the difference
		 (scope ((role phenomenon)))
		 (process-type mental)
		 (verb ((lex "know")
			(voice-class non-middle)
			(transitive-class transitive)))
		 (processor ((cat personal-pronoun)
			     (person first)))))))


  (def-test t18
      "The man that knows me."
    ((cat common)
     (head === man)
     (definite yes)
     (qualifier ((cat clause)
		 (restrictive yes)
		 (scope ((role processor)))
		 (process-type mental)
		 (verb ((lex "know")
			(voice-class non-middle)
			(transitive-class transitive)))
		 (phenomenon ((cat personal-pronoun)
			      (person first)))))))


  (def-test t19
      "The position to which the marker moves."
    ((cat common)
     (head === position)
     (definite yes)
     (qualifier ((cat clause)
		 (restrictive yes)
		 (scope ((role to-loc)))
		 (process-type action)
		 (verb ((lex "move")
			(voice-class middle)
			(transitive-class intransitive)))
		 (medium ((cat common)
			  (lex "marker")
			  (definite yes)))
		 (to-loc ((definite yes)
			  (cat common)
			  (lex "right")))))))


  (def-test t20
      "The position of the marker."
    ((cat common)
     (head === position)
     (definite yes)
     (qualifier ((cat pp)
		 (restrictive yes)
		 (np ((cat common)
		      (head === marker)
		      (definite yes)))))))


  (def-test t21
      "My wife, Carol."
    ((cat common)
     (head === wife)
     (determiner ((possessive yes)
		  (cat personal-pronoun)
		  (person first)))
     (qualifier ((cat proper)
		 (restrictive no)
		 (head === "Carol")))))


  (def-test t22
      "My brother Steve."
    ((cat common)
     (head === brother)
     (determiner ((possessive yes)
		  (cat personal-pronoun)
		  (person first)))
     (qualifier ((cat proper)
		 (restrictive yes) ;; **** Here is the difference
		 (head === "Steve")))))


  (def-test t23
      "An intelligent person."
    ((cat common)
     (head === person)
     (definite no)
     (describer ((lex "intelligent")))))


  (def-test t24
      "My brother Steve and my wife, Carol."
    ((cat list)
     (common ((cat np)))
     (distinct
      ~(((cat common)
	 (head === brother)
	 (determiner ((cat personal-pronoun)
		      (person first)))
	 (qualifier ((cat proper)
		     (restrictive yes)
		     (head === "Steve"))))
	((cat common)
	 (head === wife)
	 (determiner ((cat personal-pronoun)
		      (person first)))
	 (qualifier ((cat proper)
		     (restrictive no)
		     (head === "Carol"))))))))


  (def-test t25
      "My wife, Carol, my brother Steve and John."
    ((cat list)
     (common ((cat np)))
     (distinct
      ~(((cat common)
	 (head === wife)
	 (determiner ((cat personal-pronoun)
		      (person first)))
	 (qualifier ((cat proper)
		     (restrictive no)
		     (head === "Carol"))))
	((cat common)
	 (head === brother)
	 (determiner ((cat personal-pronoun)
		      (person first)))
	 (qualifier ((cat proper)
		     (restrictive yes)
		     (head === "Steve"))))
	((cat proper)
	 (head === "John"))))))


  (def-test t26
      "Take the hammer and hit the nail."
    ((cat list)
     (common ((cat clause)
	      (mood imperative)))
     (distinct
      ~(((process-type action)
	 (verb ((lex "take")
		(voice-class non-middle)
		(transitive-class transitive)))
	 (medium ((cat common)
		  (head === hammer)
		  (definite yes))))
	((process-type action)
	 (verb ((lex "hit")
		(voice-class non-middle)
		(transitive-class transitive)))
	 (medium ((cat common)
		  (head === nail)
		  (definite yes))))))))

		    
  (def-test t27
      "In order to install the battery, clean the box."
    ((cat clause)
     (mood imperative)
     (process-type action)
     (verb ((lex "clean")
	    (voice-class non-middle)
	    (transitive-class transitive)))
     (medium ((cat common)
	      (head === "box")
	      (definite yes)))
     (purpose ((process-type action)
	       (verb ((lex "install")
		      (voice-class non-middle)
		      (transitive-class transitive)))
	       (medium ((cat common)
			(head === "battery")
			(definite yes)))))))
		      



  (def-test t28
      "For her to do it must be a bold statement."
    ((cat clause)
     (process-type attributive)
     (epistemic-modality inference)
     (carrier ((cat clause)
	       (process-type action)
	       (agent ((cat personal-pronoun)
		       (gender feminine)))
	       (verb ((lex "do")
		      (voice-class non-middle)
		      (transitive-class transitive)))
	       (medium ((cat personal-pronoun)
			(gender neuter)))))
     (attribute ((cat common)
		 (definite no)
		 (lex "statement")
		 (describer === bold)))))



  (def-test t29
      "The man whom I may know."
    ((cat common)
     (head === man)
     (animate yes)
     (definite yes)
     (qualifier ((cat clause)
		 (restrictive no)
		 (scope ((role phenomenon)))
		 (process-type mental)
		 (epistemic-modality "may")
		 (verb ((lex "know")
			(voice-class non-middle)
			(transitive-class transitive)))
		 (processor ((cat personal-pronoun)
			     (person first)))))))


  (def-test t30
      "The SIG-display marker can move to the right."
    ((speech-act assertive)
     (tense present)
     (aspect ((perfective no) (progressive no)))
     (epistemic-modality possible)
     (cat clause)
     (mood finite)
     (verb
      ((lex "move")
       (voice-class middle) 
       (transitive-class intransitive)))
     (process-type action)  
     (medium ((definite yes)
	      (cat common)
	      (lex "sig-display marker")))
     (to-loc ((definite yes) (cat common) (lex "right")))))


  (def-test t31
      "First, take the hammer and then, hit the nail."
    ((cat list)
     (common ((cat clause)
	      (mood imperative)))
     (distinct
      ~(((process-type action)
	 (time-relater === first)
	 (verb ((lex "take")
		(voice-class non-middle)
		(transitive-class transitive)))
	 (medium ((cat common)
		  (head === hammer)
		  (definite yes))))
	((process-type action)
	 (time-relater === then)
	 (verb ((lex "hit")
		(voice-class non-middle)
		(transitive-class transitive)))
	 (medium ((cat common)
		  (head === nail)
		  (definite yes))))))))


  ;; t32 fails because epistemic-modality is not compatible with imperative.
  (def-test t32
      "Take the hammer and hit the nail."
    ((cat list)
     (common ((cat clause)
	      (mood imperative)))
     (distinct
      ~(((process-type action)
	 (verb ((lex "take")
		(voice-class non-middle)
		(transitive-class transitive)))
	 (medium ((cat common)
		  (head === hammer)
		  (definite yes))))
	((process-type action)
	 (verb ((lex "hit")
		(voice-class non-middle)
		(transitive-class transitive)))
	 (medium ((cat common)
		  (head === nail)
		  (definite yes))))))))


		    

  (def-test t33
      "Hit the nail with the hammer."
    ((process-type action)
     (cat clause)
     (mood imperative)
     (instrument ((cat common)
		  (head === hammer)
		  (definite yes)))
     (verb ((lex "hit")
	    (voice-class non-middle)
	    (transitive-class transitive)))
     (medium ((cat common)
	      (head === nail)
	      (definite yes)))))


  (def-test t34
      "Hit the nail using a hammer."
    ((process-type action)
     (cat clause)
     (mood imperative)
     (instrument ((cat common)
		  (head === hammer)
		  (definite no)))
     (verb ((lex "hit")
	    (instrument-prep "using")
	    (voice-class non-middle)
	    (transitive-class transitive)))
     (medium ((cat common)
	      (head === nail)
	      (definite yes)))))


  (def-test t35
      "Take the nail off using a screwdriver."
    ((process-type action)
     (cat clause)
     (mood imperative)
     (instrument ((cat common)
		  (head === screwdriver)
		  (definite no)))
     (verb ((lex "take")
	    (particle "off")
	    (instrument-prep "using")
	    (voice-class non-middle)
	    (transitive-class transitive)))
     (medium ((cat common)
	      (head === nail)
	      (definite yes)))))


  (def-test t36
      "The nail is taken off using a screwdriver."
    ((process-type action)
     (cat clause)
     (focus {medium})
     (instrument ((cat common)
		  (head === screwdriver)
		  (definite no)))
     (verb ((lex "take")
	    (particle "off")
	    (instrument-prep "using")
	    (voice-class non-middle)
	    (transitive-class transitive)))
     (medium ((cat common)
	      (head === nail)
	      (definite yes)))))


  ;; another way of specifying the prep for the role instrument.
  (def-test t37
      "Take the nail off using a screwdriver."
    ((process-type action)
     (cat clause)
     (mood imperative)
     (instrument ((cat common)
		  (prep "using")
		  (head === screwdriver)
		  (definite no)))
     (verb ((lex "take")
	    (particle "off")
	    (voice-class non-middle)
	    (transitive-class transitive)))
     (medium ((cat common)
	      (head === nail)
	      (definite yes)))))


  ;; Test accompaniment role
  (store-verbs '( ("line up" "lines up" "lined up" "lining up" "lined up")) )
  (def-test t38
      "The battery lines up with the compartment."
    ((process-type action)
     (cat clause)
     (mood finite)
     (tense present)
     (accompaniment ((cat common)
		     (head === compartment)
		     (definite yes)))
     (process ((lex "line up")
	       (voice-class middle)
	       (transitive-class intransitive)))
     (medium ((cat common)
	      (head === battery)
	      (definite yes)))))



  ;; Test reason role
  (def-test t39
      "He does it because of you."
    ((process-type action)
     (cat clause)
     (mood finite)
     (tense present)
     (process ((lex "do")
	       (voice-class non-middle)
	       (transitive-class transitive)))
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
    ((process-type action)
     (cat clause)
     (mood finite)
     (tense present)
     (process ((lex "do")
	       (voice-class non-middle)
	       (transitive-class transitive)))
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
	      (verb ((lex "like")
		     (voice-class non-middle)
		     (transitive-class transitive)))
	      (phenomenon ((cat personal-pronoun)
			   (person second)
			   (number singular)))))))


  ;; Test reason role
  (def-test t41
      "He scored 39 points for the Lakers."
    ((process-type action)
     (cat clause)
     (mood finite)
     (tense past)
     (process ((cat verb-group)
	       (lex "score")
	       (voice-class non-middle)
	       (transitive-class transitive)))
     (agent ((cat personal-pronoun)
	     (animate yes)
	     (gender masculine)
	     (person third)
	     (number singular)))
     (medium ((cat common)
	      (lex "39 points")
	      (countable no))) ;; I don't have numbers yet
     (behalf ((cat proper)
	      (lex "the Lakers")))))

  ;; Add examples for all roles in focus of relative clauses

  (format t "~%ir6 installed. 41 tests~%")
  (values))

