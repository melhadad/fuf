;;; -*- Mode:Lisp; Syntax:Common-Lisp; Package:FUG5 -*-
;;; ------------------------------------------------------------
;;; File        : IR4.L
;;; Description : Simple inputs to test GR4.
;;; Author      : Michael Elhadad
;;; Created     : 20-Jun-88
;;; Modified    : 07-Aug-89
;;;               18 May 90
;;; Language    : Common Lisp
;;; Package     : FUG5
;;; ------------------------------------------------------------

(in-package "FUG5")

(defun isetup4 ()
  (clear-tests)

  (def-test t1 
      "This car is expensive."
    ((cat clause)
     (process-type attributive)
     (carrier ((lex "car")
	       (np-type common)
	       (distance near)))
     (attribute === "expensive")))

  (def-test t2 
      "John gives a blue book to Mary."
    ((cat clause)
     (process-type action)
     (agent ((lex "John")
	     (np-type proper)))
     (medium ((lex "book")
	      (np-type common)
	      (definite no)
	      (describer === "blue")))
     (benef ((lex "Mary")
	     (np-type proper)))
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
	     (np-type proper)))
     (medium ((lex "book")
	      (np-type common)
	      (definite no)
	      (classifier === "science")))
     (benef ((lex "Mary")
	     (np-type proper)))
     (verb ((process-class action)
	    (voice-class non-middle)
	    (dative-prep "to")
	    (lex "give"))))) 

  (def-test t4 
      "John likes Mary."
    ((cat clause)
     (process-type mental)
     (processor ((lex "John")
		 (np-type proper)))
     (phenomenon ((lex "Mary")
		  (np-type proper)))
     (verb ((process-class mental)
	    (voice-class non-middle)
	    (lex "like")))))

  (def-test t5 
      "John is the teacher."
    ((cat clause)
     (process-type equative)
     (identified ((lex "John")
		  (np-type proper)))
     (identifier ((lex "teacher")
		  (np-type common)))))

  (def-test t6 
      "The president is John."
    ((cat clause)
     (process-type equative)
     (focus {identifier})
     (identified ((lex "John")
		  (np-type proper)))
     (identifier ((lex "president")
		  (np-type common)))))

  (def-test t7 
      "Mary is thrown a heavy ball by John."
    ((cat clause)
     (process-type action)
     (agent ((lex "John")
	     (np-type proper)))
     (medium ((lex "ball")
	      (np-type common)
	      (definite no)
	      (describer === heavy)))
     (benef ((lex "Mary")
	     (np-type proper)))
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
	       (np-type common)
	       (determiner ((possessive yes)
			    (lex "John")
			    (np-type proper)))))
     (attribute === "beautiful")))


  (def-test t9 
      "The SIG-display marker moves."
    ((speech-act assertive)
     (tense present)
     (aspect ((perfective no) (progressive no)))
     (cat clause)
     (mood finite)
     (verb
      ((lex "move")
       (voice-class middle) 
       (transitive-class intransitive)))
     (process-type action) 
     (medium ((definite yes)
	      (np-type common)
	      (lex "sig-display marker")))))

	
  (def-test t10 
      "The SIG-display marker moves to the right."
    ((speech-act assertive)
     (tense present)
     (aspect ((perfective no) (progressive no)))
     (cat clause)
     (mood finite)
     (verb
      ((lex "move")
       (voice-class middle) 
       (transitive-class intransitive)))
     (process-type action)  
     (medium ((definite yes)
	      (np-type common)
	      (lex "sig-display marker")))
     (to-loc ((definite yes) (np-type common) (lex "right")))))

	
  (def-test t11 
      "The SIG-display marker moves from the left."
    ((speech-act assertive)
     (tense present)
     (aspect ((perfective no) (progressive no)))
     (cat clause)
     (mood finite)
     (verb
      ((lex "move")
       (voice-class middle) 
       (transitive-class intransitive)))
     (process-type action)  
     (medium ((definite yes)
	      (np-type common)
	      (lex "sig-display marker")))
     (from-loc ((definite yes) (np-type common) (lex "left")))))

	
  (def-test t12 
      "The SIG-display marker moves from the left to the right."  
    ((speech-act assertive)
     (tense present)
     (aspect ((perfective no) (progressive no)))
     (cat clause)
     (mood finite)
     (verb
      ((lex "move")
       (voice-class middle) 
       (transitive-class intransitive)))
     (process-type action)
     (medium ((definite yes)
	      (np-type common)
	      (lex "sig-display marker")))
     (to-loc ((definite yes) (np-type common) (lex "right")))
     (from-loc ((definite yes) (np-type common) (lex "left")))))


	
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
     (agent ((speech-act assertive)
	     (cat clause)
	     (mood non-finite)
	     (verb ((lex "increase")
		    (voice-class middle)
		    (transitive-class transitive)))
	     (process-type action)
	     (medium ((definite yes)
		      (np-type common)
		      (lex "power level")))))
     (medium ((speech-act assertive)
	      (cat clause)
	      (mood non-finite)
	      (verb ((lex "move")
		     (voice-class middle) 
		     (transitive-class intransitive)))
	      (process-type action)  
	      (medium ((definite yes)
		       (np-type common)
		       (lex "sig-display marker")))
	      (to-loc ((definite yes)
		       (np-type common)
		       (lex "right")))))))


  (def-test t14 
      "For her to do it."
    ((cat clause)
     (mood non-finite)
     (non-finite infinitive)
     (process-type action)
     (agent ((cat np) 
	     (np-type pronoun)
	     (pronoun-type personal)
	     (gender feminine)))
     (verb ((lex "do")
	    (voice-class non-middle)
	    (transitive-class transitive)))
     (medium ((cat np)
	      (np-type pronoun)
	      (pronoun-type personal)
	      (gender neuter)))))


  (def-test t15 
      "For her to do it is a bold statement."
    ((cat clause)
     (process-type attributive)
     (carrier ((cat clause)
	       (process-type action)
	       (agent ((cat np) 
		       (np-type pronoun)
		       (pronoun-type personal)
		       (gender feminine)))
	       (verb ((lex "do")
		      (voice-class non-middle)
		      (transitive-class transitive)))
	       (medium ((cat np)
			(np-type pronoun)
			(pronoun-type personal)
			(gender neuter)))))
     (attribute ((cat np)
		 (np-type common)
		 (definite no)
		 (lex "statement")
		 (describer === bold)))))


  (def-test t16 
      "The man whom I know."
    ((cat np)
     (np-type common)
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
		 (processor ((cat np)
			     (np-type pronoun)
			     (pronoun-type personal)
			     (person first)))))))


  (def-test t17 
      "The man that I know."
    ((cat np)
     (np-type common)
     (head === man)
     (definite yes)
     (qualifier ((cat clause)
		 (restrictive yes) ;; ***** here is the difference
		 (scope ((role phenomenon)))
		 (process-type mental)
		 (verb ((lex "know")
			(voice-class non-middle)
			(transitive-class transitive)))
		 (processor ((cat np)
			     (np-type pronoun)
			     (pronoun-type personal)
			     (person first)))))))


  (def-test t18 
      "The man that knows me."
    ((cat np)
     (np-type common)
     (head === man)
     (definite yes)
     (qualifier ((cat clause)
		 (restrictive yes)
		 (scope ((role processor)))
		 (process-type mental)
		 (verb ((lex "know")
			(voice-class non-middle)
			(transitive-class transitive)))
		 (phenomenon ((cat np)
			      (np-type pronoun)
			      (pronoun-type personal)
			      (person first)))))))


  (def-test t19 
      "The position to which the marker moves."
    ((cat np)
     (np-type common)
     (head === position)
     (definite yes)
     (qualifier ((cat clause)
		 (restrictive yes)
		 (scope ((role to-loc)))
		 (process-type action)
		 (verb ((lex "move")
			(voice-class middle)
			(transitive-class intransitive)))
		 (medium ((cat np)
			  (np-type common)
			  (lex "marker")
			  (definite yes)))
		 (to-loc ((definite yes)
			  (np-type common)
			  (lex "right")))))))


  (def-test t20 
      "The position of the marker."
    ((cat np)
     (np-type common)
     (head === position)
     (definite yes)
     (qualifier ((cat pp)
		 (restrictive yes)
		 (np-type common)
		 (head === marker)
		 (definite yes)))))


  (def-test t21 
      "My wife, Carol."
    ((cat np)
     (np-type common)
     (head === wife)
     (determiner ((possessive yes)
		  (np-type pronoun)
		  (pronoun-type personal)
		  (person first)))
     (qualifier ((cat np)
		 (restrictive no)
		 (np-type proper)
		 (head === "Carol")))))


  (def-test t22 
      "My brother Steve."
    ((cat np)
     (np-type common)
     (head === brother)
     (determiner ((possessive yes)
		  (np-type pronoun)
		  (pronoun-type personal)
		  (person first)))
     (qualifier ((cat np)
		 (restrictive yes) ;; **** Here is the difference
		 (np-type proper)
		 (head === "Steve")))))


  (def-test t23 
      "An intelligent person."
    ((cat np)
     (np-type common)
     (head === person)
     (determiner ((definite no)))
     (describer ((cat adj)
		 (lex "intelligent")))))


  (def-test t24 
      "My brother Steve and my wife, Carol."
    ((cat list)
     (common ((part-of-speech np)))
     (distinct
      ~(((np-type common)
	 (head === brother)
	 (determiner ((possessive yes)
		      (np-type pronoun)
		      (pronoun-type personal)
		      (person first)))
	 (qualifier ((cat np)
		     (restrictive yes)
		     (np-type proper)
		     (head === "Steve"))))
	((np-type common)
	 (head === wife)
	 (determiner ((possessive yes)
		      (np-type pronoun)
		      (pronoun-type personal)
		      (person first)))
	 (qualifier ((cat np)
		     (restrictive no)
		     (np-type proper)
		     (head === "Carol"))))))))


  (def-test t25 
      "My wife, Carol, my brother Steve and John."
    ((cat list)
     (common ((part-of-speech np)))
     (distinct
      ~(((np-type common)
	 (head === wife)
	 (determiner ((possessive yes)
		      (np-type pronoun)
		      (pronoun-type personal)
		      (person first)))
	 (qualifier ((cat np)
		     (restrictive no)
		     (np-type proper)
		     (head === "Carol"))))
	((np-type common)
	 (head === brother)
	 (determiner ((possessive yes)
		      (np-type pronoun)
		      (pronoun-type personal)
		      (person first)))
	 (qualifier ((cat np)
		     (restrictive yes)
		     (np-type proper)
		     (head === "Steve"))))
	((np-type proper)
	 (lex "John"))))))


  (def-test t26 
      "Take the hammer and hit the nail."
    ((cat list)
     (common ((part-of-speech clause)
	      (mood non-finite)
	      (non-finite imperative)))
     (distinct
      ~(((process-type action)
	 (verb ((lex "take")
		(voice-class non-middle)
		(transitive-class transitive)))
	 (medium ((cat np)
		  (np-type common)
		  (head === hammer)
		  (definite yes))))
	((process-type action)
	 (verb ((lex "hit")
		(voice-class non-middle)
		(transitive-class transitive)))
	 (medium ((cat np)
		  (np-type common)
		  (head === nail)
		  (definite yes))))))))

		    
  (def-test t27 
      "In order to install the battery, clean the box."
    ((cat clause)
     (mood non-finite)
     (non-finite imperative)
     (process-type action)
     (verb ((lex "clean")
	    (voice-class non-middle)
	    (transitive-class transitive)))
     (medium ((cat np)
	      (np-type common)
	      (head === "box")
	      (definite yes)))
     (purpose ((process-type action)
	       (verb ((lex "install")
		      (voice-class non-middle)
		      (transitive-class transitive)))
	       (medium ((cat np)
			(np-type common)
			(head === "battery")
			(definite yes)))))))
		      



  (def-test t28 
      "For her to do it must be a bold statement."
    ((cat clause)
     (process-type attributive)
     (epistemic-modality inference)
     (carrier ((cat clause)
	       (process-type action)
	       (agent ((cat np) 
		       (np-type pronoun)
		       (pronoun-type personal)
		       (gender feminine)))
	       (verb ((lex "do")
		      (voice-class non-middle)
		      (transitive-class transitive)))
	       (medium ((cat np)
			(np-type pronoun)
			(pronoun-type personal)
			(gender neuter)))))
     (attribute ((cat np)
		 (np-type common)
		 (definite no)
		 (lex "statement")
		 (describer === bold)))))



  (def-test t29 
      "The man whom I may know."
    ((cat np)
     (np-type common)
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
		 (processor ((cat np)
			     (np-type pronoun)
			     (pronoun-type personal)
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
	      (np-type common)
	      (lex "sig-display marker")))
     (to-loc ((definite yes) (np-type common) (lex "right")))))


  (def-test t31 
      "First, take the hammer and then, hit the nail."
    ((cat list)
     (common ((part-of-speech clause)
	      (mood non-finite)
	      (non-finite imperative)))
     (distinct
      ~(((process-type action)
	 (time-relater === first)
	 (verb ((lex "take")
		(voice-class non-middle)
		(transitive-class transitive)))
	 (medium ((cat np)
		  (np-type common)
		  (head === hammer)
		  (definite yes))))
	((process-type action)
	 (time-relater === then)
	 (verb ((lex "hit")
		(voice-class non-middle)
		(transitive-class transitive)))
	 (medium ((cat np)
		  (np-type common)
		  (head === nail)
		  (definite yes))))))))


  (def-test t32 
      ;; t32 fails because epistemic-modality is not compatible with imperative.
      "<fail>"
    ((cat list)
     (common ((part-of-speech clause)
	      (mood non-finite)
	      (non-finite imperative)))
     (distinct
      ~(((process-type action)
	 (epistemic-modality possible)
	 (verb ((lex "take")
		(voice-class non-middle)
		(transitive-class transitive)))
	 (medium ((cat np)
		  (np-type common)
		  (head === hammer)
		  (definite yes))))
	((process-type action)
	 (verb ((lex "hit")
		(voice-class non-middle)
		(transitive-class transitive)))
	 (medium ((cat np)
		  (np-type common)
		  (head === nail)
		  (definite yes))))))))

		    

  (def-test t33 
      "Hit the nail with the hammer."
    ((process-type action)
     (cat clause)
     (mood non-finite)
     (non-finite imperative)
     (instrument ((cat np)
		  (np-type common)
		  (head === hammer)
		  (definite yes)))
     (verb ((lex "hit")
	    (voice-class non-middle)
	    (transitive-class transitive)))
     (medium ((cat np)
	      (np-type common)
	      (head === nail)
	      (definite yes)))))


  (def-test t34 
      "Hit the nail using a hammer."
    ((process-type action)
     (cat clause)
     (mood non-finite)
     (non-finite imperative)
     (instrument ((cat np)
		  (np-type common)
		  (head === hammer)
		  (definite no)))
     (verb ((lex "hit")
	    (instrument-prep "using")
	    (voice-class non-middle)
	    (transitive-class transitive)))
     (medium ((cat np)
	      (np-type common)
	      (head === nail)
	      (definite yes)))))


  (def-test t35 
      "Take the nail off using a screwdriver."
    ((process-type action)
     (cat clause)
     (mood non-finite)
     (non-finite imperative)
     (instrument ((cat np)
		  (np-type common)
		  (head === screwdriver)
		  (definite no)))
     (verb ((lex "take")
	    (particle "off")
	    (instrument-prep "using")
	    (voice-class non-middle)
	    (transitive-class transitive)))
     (medium ((cat np)
	      (np-type common)
	      (head === nail)
	      (definite yes)))))


  (def-test t36 
      "The nail is taken off using a screwdriver."
    ((process-type action)
     (cat clause)
     (focus {medium})
     (instrument ((cat np)
		  (np-type common)
		  (head === screwdriver)
		  (definite no)))
     (verb ((lex "take")
	    (particle "off")
	    (instrument-prep "using")
	    (voice-class non-middle)
	    (transitive-class transitive)))
     (medium ((cat np)
	      (np-type common)
	      (head === nail)
	      (definite yes)))))


  (def-test t37 
      ;; another way of specifying the prep for the role instrument.
      "Take the nail off using a screwdriver."
    ((process-type action)
     (cat clause)
     (mood non-finite)
     (non-finite imperative)
     (instrument ((cat np)
		  (prep "using")
		  (np-type common)
		  (head === screwdriver)
		  (definite no)))
     (verb ((lex "take")
	    (particle "off")
	    (voice-class non-middle)
	    (transitive-class transitive)))
     (medium ((cat np)
	      (np-type common)
	      (head === nail)
	      (definite yes)))))

  (format t "~%ir4 installed. 37 tests.~%")
  (values))

