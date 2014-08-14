;;; -*- Mode:Lisp; Syntax:Common-Lisp; -*-
;;; -----------------------------------------------------------------------
;;; File:         ir7.l
;;; Description:  Examples for ir7 testing bk-class with manner
;;; Author:       Michael Elhadad
;;; Created:      21 Feb 1991
;;; Modified:     25 Feb 91 - added ao examples
;;; Package:      FUG5
;;; -----------------------------------------------------------------------

(in-package "FUG5")

(defun isetup7 ()
  (clear-tests)

  (def-test t1
      ("The Denver Nuggets edged the Celtics."
       "The Denver Nuggets nipped the Celtics.")
    ((cat clause)
     (process-type action)
     (process ((concept game-result)
	       (transitive-class transitive)
	       (voice-class non-middle)))
     (agent ((cat proper)
	     (lex "The Denver Nugget")
	     (number plural)))
     (medium ((cat proper)
	      (lex "the Celtic")
	      (number plural)))
     (tense past)
     (manner ((concept narrow)
	      (lex "narrowly")))))


  (store-verbs '(("beat" "beats" "beat" "beating" "beat")))
  (def-test t2
      "The Denver Nuggets narrowly beat the Celtics."
    ((cat clause)
     (process-type action)
     (process ((concept game-result)
	       (transitive-class transitive)
	       (voice-class non-middle)
	       (lex "beat")))
     (agent ((cat proper)
	     (lex "The Denver Nugget")
	     (number plural)))
     (medium ((cat proper)
	      (lex "the Celtic")
	      (number plural)))
     (tense past)
     (manner ((concept narrow)
	      (lex "narrowly")))))


  (def-test t4
      "The Denver Nuggets surprisingly beat the Celtics."
    ((cat clause)
     (process-type action)
     (process ((concept game-result)
	       (transitive-class transitive)
	       (voice-class non-middle)
	       (lex "beat")))
     (agent ((cat proper)
	     (lex "The Denver Nugget")
	     (number plural)))
     (medium ((cat proper)
	      (lex "the Celtic")
	      (number plural)))
     (tense past)
     (ao ((concept rating)
	  (carrier {agent})
	  (orientation -)
	  (lex "surprisingly")))))


  (def-test t5
      ("The Denver Nuggets stunned the Celtics."
       "The Denver Nuggets surprised the Celtics.")
    ((cat clause)
     (process-type action)
     (process ((concept game-result)))
     (agent ((cat proper)
	     (lex "The Denver Nugget")
	     (number plural)))
     (medium ((cat proper)
	      (lex "the Celtic")
	      (number plural)))
     (tense past)
     (ao ((concept rating)
	  (carrier {agent})
	  (orientation -)
	  (lex "surprisingly")))))


  ;; Now try them together!!! ao AND manner...
  (def-test t6
      ("The Denver Nuggets surprisingly nipped the Celtics."
       "The Denver Nuggets surprisingly edged the Celtics.")
    ((cat clause)
     (process-type action)
     (process ((concept game-result)))
     (agent ((cat proper)
	     (lex "The Denver Nugget")
	     (number plural)))
     (medium ((cat proper)
	      (lex "the Celtic")
	      (number plural)))
     (tense past)
     (ao ((concept rating)
	  (carrier {agent})
	  (orientation -)
	  (lex "surprisingly")))
     (manner ((concept narrow)
	      (lex "narrowly")))))

  (format t "~%ir7 installed. 7 tests. Results are non-deterministic.~%")
  (values))

