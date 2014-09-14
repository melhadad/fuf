;;; -*- Mode:Lisp; Syntax:Common-Lisp; Package: FUG5 -*-
;;; -----------------------------------------------------------------------
;;; File:         np-tests.lisp
;;; Description:  Inputs for noun phrases as requested by Henschel Renate
;;; Author:       Yael Dahan
;;; Created:      June 99
;;; Modified:     Sep 14
;;; Package:      FUG5
;;; -----------------------------------------------------------------------

(in-package "FUG5")

;; The following tests are incorrect: (NP-DET-10-ADV
;;                                     NP-DET-10-DISTRIBUTIVE
;;                                     NP-DET-18A)

;; @TODO:
;; - Adv: practically every boy np-det-10-adv
;; - Distributive / collective: every boy / all the boys np-det-10-distributive
;; - Partitive: np-det-18a
;;
;;; -  implementation of partitives.
;;; -  generic nouns (a feature?)
;;; -  such
;;; -  propers with 'the' - "the Britannia" "the United States"
;;; -  no semantic modification yet (time location..)


;;; For any question, please contact me.
;;; Yael Dahan Netzer yaeln@cs.bgu.ac.il

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;               List of NP types
;;;
;;; Author: Renate Henschel
;;;
;;; Created: 03.06.99
;;;
;;; This list contains examples of NPs for nearly all types of NPs
;;; mentioned in Quirk et.al. "A Grammar of Contemporary English".
;;; The examples have been taken from the SUSANNE corpus, the MUSE
;;; corpus, and Quirk et.al. For the PP modification the head noun
;;; has been derived from a verb in cases where no equivalent NP
;;; was to be found in the corpora.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;=============================================
;;; Determination
;;;=============================================

(def-test np-det-01
  "A ship."
  ((cat np)
   (lex "ship")
   (definite no)))

 (def-test np-det-02
  "The ship."
  ((cat np)
   (lex "ship")))

(def-test np-det-03
  "Ships."
  ((cat np)
   (number plural)
   (definite no)
   (lex "ship")))

(def-test np-det-04
  "The ships."
  ((cat np)
   (number plural)
   (lex "ship")))

;;; a ship (generic usage)
(def-test np-det-01a
  "A ship."
  ((cat np)
   (definite no)
   (lex "ship")))

;;; the ship (generic usage)
(def-test np-det-02a
  "The ship."
  ((cat np)
   (lex "ship")))

;;; ships (generic usage)
(def-test np-det-03a
  "Ships."
  ((cat np)
   (number plural)
   (definite no)
   (lex "ship")))

;;; the ships (generic usage)
(def-test np-det-04a
  "The ships."
  ((cat np)
   (number plural)
   (lex "ship")))

;;; this ship (these ships)
(def-test np-det-05
  "This ship."
  ((cat np)
   (distance near)
   (lex "ship")))

;;; that ship (those ships)
(def-test np-det-06
  "That ship."
  ((cat np)
   (distance far)
   (lex "ship")))

;;; no ship (no ships)
(def-test np-det-07
  "No ship."
  ((cat np)
   (total -)
   (lex "ship")))

;;; some stressed
(def-test np-det-08
  "Some ship."
  ((cat np)
   (definite no)
   (selective yes)
   (reference-number plural)
   (reference-polarity +)
   (lex "ship")))

;;; any stressed
(def-test np-det-09
  "Any ship."
  ((cat np)
   (selective yes)
   (definite no)
   (reference-type non-specific)
   (reference-number plural)
   (reference-polarity -)
   (lex "ship")))

;; Distinction each/every
;; distributive/collective
;; adv -> every (almost every ...)
;; reference-number dual -> each

(def-test np-det-10
  ("Every ship." "Each ship.")
  ((cat np)
   (total +)
   (number singular)
   (lex "ship")))

;; @todo
#+ignore(def-test np-det-10-adv
  "Practically every ship."
  ((cat np)
   (determiner ((total +)
                (adverb ((lex "practically")))))
   (number singular)
   (lex "ship")))

(def-test np-det-10-dual
  "Each ship."
  ((cat np)
   (total +)
   (number singular)
   (reference-number dual)
   (lex "ship")))

(def-test np-det-10-plural
  ("Each ship." "Every ship.")
  ((cat np)
   (total +)
   (number singular)
   (reference-number plural)
   (lex "ship")))

;; @todo
#+ignore(def-test np-det-10-distributive
  "Each ship."
  ((cat np)
   (total +)
   (number singular)
   (reference-type distributive)
   (lex "ship")))


;;;np-det-11a: each of the ships
;  "Each of the ships."



;;; either ship (either of the ships)
(def-test np-det-12
  "Either ship."
  ((cat np)
   (total none)
   (reference-number dual)
   (selective yes)
   (lex "ship")))

;;; neither ship (neither of the ships)
(def-test np-det-13
  "Neither ship."
  ((cat np)
   (lex "ship")
   (reference-number dual)
   (total -)))

;;; which ship (which ships)
(def-test np-det-14
  "Which ship."
  ((cat np)
   (interrogative yes)
   (lex "ship")))

;;; what ship (what ships)
(def-test np-det-15
  "What ship."
  ((cat np)
   (interrogative yes)
   (selective no)
   (lex "ship")))

;;; whose ship (whose ships)
(def-test np-det-16
  "Whose ship."
  ((cat np)
   (interrogative yes)
   (possessive yes)
   (lex "ship")))

;;; my ship (my ships)
(def-test np-det-17
  "My ship."
  ((cat np)
   (possessor ((cat personal-pronoun)
	       (person first)))
   (lex "ship")))

;;; some ships (some of the ships) (unstressed)
(def-test np-det-18
  "Some ships."
  ((cat common)
   (lex "ship")
   (number plural)
   (definite no)
   (reference-number plural)
   (selective yes)))

;;; @TODO partitive
;;; some of the ships (unstressed)
#+ignore(def-test np-det-18a
  "Some of the ships."
  ((cat common)
   (lex "ship")
   (orientation +)
   (number plural)
   (definite no)
   (reference-number plural)
   (selective yes)
   (partitive yes)))

;; any ships (any of the ships) (unstressed)
(def-test np-det-19
  "Any ships."
  ((cat common)
   (lex "ship")
   (number plural)
   (total none)
   (reference-polarity -)
   (selective yes)))

(def-test np-det-20
  "Enough ships."
  ((cat common)
   (lex "ship")
   (definite no)
   (number plural)
   (evaluative yes)
   (evaluation +)
   (orientation +)))

;;;;;; mass nouns

(def-test np-det-21
  "Fuel."
  ((cat np)
   (lex "fuel")
   (definite no)
   (countable no)))

(def-test np-det-22
  "The fuel."
  ((cat np)
   (lex "fuel")
   (definite yes)
   (countable no)))

(def-test np-det-23
  "This fuel."
  ((cat np)
   (lex "fuel")
   (distance near)
   (countable no)))

(def-test np-det-24
  "That fuel."
  ((cat np)
   (lex "fuel")
   (distance far)
   (countable no)))

(def-test np-det-25
  "No fuel."
  ((cat np)
   (lex "fuel")
   (total -)
   (countable no)))

(def-test np-det-26
  "Much fuel."
  ((cat np)
   (lex "fuel")
   (orientation +)
   (degree +)
   (partitive no)
   (countable no)))

(def-test np-det-26p
  ("Plenty of fuel."
   "A good deal of fuel."
   "A great deal of fuel."
   "Lots of fuel."
   "A lot of fuel.")
  ((cat np)
   (lex "fuel")
   (orientation +)
   (degree +)
   (partitive yes)
   (countable no)))

(def-test np-det-27
  "Which fuel."
  ((cat np)
   (lex "fuel")
   (interrogative yes)
   (countable no)))

(def-test np-det-28
  "What fuel."
  ((cat np)
   (lex "fuel")
   (interrogative yes)
   (selective no)
   (countable no)))

(def-test np-det-29
  "Whose fuel."
  ((cat np)
   (lex "fuel")
   (possessive yes)
   (interrogative yes)
   (countable no)))

(def-test np-det-30
  "My fuel."
  ((cat np)
   (lex "fuel")
   (possessor ((cat personal-pronoun)
	       (person first)))
   (countable no)))

(def-test np-det-31
  "Some fuel."
  ((cat common)
   (countable no)
   (lex "fuel")
   (definite no)
   (orientation +)))


;; This is a problem: either definite no/ partitive no
;; or gets to be open-class "a great/good/large number of fuel"
(def-test np-det-31a
  "Some of the fuel."
  ((cat common)
   (countable no)
   (partitive yes)
   (definite yes)
   (selective yes)
   (orientation +)
   (lex "fuel")))

;; @todo
;;;np-det-32: any fuel

;;;np-det-33: enough fuel (enough of the fuel)
(def-test np-det-33
  "Enough fuel."
  ((cat common)
   (lex "fuel")
   (definite no)
   (countable no)
   (evaluative yes)
   (evaluation +)
   (orientation +)))

;;;***No special feature for generic
;;;np-det-21a: fuel (generic usage)
(def-test np-det-21a
  "Fuel."
  ((cat np)
   (lex "fuel")
   (definite no)
   (countable no)))

;;;np-det-22a: the fuel (generic usage)
(def-test np-det-22a
  "Fuel."
  ((cat np)
   (lex "fuel")
   (definite no)
   (countable no)))

;;;=============================================
;;; Predetermination
;;;=============================================

(def-test np-det-40
  "All ships."
  ((cat common)
   (number plural)
   (definite no)
   (total +)
   (lex "ship")))

(def-test np-det-41
  "Both ships."
  ((cat common)
   (number dual)
   (definite no)
   (total +)
   (lex "ship")))

(def-test np-det-42
 "Half fuel."
  ((cat common)
   (countable no)
   (partitive no)
   (fraction ((den 2) (num 1)))
   (definite no)
   (lex "fuel")))

;; ***partitives

(def-test np-det-40a
  "All of the ships."
  ((cat common)
   (number plural)
   (definite yes)
   (partitive yes)
   (total +)
   (lex "ship")))

(def-test np-det-41a
  "Both of the ships."
  ((cat common)
   (number dual)
   (definite yes)
   (partitive yes)
   (total +)
   (lex "ship")))

(def-test np-det-42a
  "Half of the fuel."
  ((cat common)
   (countable no)
   (partitive yes)
   (definite yes)
   (fraction ((den 2) (num 1)))
   (lex "fuel")))

(def-test np-such-1
  "Such ships."
  ((cat common)
   (number plural)
   (definite no)
   (status "such")
   (lex "ship")))


;;;=============================================
;;; Ordinals
;;;=============================================

(def-test np-ord-1
  ;; Issue in linearization of "an other"
  ("Another ship." "An other ship.")
  ((cat np)
   (lex "ship")
   (definite no)
   (countable yes)
   (ordinal ((value <>)))))

(def-test np-ord-2
  "The other ship."
  ((cat np)
   (lex "ship")
   (ordinal ((value <>)))))

(def-test np-ord-3
  ("The next ship."
   "The following ship."
   "The subsequent ship.")
  ((cat np)
   (lex "ship")
   (ordinal ((value +)))))

(def-test np-ord-4
  "The first ship."
  ((cat np)
   (lex "ship")
   (ordinal ((value 1)))))


;;;=============================================
;;; Cardinals and Quantification
;;;=============================================

;;;np-card-1: one ship
(def-test np-card-1
  "One ship."
  ((cat common)
   (lex "ship")
   (total none)
   (selective yes)
   (number singular)))

(def-test np-card-1b
  "One ship."
  ((cat common)
   (lex "ship")
   (cardinal ((value 1)))
   (definite no)))

;;;np-card-1a: one of the ships
#+ignore(def-test np-card-1a
  "One of the ships."
  ((cat common)
   (cardinal ((value 1))) ;; *which of the two?
   ;(selective yes)
   (ref-set ((lex "ship")
	     (number plural)))))


(def-test np-card-2
  "Two ships."
  ((cat common)
   (lex "ship")
   (cardinal ((value 2)))
   (definite no)))

#+ignore(def-test np-card-2a
  "Two of the ships."
  ((cat common)
   (cardinal ((value 2)))
   (ref-set ((lex "ship")
	     (definite yes)))))

(def-test np-q-1
  ("Many ships."
   "A lot of ships."
   "Lots of ships."
   "A great many ships."
   "Plenty of ships."
   "A good many ships.")
  ((cat common)
   (lex "ship")
   (definite no)
   (number plural)
   (degree +)
   (orientation +)))

(def-test np-q-2
  "More ships."
  ((cat common)
   (lex "ship")
   (definite no)
   (number plural)
   (comparative yes)
   (orientation +)))

(def-test np-q-3
  "Most ships."
  ((cat common)
   (lex "ship")
   (definite no)
   (number plural)
   (superlative yes)
   (orientation +)))

(def-test np-q-4
  "Few ships."
  ((cat common)
   (lex "ship")
   (definite no)
   (number plural)
   (orientation -)))

(def-test np-q-4a
  "Fewer ships."
  ((cat common)
   (lex "ship")
   (definite no)
   (number plural)
   (comparative yes)
   (orientation -)))

(def-test np-q-4b
  "The fewest ships."
  ((cat common)
   (lex "ship")
   (definite yes)
   (number plural)
   (partitive no)
   (superlative yes)
   (orientation -)))

(def-test np-q-5
   ("Several ships."
     "A good many ships."
     "A lot of ships."
     "Lots of ships."
     "A great many ships."
     "Plenty of ships.")
  ((cat common)
   (lex "ship")
   (definite no)
   (number plural)
   (degree +)
   (orientation +)))

;;;np-q-6: little fuel
(def-test np-q-6
  "A little fuel."
  ((cat common)
   (countable no)
   (lex "fuel")
   (definite no)
   (degree -)
   (orientation +)))

(def-test np-q-7
  "More fuel."
  ((cat common)
   (lex "fuel")
   (definite no)
   (countable no)
   (comparative yes)
   (orientation +)))

(def-test np-q-7b
  "Less fuel."
  ((cat common)
   (lex "fuel")
   (definite no)
   (countable no)
   (comparative yes)
   (orientation -)))

(def-test np-q-8
  "Most fuel."
  ((cat common)
   (lex "fuel")
   (definite no)
   (countable no)
   (superlative yes)
   (orientation +)))

(def-test np-q-8a
  "Least fuel."
  ((cat common)
   (lex "fuel")
   (definite no)
   (countable no)
   (superlative yes)
   (orientation -)))


;;;=============================================
;;;  Proper names
;;;=============================================

;;;np-name-1: Martin Carlin
(def-test np-name-1
  "Martin Carlin."
  ((cat proper)
   (lex "Martin Carlin")))

(def-test np-name-1a
  "Martin Carlin."
  ((cat proper)
   (head ((cat person-name)
	  (first-name ((lex "Martin")))
	  (last-name ((lex "Carlin")))))))

;;; the Britannia (determiner required)
(def-test np-name-2
  "The Britannia."
  ((cat proper)
   (denotation article-thing)
   (lex "Britannia")))


;;;====================================================
;;;  Pronominalization
;;;====================================================

(def-test np-pro-01
  "I."
  ((cat personal-pronoun)
   (person first)
   (number singular)))

(def-test np-pro-02
  "You."
  ((cat personal-pronoun)
   (person second)
   (number singular)))

(def-test np-pro-03
  "He."
  ((cat personal-pronoun)
   (gender masculine)
   (person third)
   (number singular)))

(def-test np-pro-03a
  "She."
  ((cat personal-pronoun)
   (gender feminine)
   (person third)
   (number singular)))

(def-test np-pro-03b
  "I."
  ((cat personal-pronoun)
   (person first)
   (number singular)))

(def-test np-pro-04
  "We."
  ((cat personal-pronoun)
   (person first)
   (number plural)))

(def-test np-pro-05
  "They."
  ((cat personal-pronoun)
   (person third)
   (number plural)))

(def-test np-pro-06
  "Who."
  ((cat question-pronoun)
   (animate yes)))

(def-test np-pro-07
  "Which."
  ((cat question-pronoun)
   (restrictive yes)))


(def-test np-pro-08
  "Ourselves."
  ((cat personal-pronoun)
   (person first)
   (case reflexive)
   (number plural)))


;;;np-pro-09: each other
;;***this can be done with binding (in a context)
#+ignore(def-test b7 ;; try reciprocal
  "They met each other"
  ((cat clause)
   (tense past)
   (proc ((type material) (lex "meet")))
   (partic ((agent ((cat personal-pronoun) (number plural)))
            (affected ((index {^2 agent index}) (reflexive-type reciprocal)))))))


;;;np-pro-10: one another

(def-test np-pro-11
  "This."
  ((cat demonstrative-pronoun)
   (distance near)))

(def-test np-pro-11a
  "These."
  ((cat demonstrative-pronoun)
   (number plural)
   (distance near)))

(def-test np-pro-12
  "That."
  ((cat demonstrative-pronoun)
   (distance far)))

;;;======================================================
;;;  Predicative NPs
;;;======================================================

;;;np-pred-1: The Britannia is a ship.
(def-test np-pred-1
  "The Britannia is a ship."
  ((cat clause)
   (proc ((type ascriptive)
	  (mode equative)))
   (partic ((identifier ((cat np)
			 (lex "ship")
			 (definite no)))
	    (identified ((cat proper)
			 (denotation article-thing)
			 (lex "Britannia")))))))


;;;np-pred-2: Jacques Delors is the EC president.
(def-test np-pred-2
  "Jacques Delors is the EC president."
  ((cat clause)
   (proc ((type ascriptive)
	  (mode equative)))
   (partic ((identifier ((cat common)
			 (lex "president")
			 (classifier ((lex "EC")))))
	    (identified ((cat proper)
			 (lex "Jacques Delors")))))))




;;;====================================================
;;;  Partitives and measure phrases
;;;====================================================

;; ***partitive or measure?
;;;np-of-2: a glass of water (quantity)
(def-test np-of-2
  "A glass of water."
  ((cat partitive)
   (part ((cat np)
	  (lex "glass")
	  (definite no)))
   (part-of ((cat common)
	     (countable no)
	     (lex "water")
	     (definite no)))))

;;;***not good
;;;(def-test np-of-2a
;;;  "A glass of water."
;;;  ((cat measure)
;;;   (quantity ((cat np)
;;;	      (lex "glass")))
;;;   (unit ((

;;;Np-meas-1: 4mg of oestradiol

;;;np-meas-2: 75 micrograms oestradiol

;;;====================================================
;;;  Generalized Possession
;;;====================================================

(def-test np-of-1
  "The funnel of the ship."
  ((cat common)
   (lex "funnel")
   (qualifier ((np ((lex "ship")))))))

(def-test np-of-2a
  "Onassi's ship."
  ((cat common)
   (lex "ship")
   (possessor ((cat proper)
	       (lex "Onassi")))))

;;; ***special treatment of nominalization
(def-test np-of-3
  "The arrival of the train."
  ((cat common)
   (lex "arrival")
   (qualifier ((np ((lex "train")))))))

;;;np-of-4: an opera of Verdi's
(def-test np-of-4
  "An opera of Verdi's."
  ((cat common)
   (lex "opera")
   (definite no)
   (qualifier ((np ((cat common)
		    (head ((gap yes)))
		    (possessor ((cat proper)
				(lex "Verdi")))))))))


;;;np-of-5: the City of Rome
(def-test np-of-5
  "The City of Rome."
  ((cat common)
   (lex "City")
   (qualifier ((np ((cat proper) (lex "Rome")))))))

;; ***nominalization
;;;np-of-6: the imprisonment of the murderer
(def-test np-of-6
  "The imprisonment of the murderer."
  ((cat common)
   (lex "imprisonment")
   (qualifier ((np ((cat common) (lex "murderer")))))))


;;;======================================================
;;;  Premodification
;;;======================================================

(def-test np-premod-1
  "A small green carved Chinese jade idol."
  ((cat common)
   (definite no)
   (lex "idol")
   (classifier ((lex "jade")))
   (describer ((cat list)
	       (distinct ~(((cat adj) (lex "small"))
			   ((cat adj) (lex "green"))
			   ((cat adj) (lex "carved"))
			   ((cat adj) (lex "Chinese"))))))))

(def-test np-premod-2
  "Half my first new salary."
  ((cat common)
   (lex "salary")
   (partitive no)
   (describer ((lex "new")))
   (ordinal ((value 1)))
   (possessor ((cat personal-pronoun)
	       (person first)))
   (fraction ((den 2) (num 1)))))

(def-test np-premod-3a
  "These last two days."
  ((cat common)
   (lex "day")
   (number dual)
   (partitive no)
   (distance near)
   (cardinal ((value 2)))
   (ordinal ((value last)))))

(def-test np-premod-3
  "Both these last two days."
  ((cat common)
   (lex "day")
   (number dual)
   (partitive no)
   (distance near)
   (total +)
   (cardinal ((value 2)))
   (ordinal ((value last)))))


;; ***this for partitive ref-set etc

(def-test np-premod-4
  "All the little Danish butter."
  ((cat common)
   (lex "butter")
   (countable no)
   (describer ((cat list)
	       (distinct ~(((cat adj)
			    (lex "little"))
			   ((cat adj)
			    (lex "Danish"))))))
   (total +)
   (definite yes)))
   ;(orientation +)
   ;(degree -)))


;;;======================================================
;;;  Postmodifying PPs
;;;======================================================
;;; position

;;;np-space-01: the medallion in the cabinet
(def-test np-space-01
  "The medallion in the cabinet."
  ((cat common)
   (lex "medallion")
   (qualifier ((cat pp)
	       (prep ((lex  "in")))
	       (np ((lex "cabinet")))))))

;;;np-space-01a: the medallion inside the cabinet
(def-test np-space-01a
  "The medallion inside the cabinet."
  ((cat common)
   (lex "medallion")
   (qualifier ((cat pp)
	       (prep ((lex  "inside")))
	       (np ((lex "cabinet")))))))

;;;np-space-02: the decoration on this cabinet
(def-test np-space-02
  "The decoration on this cabinet."
  ((cat common)
   (lex "decoration")
   (qualifier ((cat pp)
	       (prep ((lex  "on")))
	       (np ((distance near)
		    (lex "cabinet")))))))

;;;np-space-03: the medallion at the cabinet
(def-test np-space-03
  "The medallion at the cabinet."
  ((cat common)
   (lex "medallion")
   (qualifier ((cat pp)
	       (prep ((lex  "at")))
	       (np ((lex "cabinet")))))))

;;;np-space-04: the events outside the United States
(def-test np-space-04
  "The events outside the United States."
  ((cat common)
   (lex "event")
   (number plural)
   (qualifier ((cat pp)
	       (prep ((lex "outside")))
	       (np ((cat address)
		    (country ((lex "the United States")))))))))


;;;np-space-04a: dollars out off circulation
;;;np-space-05: the time off the stage
;;;np-space-06: miles away from other farms

;;; destination

;;;np-space-07: access to the lower half
;;;np-space-08: the mounting of porcelain plaques  on furniture
;;;np-space-09: the journey into this area
;;;np-space-10: the discharge from the hospital
;;;np-space-11: the jump off the wing
;;;np-space-12: the step out of the car



;;;Relative position/destination

;;;np-space-13: a tree by the stream

;;;np-space-14: the drawer above the door
;;;np-space-15: bridges  over roads
;;;np-space-16: Pooh on top of Piglet
;;;np-space-17: the drawer below the door
;;;np-space-18: the Senat  under White House pressure
;;;np-space-19: smoke underneath the ceiling


;;;np-space-20: the medallion behind the doors
;;;np-space-21: a table in front of the cabinet


;;;np-space-22: the glass partition between me and the driver

;;;np-space-23: his train trip across the nation
;;;np-space-24: the gas flow through a plasma generator
;;;np-space-25: moving past Lafayette Square

;;;np-space-26: hundreds of pavilions up the river valley
;;;np-space-27: the village down the hill

;;;np-space-28: the pressure difference along the major axis

;;;np-space-29: peoples beyond territorial boundaries

;;;Time location
;;;np-time-01: his death in 1785
(def-test np-time-01
  "His death in 1785."
  ((cat np)
   (lex "death")
   (possessor ((cat pronoun)
	       (gender masculine)))
   (qualifier ((cat pp)
               (prep ((lex "in")))
	       (np ((cat date)
		    (year ((value "1785")))))))))

;;;np-time-02: a public hearing on Feb. 22
;;;np-time-03: a press conference at 4:30 pm

;;;Duration
;;;np-time-04: continual use for over a century

;;;np-time-05: a day before the interview
;;;np-time-06: a week after the election
;;;np-time-07: the situation since 1865
;;;np-time-08: the activities until summer
;;;np-time-09: the brief interval between 1930 and 1961

;;;np-time-10: the increase of costs by 1966
;;;np-time-11: hospital stays up to 90 days

;;;np-pp-01: contracts for repair works (purpose)
;;;np-pp-02: a small house for the King's mistress (recipient)
;;;np-pp-03: the table from the Trianon de Porcelain (source)

;;;np-pp-05: nerves like banjo strings (resemblance)

;;;np-pp-06: unemployment relief by nation-wide taxation (means)
;;;np-pp-07: policy with detailed application to  danger spots (means)
;;;np-pp-08: tax increase without consent (means)

;;;np-pp-09: the kick with his left foot (instrument)
;;;np-pp-10: a drawing by  Nicolas Pineau (agentive)

;;;np-pp-11: curry with rice (accompaniment)
;;;np-pp-12: opossition against your plan
;;;np-pp-13: support for your plan

;;;np-pp-14: coffers with secret compartments (having "with")
;;;np-pp-15: he without his wife (having "without")

;;;np-pp-16: a book on butterflies (subject matter)
;;;np-pp-17: a story about a princess (subject matter)

;;;np-pp-18: a cake with eggs (ingredients)
;;;np-pp-19: This table's marquetry of ivory and horn (material)



;;;=============================================
;;; Clausal postmodification
;;;=============================================

(def-test np-res-1
  "A medal to show the king of France."
  ((cat np)
   (lex "medal")
   (definite no)
   (qualifier ((cat clause)
	       (mood to-infinitive)
	       (scope {^ partic identified})
	       (proc ((type ascriptive)
		      (mode equative)
		      (lex "show")))
	       (partic ((identifier ((cat np)
				     (lex "king")
				     (qualifier ((cat pp)
						 (np ((cat proper)
						      (lex "France")))))))))))))

(def-test np-res-2
  "A medal that shows the king of France."
  ((cat np)
   (lex "medal")
   (definite no)
   (qualifier ((cat clause)
	       (restrictive yes)
	       (scope {^ partic identified})
	       (proc ((type ascriptive)
		      (mode equative)
		      (lex "show")))
	       (partic ((identified ((index {^4 index})))
			(identifier ((cat np)
				     (lex "king")
				     (qualifier ((np ((cat proper)
						      (lex "France")))))))))))))


(def-test np-res-3
  "A medal showing the king of France."
  ((cat np)
   (lex "medal")
   (definite no)
   (qualifier ((cat clause)
	       (mood present-participle)
	       (controlled {^ partic identified})
	       (proc ((type ascriptive)
		      (mode equative)
		      (lex "show")))
	       (partic ((identified ((index {^4 index})))
			(identifier ((cat np)
				     (lex "king")
				     (qualifier ((np ((cat proper)
						      (lex "France")))))))))))))

(store-verbs '(("strike" "strikes" "struck" "striking" "struck")))
(def-test np-res-4
  "A medal that was struck in 1661."
  ((cat np)
   (lex "medal")
   (definite no)
   (qualifier ((cat clause)
	       (restrictive yes)
	       (scope {^ partic affected})
	       (proc ((type material)
		      (lex "strike")
		      (voice passive)
		      (tense past)
		      (agentive no)))
	       (partic ((affected ((index {^4 index})))))
	       (circum ((time ((cat pp)
                               (prep ((lex "in")))
			       (np ((cat date)
				    (year ((value "1661")))))))))))))

(def-test np-res-5
  "A medal struck in 1661."
  ((cat np)
   (lex "medal")
   (definite no)
   (qualifier ((cat clause)
	       (mood past-participle)
               (controlled {^ partic affected})
	       (proc ((type material)
		      (lex "strike")
		      (voice passive)))
	       (partic ((affected ((index {^4 index})))))
	       (circum ((time ((cat pp)
			       (prep ((lex "in")))
			       (np ((cat date)
				    (year  ((value "1661")))))))))))))

;;;=============================================
;;; Coordination
;;;=============================================

(def-test np-coor-1
  "The ebeniste and his wife."
  ((cat np)
   (complex conjunction)
   (distinct ~(((lex "ebeniste"))
	       ((possessor ((cat pronoun) ;; should be with index
			    (person third)
			    (gender masculine)))
		(lex "wife"))))))

(def-test np-coor-1a
  "The ebeniste or his wife."
  ((cat np)
   (complex conjunction)
   (conjunction ((cat conj) (lex "or")))
   (distinct ~(((lex "ebeniste"))
	       ((possessor ((cat pronoun) ;; should be with index
			    (person third)
			    (gender masculine)))
		(lex "wife"))))))

(def-test np-coor-2
  "The pendulum and weights."
  ((cat np)
   (head ((cat noun)
	  (complex conjunction)
	  (distinct ~(((lex "pendulum"))
		      ((lex "weight")
		       (number plural))))))))

;;;=============================================
;;;  Apposition
;;;=============================================
;;; Class-ascription

(def-test np-app-01
  "The Trianon de Porcelaine, a small house."
  ((cat np)
   (complex apposition)
   (distinct ~(((cat proper)
		(denotation article-thing)
                (lex "Trianon de Porcelaine"))
	       ((cat np)
		(lex "house")
		(definite no)
		(describer ((lex "small"))))))))

(def-test np-app-02
  "A Polish general, Count Jan Klemens Branicki."
  ((cat np)
   (complex apposition)
   (distinct ~(((cat np)
		(definite no)
		(lex "general")
		(describer ((lex "Polish"))))
	       ((cat proper)
		(head ((cat person-name)
		       (title ((lex "Count")))
		       (first-name ((lex "Jan")))
		       (middle-name ((lex "Klemens")))
		       (last-name ((lex "Branicki"))))))))))


(def-test np-app-03a
  "Financial expert Tom Timber."
  ((cat np)
   (complex apposition)
   (restrictive yes)
   (distinct ~(((cat np)
		(lex "expert")
		(denotation zero-article-thing)
		(describer ((lex "financial"))))
	       ((cat proper)
		(head ((cat person-name)
		       (first-name ((lex "Tom")))
		       (last-name ((lex "Timber"))))))))))

(def-test np-app-03
  "Financial expert Tom Timber."
  ((cat person-name)
   (title ((lex "Financial expert")))
   (first-name ((lex "Tom")))
   (last-name ((lex "Timber")))))

(def-test np-app-04
  "The Czech painter Alphonse Mucha."
  ((cat np)
   (complex apposition)
   (restrictive yes)
   (distinct ~(((cat np)
		(lex "painter")
		(describer ((lex "Czech"))))
	       ((cat proper)
		(lex "Alphonse Mucha"))))))

(def-test np-app-05
  "My friend Peter."
  ((cat np)
   (complex apposition)
   (restrictive yes)
   (distinct
    ~(((cat common)
       (head === "friend")
       (possessor  ((cat personal-pronoun)
		    (person first))))
      ((cat proper)
       (head === "Peter"))))))

;;; Identity-assertion

(def-test np-app-06
  "The French king Louis XIV."
  ((cat np)
   (complex apposition)
   (restrictive yes)
   (distinct ~(((lex "king")
		(describer ((lex "French"))))
	       ((cat proper)
		(head ((cat person-name)
		       (first-name ((lex "Louis")))
		       (dynasty ((cat ordinal) (value 14))))))))))

(def-test np-app-07
  "The French king, Louis XIV."
  ((cat np)
   (complex apposition)
   (restrictive no)
   (distinct ~(((lex "king")
		(describer ((lex "French"))))
	       ((cat proper)
		(head ((cat person-name)
		       (first-name ((lex "Louis")))
		       (dynasty ((cat ordinal) (value 14))))))))))

(def-test np-app-08
  "Louis XIV, the French king."
  ((cat np)
   (complex apposition)
   (distinct ~(((cat proper)
		(head ((cat person-name)
		       (first-name ((lex "Louis")))
		       (dynasty ((cat ordinal) (value 14))))))
	       ((lex "king")
		(describer ((lex "French"))))
	       ))))

(def-test np-app-09
  "Mr. Campbell the lawyer."
  ((cat np)
   (complex apposition)
   (restrictive yes)
   (distinct ~(((cat proper)
		(head ((cat person-name)
		       (title ((lex "Mr.")))
		       (last-name ((lex "Campbell"))))))
	       ((cat np)
		(lex "lawyer"))))))

(store-plurals '(("shelf" "shelves")))

(def-test np-both-1
  "Both the corner cupboard and the objects displayed on its shelves."
  ((cat np)
   (complex apposition)
   (restrictive yes)
   (distinct
    ~(((cat pronoun)
       (lex "both")
       (number dual))
      ((complex conjunction)
       (cat common)
       (distinct
        ~(((lex "cupboard")
           (classifier ((lex "corner"))))
          ((lex "object")
           (number plural)
           (qualifier
            ((cat clause)
             (relative-marker ((gap yes)))
             (scope {^ partic located})
             (proc ((type locative)
                    (lex "display")
                    (tense past)))
             (partic ((location ((cat pp)
                                 (prep ((lex "on")))
                                 (np ((cat common)
                                      (possessor ((cat pronoun)))
                                      (lex "shelf")
                                      (number plural)))))))))))))))))

(def-test np-both-2
  "She and Turba, both."
  ((cat np)
   (complex apposition)
   (distinct ~(((complex conjunction)
		(distinct ~(((cat pronoun)
			     (gender feminine))
			    ((cat proper)
			     (lex "Turba")))))
	       ((cat pronoun)
		(lex "both"))))))


(def-test np-app-10
  "The possibility that the observed radiation may be a combination of thermal and non-thermal components."
  ((cat common)
   (lex "possibility")
   (qualifier
    ((cat clause)
     (restrictive yes)
     (process ((type ascriptive)
	       (mode equative)))
     (epistemic-modality "may")
     (participants
      ((identified ((cat common)
		    (lex "radiation")
		    (describer ((lex "observed")))))
       (identifier
        ((cat common)
         (definite no)
         (lex "combination")
         (qualifier
          ((cat pp)
           (np ((cat common)
                (lex "component")
                (number plural)
                (definite no)
                (describer ((cat ap)
                            (complex conjunction)
                            (distinct ~(((lex "thermal"))
                                        ((lex "non-thermal"))))))))))))))))))


(def-test np-app-11
  "The appeal to join the movement."
  ((cat common)
   (lex "appeal")
   (qualifier ((cat clause)
	       (mood to-infinitive)
	       (proc ((type material)
		      (lex "join")))
	       (partic ((agent ((semantics somebody)))
			(range ((cat common)
				(lex "movement")))))))))


;;;np-such-2: attachments such as an ocular micrometer
;;;np-such-3: such items as geranium petals, hair, fly wings, and fleas

;;;=============================================
;;;  Nonrestrictive relative clauses
;;;=============================================

(def-test res-rel-1
  "This microscope was made for an aristocratic amateur scientist, who would have used it in his cabinet de curiosite."
  ((cat clause)
   (proc ((type material)
	  (agentive no)
	  (voice passive)
   (tense past)
	  (lex "make")))
   (partic ((affected ((cat np)
		       (distance near)
		       (lex "microscope")))))
   (circum
    ((behalf ((cat pp)
              (np ((cat np)
                   (definite no)
                   (animate yes)
                   (describer ((lex "aristocratic")))
                   (classifier ((lex "amateur")))
                   (head ((lex "scientist")))
                   (qualifier
                    ((cat clause)
                     (epistemic-modality "would")
                     (restrictive no)
                     (scope {^ partic agent})
                     (proc ((type material)
                            (tense present-perfect)
                            (lex "use")))
                     (partic ((affected ((cat pronoun)
                                         (index {^7 index})))))
                     (circum
                      ((location
                        ((cat pp)
                         (prep ((lex "in")))
                         (np ((cat common)
                              (lex "cabinet de curiosite")
                              (possessor ((cat pronoun)
                                          (index {^6 index}))))))))
                      )))))))))))
