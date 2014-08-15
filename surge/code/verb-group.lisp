;;; -*- Mode:Lisp; Syntax:Common-Lisp; Package: -*-
;;; -----------------------------------------------------------------------
;;; File:         verb-group.lisp
;;; Description:  Grammatical systems for verb group
;;; Author:       Michael Elhadad
;;; Created:      19 Dec 1991
;;; Modified:     18 Aug 1992 Added adverb (inherited from clause)
;;;                5 Jul 1995 SURGE 2.2 VERSION
;;;                           - Fixed problem of wh/inversion in
;;;                             long-distance (Who do you think won?) use do.
;;;                             (embedded no).
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

;;; BRANCH FOR SIMPLE-VERB-GROUP
(def-conj simple-verb-group
  (cat simple-verb-group)
  (generic-cat verb-group)
  (complex none)
  (interrogative ((alt (none #(under yes-no) #(under wh))))) ;; default is not
  (insistence ((alt (no #(under yes)))))           ;; default is not insistent
  (alt verb-lexicalization (:demo "Lexicalization of verb")
    (((concept given)
      (lexical-verb ((concept {^ ^ concept})
		     (subcat {^ ^ subcat})
		     (lex {^ ^ lex})
		     (cat lex-verb))))
     ((lex given))))
  (alt verb-polarity (:index polarity)
    (((polarity positive)
      (notf none))
     ((polarity #(under negative))
      (notf ((lex "not") (cat adv))))))
  ;; the tensed feature may be the event or any of the auxs.
  (tensed-feature ((person { ^ ^ person})
		   (number { ^ ^ number})
		   (ending { ^ ^ ending})))
  ;; there is always an event verb -- its also called the main verb.
  (event ((cat verb)
	  (lex { ^ ^ lex})))
  ;; deal with modality: allow only one type or no modality at all
  ;; default is no modality
  (alt only-one-modal
      (((epistemic-modality none)
	(deontic-modality none)
	(modality none))
       ((epistemic-modality given)
	(deontic-modality none)
	(modality epistemic))
       ((deontic-modality given)
	(epistemic-modality none)
	(modality deontic))))

  (:! tense-selection)
  (:! modality)

  ;; Now, the tense feature should be selected.  Deal
  ;; with voice, interrogative and polarity.
  (:! voice-verb-group)

  (:! notf-adverb-placement)

  ;; Now fill the slots for auxiliaries if necessary
  (alt (((be-1 none)
	 (cset ((- be-1))))
	((be-1 given)
	 (be-1 ((lex "be")
		(cat verb))))))
  (alt (((be-2 none)
	 (cset ((- be-2))))
	((be-2 given)
	 (be-2 ((lex "be")
		(cat verb))))))
  (alt (((have-1 none)
	 (cset ((- have-1))))
	((have-1 given)
	 (have-1 ((lex "have")
		  (cat verb))))))
  (alt (((have-2 none)
	 (cset ((- have-2))))
	((have-2 given)
	 (have-2 ((lex "have")
		  (cat verb))))))
  (alt (((beg none)
	 (cset ((- beg going-to))))
	((beg given)
	 (beg ((lex "be")
	       (cat verb)))
	 ;; we need to add the going to which is never tensed.  So,
	 ;; make it a cat modal.
	 (going-to ((lex "going to")
		    (cat modal))))))
  (opt ((aux none)
	(cset ((- aux)))))

  ;; Put everything together. Notf and adverb have already been placed.
  ;; For interrogative, the tensed-feature has been fronted, so
  ;; don't put it here
  (alt pattern-interrogative (:index interrogative)
    (((interrogative none)
      (pattern (aux dots have-1 dots beg dots going-to dots
		    have-2 dots be-1 dots be-2 dots event dots)))
     ((interrogative interrogative)
      (tensed-feature ((gap yes)))
      ;; If tensed-feature not bound yet, it means aux should be
      ;; fronted by the question.
      (alt aux-or-tensed (:wait {^ tensed-feature lex})
	(((tensed-feature ((lex given))))
	 ((tensed-feature {^ aux}))))
      (alt notf-interrogative
	  (((notf none))
	   ((notf ((gap yes)
		   (lex {^ ^ ^ fronted-not lex})
		   (cat {^ ^ ^ fronted-not cat}))))))
      (alt adverb-interrogative
	  (((adverb none))
	   ((aux given))
	   ;; No aux: front adverb "who never owned this book"
	   ((aux none)
	    (adverb ((gap yes)
		     (lex {^ ^ ^ fronted-adverb lex})
		     (cat {^ ^ ^ fronted-adverb cat}))))))
      (pattern (aux dots have-1 dots beg dots going-to dots
		    have-2 dots be-1 dots be-2 dots event dots))))))




(def-alt tense-selection (:index tense)
  (:demo "what is the tense?")
  ;; ***** Done only for case of non-modal finite tense

  ;; SIMPLE TENSES
  (
   ;; tense 2, the default:
   ;; I take the bus.[a]
   ;; The bus is taken by me.[p]
   ((tense present)
    (tpattern ((:st  :equals :rt0)
	       (:st  :none   :rt1)
	       (:et  :none   :rt1)
	       (:rt0 :none   :rt1)))
    (:! aspect-choice)
    (time-frame present)
    (alt (((simple yes)
	   (modality none))
	  ((simple no))))
    (verb-aspect root)
    (tensed-feature ((tense present))))

   ;; Tense 1: past
   ;; I took the bus.[a]
   ;; The bus was taken by me.[p]
   ((tense past)
    (tpattern ((:rt0 :precedes :st)
	       (:st  :none     :rt1)
	       (:et  :none     :rt1)
	       (:rt0 :none     :rt1)))
    (alt (((simple yes)
	   (modality none))
	  ((simple no))))
    (time-frame past)
    (:! aspect-choice)
    (tensed-feature ((tense past))))

   ;; tense 3
   ;; I will take the bus.[a]
   ;; The bus will be taken by me.[b]
   ;; for the future tenses, the auxillary "will" is
   ;; treated as a modal.  There is no tensed feature
   ;; and no agreement is necessary.
   ((tense future)
    (tpattern ((:st  :precedes :rt0)
	       (:st  :none     :rt1)
	       (:et  :none     :rt1)
	       (:rt0 :none    :rt1)))
    (:! aspect-choice)
    (simple no)
    (time-frame future)
    (verb-aspect root))

   ;; tense 4
   ;; I had taken the bus.(a)
   ;; The bus had been taken by me.(p)
   ((tense past-perfect)
    (time-frame past)
    (simple no)
    (tpattern ((:rt1 :precedes :st)
	       (:rt0 :precedes :rt1)
	       (:st :none :rt2)
	       (:rt1 :none :rt2)
	       (:rt0 :none :rt2)
	       (:et :none :rt2)))
    (first-verb {^ have-1})
    (tensed-feature ((tense past)))
    (:! aspect-choice)
    (verb-aspect past-participle))

   ;; tense 5
   ;; I have taken the bus
   ;; The bus has been taken by me.
   ((tense  present-perfect)
    (time-frame present)
    (simple no)
    (tpattern ((:rt0 :precedes :rt1)
	       (:rt1 :equals :st)
	       (:st :none :rt2)
	       (:rt1 :none :rt2)
	       (:et :none :rt2)
	       (:rt0 :none :rt2)))
    (first-verb {^ have-1})
    (tensed-feature ((tense present)))
    (:! aspect-choice)
    (verb-aspect past-participle))

   ;; tense 6
   ;; I will have taken the bus.[a]
   ;; The bus will have been taken by me.[p]
   ((tense future-perfect)
    (simple no)
    (time-frame future)
    (tpattern ((:st :precedes :rt0)
	       (:rt0 :precedes :rt1)
	       (:st :none :rt2)
	       (:rt1 :none :rt2)
	       (:et :none :rt2)
	       (:rt0 :none :rt2)))
    (aux ((lex "will")
	  (cat modal)))
    (tensed-feature {^ aux})
    (first-verb {^ aux})
    (have-1 ((ending root)))
    (:! aspect-choice)
    (verb-aspect past-participle))

   ;; tense 7
   ;; I was taking the bus.(a)
   ;; The bus was being taken by me.(p)
   ((tense past-progressive)
    (simple no)
    (time-frame past)
    (tpattern ((:rt0 :precedes :st)
	       (:rt0 :includes :et)
	       (:st :none :rt1)
	       (:et :none :rt1)
	       (:rt0 :none :rt1)))
    (aspect ((alt aspect-7 (event process))))
    (be-1 ((tense past)))
    (be-1 { ^ tensed-feature})
    (verb-aspect present-participle))

   ;; tense 8
   ;; I am taking the bus.(a)
   ;; The bus is being taken by me.(p)
   ((tense present-progressive)
    (simple no)
    (time-frame present)
    (tpattern ((:rt0 :precedes :st)
	       (:rt0 :includes :et)
	       (:st :none :rt1)
	       (:et :none :rt1)
	       (:rt0 :none :rt1)))
    (aspect ((alt aspect-8 (event process))))
    (be-1 ((tense present)))
    (be-1 { ^ tensed-feature})
    (verb-aspect present-participle))

   ;; tense 9
   ;; I will be taking the bus.(a)
   ;; The bus will be being taken by me.(p)
   ((tense future-progressive)
    (simple no)
    (time-frame future)
    (tpattern ((:st :precedes :rt0)
	       (:rt0 :includes :et)
	       (:st :none :rt1)
	       (:et :none :rt1)
	       (:rt0 :none :rt1)))
    (aspect ((alt aspect-9 (event process))))
    (aux ((lex "will")
	  (cat modal)))
    (be-1 ((ending root)))
    (verb-aspect present-participle))

   ;; tense 10
   ;; I was going to take the bus.[a]
   ;; The bus was going to be taken by me.[p]
   ((tense tense-10)
    (time-frame past)
    (tpattern ((:rt1 :precedes :st) (:rt1 :precedes :rt0)
	       (:rt1 :none :rt2) (:st :none :rt2)
	       (:et :none :rt2)))
    (simple no)
    (:! aspect-choice)
    (verb-aspect root)
    (beg ((tense past)))
    (beg { ^ tensed-feature}))

   ;; tense 11
   ;; I am going to take the bus.[a]
   ;; The bus is going to be taken by me.[p]
   ((tense tense-11)
    (time-frame present)
    (tpattern ((:st :equals :rt1) (:rt1 :precedes :rt0)
	       (:st :none :rt2) (:et :none :rt2)
	       (:rt1 :none :rt2) (:rt0 :none :rt2)))
    (simple no)
    (:! aspect-choice)
    (verb-aspect root)
    (beg ((tense present)))
    (beg { ^ tensed-feature}))

   ;; tense 12
   ;; I will be going to take.[a]
   ;; The bus will be going to be taken.[b]
   ((tense tense-12)
    (time-frame future)
    (tpattern ((:st :precedes :rt1)
	       (:rt1 :precedes :rt0)
	       (:st :none :rt2) (:et :none :rt2)
	       (:rt1 :none :rt2) (:rt0 :none :rt2)))
    (simple no)
    (:! aspect-choice)
    (verb-aspect root)
    (beg ((ending root)))
    (aux ((lex "will")
	  (cat modal))))

   ;; tense 13
   ;; I was going to have taken the bus.[a]
   ;; The bus was going to have been taken by me.[p]
   ((tense tense-13)
    (simple no)
    (time-frame past)
    (tpattern ((:rt1 :precedes :st) (:rt1 :precedes :rt0)
	       (:rt0 :precedes :rt2) (:st :none :rt3)
	       (:rt0 :none :rt3) (:rt1 :none :rt3)
	       (:rt2 :none :rt3) (:et :none :rt3)))
    (:! aspect-choice)
    (beg ((tense past)))
    (beg { ^ tensed-feature})
    (have-2 ((ending root)))
    (verb-aspect past-participle))

   ;; tense 14
   ;; I am going to have taken the bus.[a]
   ;; The bus is going to have been taken by me.[p]
   ((tense tense-14)
    (simple no)
    (time-frame present)
    (tpattern ((:st :equals :rt1) (:rt1 :precedes :rt0)
	       (:rt0 :precedes :rt2) (:st :none :rt3)
	       (:rt0 :none :rt3) (:rt1 :none :rt3)
	       (:rt2 :none :rt3)))
    (:! aspect-choice)
    (beg ((tense present)))
    (beg { ^ tensed-feature})
    (have-2 ((ending root)))
    (verb-aspect past-participle))

   ;; tense 15
   ;; I will be going to have taken the bus.[a]
   ;; The bus will be going to have been taken by me.[p]
   ((tense tense-15)
    (simple no)
    (time-frame future)
    (tpattern ((:st :precedes :rt1) (:rt1 :precedes :rt0)
	       (:rt0 :precedes :rt2) (:st :none :rt3)
	       (:rt0 :none :rt3) (:rt1 :none :rt3)
	       (:rt2 :none :rt3)))
    (:! aspect-choice)
    (aux ((lex "will") (cat modal)))
    (beg ((ending root)))
    (have-2 ((ending root)))
    (verb-aspect past-participle))

   ;; tense 16
   ;; I had been taking the bus.[a]
   ;; The bus had been being taken by me.[p]
   ((tense tense-16)
    (simple no)
    (time-frame past)
    (tpattern ((:rt1 :precedes :st) (:rt0 :precedes :rt1)
	       (:rt0 :during :et) (:st :none :rt2)
	       (:rt0 :none :rt2) (:rt1 :none :rt2)))
    (aspect ((alt (process event))))
    (have-1 ((tense past)))
    (have-1 { ^ tensed-feature})
    (be-1 ((ending past-participle)))
    (verb-aspect present-participle))

   ;; tense 17
   ;; I have been taking the bus.[a]
   ;; The bus has been being taken by me.[p]
   ((tense tense-17)
    (time-frame present)
    (simple no)
    (tpattern ((:rt1 :equals :st) (:rt0 :precedes :rt1)
	       (:rt0 :during :et) (:st :none :rt2)
	       (:rt0 :none :rt2) (:rt1 :none :rt2)))
    (aspect ((alt (process event))))
    (have-1 ((tense present)))
    (have-1 { ^ tensed-feature})
    (be-1 ((ending past-participle)))
    (verb-aspect present-participle))

   ;; tense 18
   ;; I will have been taking the bus.[a]
   ;; The bus will have been being taken by me.[p]
   ((tense tense-18)
    (time-frame future)
    (simple no)
    (tpattern ((:rt1 :precedes :st) (:rt0 :precedes :rt1)
	       (:rt0 :during :et) (:st :none :rt2)
	       (:rt0 :none :rt2) (:rt1 :none :rt2)))
    (aspect ((alt (process event))))
    (aux ((lex "will") (cat modal)))
    (have-1 ((ending root)))
    (be-1 ((ending past-participle)))
    (verb-aspect present-participle))

   ;; tense 19
   ;; I was going to be taking the bus.[a]
   ;; The bus was going to be being taking the bus.[p]
   ((tense tense-19)
    (simple no)
    (time-frame past)
    (tpattern ((:rt1 :precedes :st) (:rt1 :precedes :rt0)
	       (:rt0 :during :et) (:st :none :rt2)
	       (:rt0 :none :rt2) (:rt1 :none :rt2)
	       (:et :none :rt2)))
    (aspect ((alt aspect-19 (process event))))
    (beg ((tense past)))
    (beg { ^ tensed-feature})
    (be-1 ((ending root)))
    (verb-aspect present-participle))

   ;; tense 20
   ;; I am going to be taking the bus.[a]
   ;; The bus is going to be being taken by me.[p]
   ((tense tense-20)
    (time-frame present)
    (simple no)
    (tpattern ((:rt1 :equals :st) (:rt1 :precedes :rt0)
	       (:rt0 :during :et) (:st :none :rt2)
	       (:rt0 :none :rt2) (:rt1 :none :rt2)
	       (:et :none :rt2)))
    (aspect ((alt aspect-20 (process event))))
    (beg ((tense present)))
    (beg { ^ tensed-feature})
    (be-1 ((ending root)))
    (verb-aspect present-participle))

   ;; tense 21
   ;; I will be going to be taking the bus.[a]
   ;; The bus will be going to be being taken by me.[p]
   ((tense tense-21)
    (simple no)
    (time-frame future)
    (tpattern ((:st :precedes :rt1) (:rt1 :precedes :rt0)
	       (:rt0 :during :et) (:st :none :rt2)
	       (:rt0 :none :rt2) (:rt1 :none :rt2)
	       (:et :none :rt2)))
    (aspect ((alt aspect-21 (process event))))
    (aux ((lex "will") (cat modal)))
    (beg ((ending root)))
    (be-1 ((ending root)))
    (verb-aspect present-participle))

   ;; ***** NO SEMANTICS
   ;; tense 22
   ;; I had been going to take the bus.[a]
   ;; The bus had been going to be taken by me.[p]
   ((tense tense-22)
    (simple no)
    (time-frame past)
    ;; (tpattern ((:rt0 :precedes :et)))
    (have-1 ((tense past)))
    (have-1 { ^ tensed-feature})
    (beg ((ending past-participle)))
    (verb-aspect root)
    )

   ;; ***** NO SEMANTICS
   ;; tense 23
   ;; I have been going to take the bus.[a]
   ;; The bus has been going to be taken by me.[p]
   ((tense tense-23)
    (simple no)
    (time-frame present)
    ;; (tpattern ((:rt0 :precedes :et)))
    (have-1 ((tense present)))
    (have-1 { ^ tensed-feature})
    (beg ((ending past-participle)))
    (verb-aspect root)
    )

   ;; ***** NO SEMANTICS
   ;; tense 24
   ;; I will have been going to take the bus.[a]
   ;; The bus will have been going to be taken by me.[p]
   ((tense tense-24)
    (simple no)
    (time-frame future)
    ;; (tpattern ((:rt0 :precedes :et)))
    (aux ((lex "will") (cat modal)))
    (have-1 ((ending root)))
    (beg ((ending past-participle)))
    (verb-aspect root)
    )

   ;; tense 25
   ;; I had been going to have taken the bus.[a]
   ;; The bus had been going to have been taken by me.[p]???
   ((tense tense-25)
    (simple no)
    (time-frame past)
    (tpattern ((:rt2 :precedes :st)
	       (:rt1 :precedes :rt2)
	       (:rt1 :precedes :rt0)
	       (:rt0 :precedes :rt3)))
    (:! aspect-choice)
    (verb-aspect past-participle)
    (have-1 ((tense past)))
    (have-1 {^ tensed-feature})
    (beg ((ending past-participle)))
    (have-2 ((ending root))))

   ;; tense 26
   ;; I have been going to have taken the bus.[a]
   ;; The bus has been going to have been taken by me.[p]???
   ((tense tense-26)
    (simple no)
    (time-frame present)
    (tpattern ((:rt2 :equals :st)
	       (:rt1 :precedes :rt2)
	       (:rt1 :precedes :rt0)
	       (:rt0 :precedes :rt3)))
    (:! aspect-choice)
    (verb-aspect past-participle)
    (have-1 ((tense present)))
    (have-1 {^ tensed-feature})
    (beg ((ending past-participle)))
    (have-2 ((ending root))))

   ;; tense 27
   ;; I will have been going to have taken the bus.[a]
   ;; The bus will have been going to have been taken by me.[p]???
   ((tense tense-27)
    (simple no)
    (time-frame future)
    (tpattern ((:st :precedes  :rt2)
	       (:rt1 :precedes :rt2)
	       (:rt1 :precedes :rt0)
	       (:rt0 :precedes :rt3)))
    (:! aspect-choice)
    (verb-aspect past-participle)
    (aux ((lex "will") (cat modal)))
    (have-1 ((ending root)))
    (beg ((ending past-participle)))
    (have-2 ((ending root))))

   ;; tense 28
   ;; I was going to have been taking the bus.[a]
   ;; The bus was going to have been being taken by me.[p]
   ((tense tense-28)
    (simple no)
    (time-frame past)
    (tpattern ((:rt1 :precedes :st) (:rt1 :precedes :rt0)
	       (:rt0 :precedes :rt2) (:rt0 :during :et)))
    (aspect ((alt aspect-28 (event process))))
    (tensed-feature {^ beg})          ;; first-verb
    (tensed-feature ((tense past)))
    (have-2 ((ending root)))
    (be-1 ((ending past-participle)))
    (verb-aspect present-participle))

   ;; tense 29
   ;; I am going to have been taking the bus.[a]
   ;; The bus is going to have been being taken by me.[p]
   ((tense tense-29)
    (simple no)
    (time-frame present)
    (tpattern ((:rt1 :equals :st) (:rt1 :precedes :rt0)
	       (:rt0 :precedes :rt2) (:rt0 :during :et)))
    (aspect ((alt aspect-29 (event process))))
    (beg ((tense present)))
    (beg { ^ tensed-feature})
    (have-2 ((ending root)))
    (be-1 ((ending past-participle)))
    (verb-aspect present-participle))

   ;; tense 30
   ;; I will be going to have been taking the bus.[a]
   ;; The bus will be going to have been being taken by me.[p]
   ((tense tense-30)
    (simple no)
    (time-frame future)
    (tpattern ((:st :precedes :rt1) (:rt1 :precedes :rt0)
	       (:rt0 :precedes :rt2) (:rt0 :during :et)))
    (aspect ((alt aspect-30 (event process))))
    (aux ((lex "will") (cat modal)))
    (beg ((ending root)))
    (have-2 ((ending root)))
    (be-1 ((ending past-participle)))
    (verb-aspect present-participle))

   ;; tense 31
   ;; I had been going to be taking the bus.[a]
   ;; The bus had been going to be being taken by me.[p]
   ((tense tense-31)
    (simple no)
    (time-frame past)
    (tpattern ((:rt2 :precedes :st) (:rt1 :precedes :rt2)
	       (:rt1 :precedes :rt0) (:rt0 :during :et)
	       (:st :none :rt3) (:rt0 :none :rt3) (:rt1 :none :rt3)
	       (:rt2 :none :rt3) (:et :none :rt3)))
    (aspect ((alt (process event))))
    (have-1 ((tense past)))
    (have-1 { ^ tensed-feature})
    (beg ((ending past-participle)))
    (be-1 ((ending root)))
    (verb-aspect present-participle))

   ;; tense 32
   ;; I have been going to be taking the bus.[a]
   ;; The bus has been going to be being taken by me.[p]
   ((tense tense-32)
    (simple no)
    (time-frame present)
    (tpattern ((:rt2 :equals :st) (:rt1 :precedes :rt2)
	       (:rt1 :precedes :rt0) (:rt0 :during :et)
	       (:st :none :rt3) (:rt0 :none :rt3) (:rt1 :none :rt3)
	       (:rt2 :none :rt3) (:et :none :rt3)))
    (aspect ((alt (process event))))
    (have-1 ((tense present)))
    (have-1 { ^ tensed-feature})
    (beg ((ending past-participle)))
    (be-1 ((ending root)))
    (verb-aspect present-participle))

   ;; tense 33
   ;; I will have been going to be taking the bus.[a]
   ;; The bus will have been going to be being taken by me.[p]
   ((tense tense-33)
    (simple no)
    (time-frame future)
    (tpattern ((:st :precedes :rt1) (:rt1 :precedes :rt2)
	       (:rt1 :precedes :rt0) (:rt0 :during :et)
	       (:st :none :rt3) (:rt0 :none :rt3) (:rt1 :none :rt3)
	       (:rt2 :none :rt3) (:et :none :rt3)))
    (aspect ((alt (process event))))
    (aux ((lex "will") (cat modal)))
    (have-1 ((ending root)))
    (beg ((ending past-participle)))
    (be-1 ((ending root)))
    (verb-aspect present-participle))

   ;; tense 34
   ;; I had been going to have been taking the bus.[a]
   ;; The bus had been going to have been being taken by me.[p]
   ((tense tense-34)
    (simple no)
    (time-frame past)
    (tpattern ((:rt2 :precedes :st)
	       (:rt1 :precedes :rt2)
	       (:rt1 :precedes :rt0)
	       (:rt0 :precedes :rt3)
	       (:rt0 :during :et)))
    (aspect ((alt (process event))))
    (have-1 ((tense past)))
    (have-1 { ^ tensed-feature})
    (have-2 ((ending root)))
    (beg ((ending past-participle)))
    (be-1 ((ending past-participle)))
    (verb-aspect present-participle))

   ;; tense 35
   ;; I have been going to have been taking the bus.[a]
   ;; The bus has been going to have been being taken by me.[p]
   ((tense tense-35)
    (simple no)
    (time-frame present)
    (tpattern ((:st :equals :rt2)
	       (:rt1 :precedes :rt2)
	       (:rt1 :precedes :rt0)
	       (:rt0 :precedes :rt3)
	       (:rt0 :during :et)))
    (aspect ((alt (process event))))
    (have-1 ((tense present)))
    (have-1 { ^ tensed-feature})
    (have-2 ((ending root)))
    (beg ((ending past-participle)))
    (be-1 ((ending past-participle)))
    (verb-aspect present-participle))

   ;; tense 36
   ;; I will have been going to have been taking the bus.[a]
   ;; The bus will have been going to have been being taken by me.[p]
   ((tense tense-36)
    (simple no)
    (time-frame future)
    (tpattern ((:st :precedes :rt1)
	       (:rt1 :precedes :rt2)
	       (:rt1 :precedes :rt0)
	       (:rt0 :precedes :rt3)
	       (:rt0 :during :et)))
    (aspect ((alt (process event))))
    (aux ((lex "will") (cat modal)))
    (have-1 ((ending root)))
    (have-2 ((ending root)))
    (beg ((ending past-participle)))
    (be-1 ((ending past-participle)))
    (verb-aspect present-participle))
   ))


(def-alt aspect-choice
  (((aspect event)
    (tpattern ((:rt0 :equals :et))))
   ((aspect given)
    (aspect ((alt (stative process))))
    (tpattern ((:rt0 :during :et))))))


(def-alt modality
  (:demo "what modality is used w/ this verb?")
  (
   ((modality none)
    %TRACE-OFF%
    (control (control-demo "No modality in this clause."))
    %TRACE-ON%
    (tensed-feature {^ first-verb})
    (alt time-frame (:index time-frame)
      (((time-frame present))
       ((time-frame past))
       ((time-frame future)
	(aux ((lex "will") (cat modal))))
       )))
   ((epistemic-modality fact)
    (tensed-feature {^ first-verb})
    (alt (:index time-frame)
	(((time-frame present))
	 ((time-frame past))
	 ((time-frame future)
	  (aux ((lex "will") (cat modal))))
	 )))
   ((epistemic-modality inference)
    (first-verb ((ending root)))
    (alt (:index time-frame)
	(((time-frame present)
	  (aux ((lex "must") (cat modal))))
	 ((time-frame future)
	  ;; there is already a will
	  )
	 ((time-frame past)
	  (aux ((lex "must've") (cat modal))))
	 )))
   ((epistemic-modality possible)
    (first-verb ((ending root)))
    (alt (:index time-frame)
	(((time-frame present)
	  (aux ((lex "can") (cat modal))))
	 ((time-frame future)
	  (aux ((lex "can") (cat modal))))
	 ((time-frame past)
	  (aux ((lex "could have")))))))
   ((epistemic-modality given)
    (first-verb ((ending root)))
    (aux ((cat modal)
	  (lex {^ ^ epistemic-modality}))))
   ((deontic-modality duty)
    (first-verb ((ending root)))
    (alt (:index time-frame)
	(((time-frame present)
	  (aux ((lex "must") (cat modal))))
	 ((time-frame future)
	  (aux ((lex "must") (cat modal))))
	 ((time-frame past)
	  (aux ((lex "must've") (cat modal))))
	 )))
   ((deontic-modality given)
    (first-verb ((ending root)))
    (aux ((cat modal)
	  (lex {^ ^ deontic-modality}))))
   ((modality given)
    (first-verb ((ending root)))
    (aux ((cat modal)
	  (lex {^ ^ modality}))))
   ))



(def-alt voice-verb-group (:index voice)
  (:wait {^ event lex})
  (
   ;; First: very special case of "to be"
   ;; passive is "to be" - no auxiliary for negation and question
   ((event ((lex "be")))
    (alt simple-be (:index simple)
      ;; for simple tenses, don't add "do"
      (((simple yes)
	;; (event {^ first-verb})
	(event {^ tensed-feature}))
       ((simple no)
	(event ((ending {^ ^ verb-aspect})))))))
   ;; then special case of all copulae: passive is themselves
   ;; and all other verbs at active
   ((alt (((voice active))
	  ((copula #(under yes)))))
    (alt simple-do (:index simple)
      (
       ;; For simple tenses, the auxillary "do" must be added
       ;; e.g. I do not open the door
       ;; or   Did you open the door?
       ((simple yes)
	(alt simple-polarity (:index polarity)
	  (((polarity negative)   ;; e.g. I did not see it.
	    (aux ((lex "do") (cat verb)))
	    (aux {^ tensed-feature})
	    (event ((ending root))))
	   ((polarity positive) 	;; e.g. I saw it
	    (alt simple-interrogative (:index interrogative)
	      ((;; When wh question where scope is subject NOT EMBEDDED
		;; don't use aux.
		;; Example: Who do you think really won the prize?
		(interrogative wh)
		({^ scope} ((synt-funct #(under subject))
			    (clause-level ((embedded no)))))
		(aux none)
		(event { ^ tensed-feature}))
	       ((interrogative interrogative)
		(aux ((lex "do") (cat verb)))
		(aux {^ tensed-feature})
		(event ((ending root))))
	       ((alt simple-insistence (:index insistence)
		  (((insistence no)
		    (aux none)
		    (event { ^ tensed-feature}))
		   ((insistence yes)
		    (aux ((lex "do") (cat verb)))
		    (aux {^ tensed-feature})
		    (event ((ending root)))))))))))))
       ((simple no)
	;; there is an aux of one kind or another,
	;; the default ending of the event should be
	;; set event ending???
	(event ((ending {^ ^ verb-aspect})))))))
   ((voice passive)
    (copula no)
    (alt passive-simple (:index simple)
      (;; no auxilliary necessarily
       ((simple yes)
	(event ((ending past-participle)))
	(be-2 { ^ tensed-feature}))
       ;; there's an auxilliary
       ((simple no)
	(event ((ending past-participle)))
	(be-2 ((ending { ^ ^ verb-aspect})))))  )) ))


(def-alt notf-adverb-placement (:index polarity)
  ;; ***** WORK ON NEGATION OF NON-FINITE
  (((polarity positive)
    (adverb none))
   (
    (alt aux-notf (:index aux)
      (((aux given)
	(pattern (dots aux notf adverb dots)))
       ((aux none)
	(alt have-1-notf (:index have-1)
	  (((have-1 given)
	    (pattern (dots have-1 notf adverb dots)))
	   ((have-1 none)
	    (alt beg-notf (:index beg)
	      (((beg given)
		(pattern (dots beg notf adverb dots)))
	       ((beg none)
		(alt have-2-notf (:index have-2)
		  (((have-2 given)
		    (pattern (dots have-2 notf adverb dots)))
		   ((have-2 none)
		    (alt be-1-notf (:index be-1)
		      (((be-1 given)
			(pattern (dots be-1 notf adverb dots)))
		       ((be-1 none)
			(alt be-2-notf (:index be-2)
			  (((be-2 given)
			    (pattern (dots be-2 notf adverb dots)))
			   ((be-2 none)
			    ;; there are no aux'es but the polarity
			    ;; is negative.  It must be the "be"
			    ;; special case: I am not a tree.
			    ;; or else there is only an adverb:
			    ;; "I never do it."
			    (pattern (dots adverb event notf dots))))
			  )))
		      )))
		  )))
	      )))
	  )))
      )))
  )


(store-verbs '(("run up" "runs up" "ran up" "running up" "run up")
	       ("open" "opens" "opened" "opening" "opened")))

;; ============================================================
(provide "verb-group")
;; ============================================================
