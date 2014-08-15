;;; -*- Mode:Lisp; Syntax:Common-Lisp; Package: -*-
;;; -----------------------------------------------------------------------
;;; File:         transitivity.lisp
;;; Description:  Grammar systems of the clause dealing with transitivity
;;; Author:       Michael Elhadad
;;; Created:      19 Dec 1991
;;; Modified:     07 Jan 1992: add a realization link when mapping is not
;;;                            direct constituent to constituent.  To be
;;;                            used by the relative system and add info
;;;                            about relative-marker and question-pronoun.
;;;               09 Jan 1992: add question and relative info for each role.
;;;               05 Jul 1995: SURGE 2.2 VERSION
;;;               12 May 1996: Allow all combinations of partic for verbal
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

(def-alt transitivity
  ;; TRANSITIVITY SYSTEM
  ;; Transitivity system determines what configuration of participants
  ;; can be used for the current process and maps the participants to a
  ;; position in the obliqueness hierarchy.
  ;; The voice system does the mapping semantic -> syntactic roles
  ;; Things done:
  ;; - Determine configuration of participants
  ;; - Map participants to obliqueness level
  ;; - Provides default verbs for processes
  ;; There are 35 kernel templates, with 35 active and 18 passive forms
  (:index process-type)
  (:demo "Is the process simple or composite?")
  (((process-type lexical)
    ;; Need to have the mapping lexical-roles -> oblique in lexicon
    ;; The mapping is under the subcat feature of the process.
    (process ((subcat given)))
    (oblique {^ process subcat}))

   ;; Following are the general classes of verbs
   ;; using Fawcett's transitivity system.
   ((process-type simple-process)
    (:! simple-process))

   ((process-type composite)
    (participants ((range none))) ;; don't want to mess with that
    (:! composite-process))))



(def-alt simple-process (:index process-type)
  ;; Simple processes: list of templates
  ;; Material:
  ;; Ag:       John runs.
  ;; Ag+Af     John eats a pie.
  ;; Ag+Cr     John cooks diner.
  ;; Ag+Rg     John sings a song.
  ;; Af        The sun shines.
  ;; Cr        The window popped.
  ;; Mental:
  ;; Pr        I think.
  ;; Pr+Ph     I think it's good.
  ;; Verbal:
  ;; Sa        It talks.
  ;; Sa+Ad     John talks to Steve.
  ;; Sa+Ve     Steve says fix it.
  ;; Sa+Ad+Ve  Steve asks Doree to fix it.
  ;; Ascriptive:
  ;; Ca        I am.
  ;; Ca+At     It is not personal.
  ;; Id+Ir     Steve is Doree's advisor.
  ;; Possessive:
  ;; Ca+Pd  [Pr = Ca]   Steve has advisees.
  ;; Id+Pd  [Pr = Id]   Steve owns a box.
  ;; Locative:
  ;; [none]             It rains.
  ;; Ca                 There is a unicorn.
  ;; Ca+Ln  [Ca = Ld]   Steve is in his office.
  ;; Id+Ln  [Ir = Ld]   Steve's office contains a computer.
  (
   ;; Material: Ag+Af or Ag+Cr or Ag+Rg
   ((process-type material)
    (participants ((fset (agent affected created range))))
    ;; Enumerate all acceptable subsets of these roles based on agentive,
    ;; dispositive and creative.
    (:! material-simple-agentive))

   ;; Mental processes: *****
   ((process-type mental)
    (participants ((fset (processor phenomenon))))
    (oblique ((fset (1 2))
	      (1 {^2 participants processor})
	      (2 {^2 participants phenomenon})))
    (alt mental-transitive (:index (process transitive))
      (((process ((transitive yes)))
	(participants ((phenomenon any))))
       ((process ((transitive no)))
	(participants ((phenomenon none)))))))

   ;; Verbal: not tested yet
   #+ignore((process-type verbal)
    (participants ((fset (sayer addressee verbalization))))
    (oblique ((fset (1 2 3))
	      (1 {^ ^ participants sayer})
	      (2 {^ ^ participants verbalization})
	      (3 {^ ^ participants addressee}))))

   ;; JR-1/30/93: allowed any combination of the three roles **UNTESTED**
   ((process-type verbal)
    (participants ((fset (sayer addressee verbalization))))
    (oblique ((fset (1 2 3))))
    (alt sayer-present
      (((participants ((sayer none))))
       ((participants ((sayer given)))
	(oblique ((1 {^2 participants sayer}))))))
    (alt verbalization-present
      (((participants ((verbalization none)))
	(oblique ((2 none))))
       ((participants ((verbalization given)))
	(oblique ((2 {^2 participants verbalization}))))))
    (alt addressee-present
      (((participants ((addressee none)))
	(oblique ((3 none))))
       ((participants ((addressee given)))
	(oblique ((3 {^3 participants addressee})))))))

   ;; Relational processes:
   ((process-type relation)
    ;; General things on Mode
    (alt mode (:index (process mode))
      (((process ((mode attributive)
		  (voice active)))
	(oblique ((1 {^ ^ participants carrier}))))
       ((process ((mode equative)))
	(oblique ((1 {^ ^ participants identified}))))))
    (process ((change-mode ((alt (current maintain change))))))

    ;; Enumerate specializations of relation:
    ;; Ascriptive, Locative, Possessive
    (:! relational-simple))))


(def-alt material-simple-agentive (:index (process agentive))
  ;; Within a clause, (type material)
  (((process ((agentive yes)))
    (oblique ((fset (1 2))
	      (1 {^ ^ participants agent})))
    (alt effective (:index (process effective))
      (((process ((effective yes)))
	(alt effect-type (:index (process effect-type))
	  (((process ((effect-type dispositive)))
	    (participants ((affected given)
			   (fset (agent affected))))
	    (oblique ((2 {^ ^ participants affected}))))
	   ((process ((effect-type creative)))
	    (participants ((created given)
			   (fset (agent created))))
	    (oblique ((2 {^ ^ participants created}))))
	   ;; end of effective yes
	   )))
       ((process ((effective no)
		  (agentive yes)))
	(participants ((fset (agent range))))
	(alt range-transitive (:index (participants range))
	  (((participants ((range given)))
	    (oblique ((2 {^ ^ participants range}))))
	   ((participants ((range none)))
	    (oblique ((fset (1))))))))
       ;; end of agentive yes
       )))

   ((process ((agentive no)
	      (effective yes)))
    (oblique ((fset (1))))
    (alt af-type (:index (process effect-type))
      (((process ((effect-type dispositive)))
	(participants ((fset (affected))))
	(oblique ((1 {^ ^ participants affected}))))
       ((process ((effect-type creative)))
	(participants ((fset (created))))
	(oblique ((1 {^ ^ participants created})))))))))


(def-alt relational-simple (:index process-type)
  ;; Enumerate specializations of relational process/roles
  ;; Ascriptive, Locative, Possessive
  (
   ;; ASCRIPTIVE - sort of default
   ((process-type ascriptive)
    (:! ascriptive-mode))
   ;; LOCATIVE
   ((process-type locative)
    (:! locative-mode)
    ;; default verb locative (both equative and attrib)
    (process ((alt (((lex "be")
		     (copula yes)
		     (subject-clause none)
		     (object-clause none))
		    ((lex given)))))))
   ;; POSSESSIVE
   ((process-type possessive)
    (:! possessive-mode))))



(def-alt ascriptive-mode (:index (process mode))
  ;; Simple-process of type ascriptive: attributive vs equative.
  ;; Corresponding default verb
  ;; Note that "to be" in equative and attrib. has different
  ;; properties in terms of subject-clause/object-clause.
  (((process ((mode attributive)))
    (participants ((fset (carrier attribute))
		   (attribute ((question-pronoun ((lex "how")))))))
    (oblique ((fset (1 4))
	      (4 {^2 participants attribute})))
    (alt verb-be-attributive
	(:demo "What verb for ascriptive attributive?")
      (((process ((lex "be")
		  (subject-clause infinitive)
		  (object-clause none))))
       ((process ((lex given)))))))
   ((process ((mode equative)))
    ;; So that relatives and questions select "which" and no "what"
    (participants ((identifier ((restrictive yes)))))
    (oblique ((fset (1 2))
	      (2 {^ ^ participants identifier})))
    (alt verb-be-equative
	(:demo "What verb for ascriptive equative?")
      (((process ((lex "be")
		  (copula yes)
		  (subject-clause that)
		  (object-clause present-participle))))
       ((process ((lex given)))))))))


(def-alt locative-mode (:index (process mode))
  ;; Enumerate different types of locative relations
  ;; locative can have 0, 1 or 2 participants
  ;; 0 and 1 also have a dummy constituent (it or there).
  ;; natural-phenom, existential, temporal, locative for both equative
  ;; and attributive
  (((process ((mode attributive)))
    (alt loc-arity (:index process-type)
      (
       ;; NATURAL-PHENOMENON
       ((process-type #(under natural-phenom))
	(participants none)
	(dummy-constituent yes)
	(oblique none))

       ;; EXISTENTIAL
       ((process-type #(under existential))
	(participants ((fset (carrier located))
		       (located {^ carrier})))
	(dummy-constituent yes)
	(oblique ((fset (1)))))

       ;; TEMPORAL
       ((process-type #(under temporal))
	(participants
	 ((fset (carrier attribute located time))
	  (located {^ carrier})
	  (time {^ attribute})))
	(alt (((process ((circumstance-as participant)))
	       (participants
		((time ((question-embedded no)
			(relative-embedded no)
			(question-pronoun ((lex ((alt (given "when"))))))
			(relative-marker ((lex ((alt (given "when")))))))))))
	      ((process ((circumstance-as process))))))
	(oblique ((fset (1 4))
		  (4 {^ ^ participants time}))))

       ;; ANY OTHER LOCATIVE
       ((process-type locative)
	(participants
	 ((fset (carrier attribute located location))
	  (located {^ carrier})
	  (location {^ attribute})))
	(alt (((process ((circumstance-as participant)
			 (type #(under accompaniment))))
	       (participants
		((location ((question-embedded yes)
			    (relative-embedded yes)
			    (prep ((lex ((alt (given "with")))))))))))
	      ((process ((circumstance-as participant)
			 (type spatial)))
	       (participants
		((location
                  ((question-embedded no)
                   (relative-embedded no)
                   (question-pronoun ((lex ((alt (given "where"))))))
                   (relative-marker ((lex ((alt (given "where")))))))))))
	      ((process ((circumstance-as process))))))
	(oblique ((fset (1 4))
		  (4 {^ ^ participants location})))))))

   ;; restrictive for questions: optional for locatives.
   ((process ((mode equative)))
    (alt (((process-type #(under temporal))
	   (participants
	    ((fset (identifier identified located location time))
	     (time {^ location}))))
	  ((process-type locative)
	   (participants
	    ((fset (identifier identified located location)))))))
    (participants ((located {^ identified})
		   (location {^ identifier})))
    (oblique ((fset (1 2))
	      (2 {^ ^ participants identifier}))))))



(def-alt possessive-mode (:index (process mode))
  (((process ((mode attributive)))
    (participants
     ((fset (carrier attribute possessor possessed))
      (possessor {^ carrier})
      (possessor ((question-embedded no)
		  (relative-embedded no)))
      (possessed {^ attribute})))
    (oblique ((fset (1 4))
	      (4 {^ ^ participants possessed})))
    ;; Default verb is "have"
    (alt verb-have-possessive
	(:demo "What verb for possessive?")
      (((process ((lex "have")
		  (subject-clause infinitive)
		  (object-clause none))))
       ((process ((lex given)))))))

   ;; restrictive is optional
   ((process ((mode equative)))
    (participants
     ((fset (identifier identified possessor possessed))
      (possessor {^ identified})
      (possessed {^ identifier})))
    (oblique ((fset (1 2))
	      (2 {^ ^ participants possessed})))
    ;; Default verb is "own"
    (alt verb-own-possessive
	(:demo "What verb for possessive?")
      (((process ((lex "own")
		  (subject-clause none)
		  (object-clause none))))
       ((process ((lex given)))))))))



(def-alt composite-process
  ;; COMPOSITE PROCESSES:
  ;; compose only material + relation
  ;; Specify relation type under relation-type.
  ;; Index on agentive/effective/effect-type
  ;; Best to view composition as adding a result relation to an action
  ;; Table of possible combinations:
  ;; event = Ag, Ag+Af, Ag+Cr, Af, Cr
  ;; relation = Ca+Att, Ca+Pos, Ca+Loc, Id+Ir, [Id+Loc, Id+Pos]
  ;; The following embedded alts identify only the permissible
  ;; combinations [think of it as a matrix event/rel]
  ;; Ag+Af/Ca+At:  they made him rich
  ;; Ag+Af/Id+Ir:  they made him the boss *****
  ;; Ag/Ca+Af+At:  she made him a good wife
  ;; Ag+Af/Ca+Pos: the boss gave the babe cold cash
  ;; Ag+Af/Ca+Loc: I push the box to the left
  ;; Ag+Cr/Ca+At:  he cooked the diner spicy (?)
  ;; Ag+Cr/Ca+Loc: The program popped the window on the screen
  ;; Ag/Ca+At:     he became rich
  ;; Ag/Id+Ir:     he became the boss
  ;; Ag/Ca+Pos:    the boss bought a Rolls
  ;; Ag/Ca+Loc:    he went home
  ;; Af/Ca+At:     the kettle boiled dry
  ;; Af/Ca+Pos:    the boss received cash
  ;; Af/Ca+Loc:    the box fell on the floor
  ;; Cr/Ca+At:     he was born blind (?)
  ;; Cr/Ca+Loc:    the window popped on the screen
  (:index (process agentive))
  (((process ((agentive yes)))
    (participants ((agent ((function agent)))))
    (oblique ((1 {^ ^ participants agent})))
    (alt composite-effective (:index (process effective))
      (((process ((effective yes)))
	(participants ((agent ((function third-party-agent)))))
	(:! composite-agentive-effect-type))
       ;; Agent only
       ((process ((effective no)))
	(:! composite-agentive-relational)))))

   ((process ((agentive no)
	      (effective yes)
	      (mode attributive)))  ;; no equative allowed
    (participants ((agent none)
		   (carrier ((function carrier)))))
    (:! composite-non-agentive-effect-type))))



(def-alt composite-agentive-effect-type (:index (process effect-type))
  ;; Within a composite-process with (agentive yes) (effective yes)
  (((process ((effect-type dispositive)))
    ;; Structure is Ag+Af
    (oblique ((2 {^ ^ participants affected})))
    ;; Enumerate acceptable relation types
    (:! ag-disp-relation-type)
    ;; Check now that affected or any synonym is indeed given
    (participants ((affected any)
		   (affected ((function affected))))))

   ((process ((effect-type creative)))
    ;; Structure is Ag+Cr
    ;; default verb is "create"
    (process ((alt (((lex "create")
		     (object-clause none))
		    ((lex given))))))
    ;; Enumerate acceptable relation types
    (alt ag-cr-relation (:index (process relation-type))
      (
       ;; Ag+Cr/Ca+At:  he cooked the diner spicy (?)
       ((process ((relation-type ascriptive)
		  (mode attributive)))
	(participants
	 ((attribute ((question-pronoun ((lex ((alt (given "how"))))))))
	  (fset (agent created carrier attribute)))))

       ;; Ag+Cr/Ca+Loc: The prg popped the wnd on the screen
       ((process ((relation-type locative)
		  (mode attributive)))
	(participants
	 ((location ((question-pronoun ((lex ((alt (given "where"))))))
		     (relative-marker ((lex ((alt (given "where"))))))
		     (question-embedded no)
		     (relative-embedded no)))
	  (located {^ carrier})
	  (attribute {^ location})
	  (fset (agent created carrier attribute located location)))))
       ))
    ;; Check now that created or any synonym is given
    (participants ((created any)
		   (carrier {^ created})
		   (created ((function created)))))
    (oblique ((fset (1 2 4))
	      (2 {^ ^ participants created})
	      (4 {^ ^ participants attribute}))))))


(def-alt ag-disp-relation-type (:index (process relation-type))
  (
   ;; Ag+Af/Ca+At: they made him rich
   ((process ((relation-type ascriptive) (mode attributive)))
    (participants
     ((attribute ((question-pronoun ((lex ((alt (given "what"))))))))
      (carrier ((function carrier)))
      (affected {^ carrier})
      (fset (agent affected carrier attribute))))
    (oblique ((fset (1 2 4))
	      (4 {^ ^ participants attribute})))
    ;; Default verb is "make"
    (process ((alt (((lex "make"))
		    ((lex given)))))))

   ;; Ag+Af/Id+Ir: they elected him the boss
   ((process ((relation-type ascriptive) (mode equative)))
    (participants
     ((identifier ((question-pronoun ((lex ((alt (given "what"))))))))
      (identified ((function identified)))
      (affected {^ identified})
      (fset (agent affected identified identifier))))
    (oblique ((fset (1 2 4))
	      (4 {^ ^ participants identifier})))
    ;; Default verb is "make"
    (process ((alt (((lex "make"))
		    ((lex given)))))))

   ;; Ag+Af/Ca+Pos: the babe gave the boss cold cash
   ((process ((relation-type possessive)
	      (mode attributive)))
    (participants
     ((fset (agent affected carrier attribute possessor possessed))
      (possessed any)
      (carrier ((function carrier)))
      (affected {^ carrier})
      (possessor {^ carrier})
      (possessor ((question-embedded yes)
		  (relative-embedded yes)
		  (prep ((alt (((lex {^4 process dative-prep})
				(lex given))
			       ((lex "to"))))))))
      (possessed {^ attribute})))
    (oblique ((fset (1 2 3))
	      (3 {^ ^ participants possessed})))
    ;; Default verb is "give"
    (process ((alt (((lex "give"))
		    ((lex given)))))))

   ;; Ag+Af/Ca+Loc: I push the box to the left
   ((process ((relation-type locative)
	      (mode attributive)))
    (participants
     ((location ((question-pronoun ((lex ((alt (given "where"))))))
		 (question-embedded no)
		 (relative-embedded no)
		 (relative-marker ((lex ((alt (given "where"))))))))
      (carrier ((function carrier)))
      (located {^ carrier})
      (location {^ attribute})
      (affected {^ carrier})
      (fset (agent affected carrier attribute located location))))
    (oblique ((fset (1 2 4))
	      (4 {^ ^ participants location})))
    ;; Default verb is "move"
    (process ((alt (((lex "move"))
		    ((lex given)))))))
   ))


(def-alt composite-agentive-relational (:index (process relation-type))
  ;; Within a composite-process with (agentive yes) (effective no)
  ;; Enumerate permissible relation types
  (
   ;; Ag/Ca+At:     he became rich
   ((process ((relation-type ascriptive)
	      (mode attributive)))
    (participants
     ((carrier ((function carrier)))
      (carrier {^ agent})
      (attribute ((question-pronoun ((lex ((alt (given "what"))))))))
      (fset (agent carrier attribute))))
    (oblique ((fset (1 4))
	      (4 {^ ^ participants attribute})))
    ;; Default verb is "become"
    (process ((alt (((lex "become")
		     (subject-clause none)
		     (object-clause none)
		     (change-mode change))
		    ((lex given)))))))

   ;; Ag/Id+Ir:     he became the boss
   ((process ((relation-type ascriptive)
	      (voice active)
	      (mode equative)))
    (participants
     ((identifier ((question-pronoun ((lex ((alt (given "what"))))))))
      (identified ((function identified)))
      (identified {^ agent})
      (fset (agent identified identifier))))
    (oblique ((fset (1 4))
	      (4 {^ ^ participants identifier})))
    ;; Default verb is "become"
    (process ((alt (((lex "become")
		     (subject-clause none)
		     (object-clause none)
		     (change-mode change))
		    ((lex given)))))))

   ;; Ag/Ca+Pos:    the boss bought a Rolls
   ((process ((relation-type possessive)
	      (mode attributive)))
    (participants
     ((possessed any)
      (carrier ((function carrier)))
      (carrier {^ agent})
      (attribute {^ possessed})
      (possessor {^ carrier})
      (fset (agent carrier attribute
		   possessor possessed))))
    (oblique ((fset (1 3))
	      (3 {^ ^ participants possessed})))
    ;; Default verb is "get"
    (process ((alt (((lex "get"))
		    ((lex given)))))))

   ;; Ag/Ca+Loc:    he went home
   ((process ((relation-type locative)
	      (mode attributive)))
    (participants
     ((location ((question-embedded no)
		 (relative-embedded no)
		 (question-pronoun ((lex ((alt (given "where"))))))
		 (relative-marker ((lex ((alt (given "where"))))))))
      (attribute {^ location})
      (carrier {^ located})
      (carrier {^ agent})
      (carrier ((function carrier)))
      (fset (agent carrier attribute located location))))
    (oblique ((fset (1 4))
	      (4 {^ ^ participants location})))
    ;; Default verb is "go"
    (process ((alt (((lex "go")
		     (object-clause none))
		    ((lex given)))))))))


(def-alt composite-non-agentive-effect-type (:index (process effect-type))
  (((process ((effect-type dispositive)))
    ;; Structure is Af
    (participants ((carrier {^ affected})
		   (affected any)
		   (affected ((function affected)))))
    (oblique ((1 {^ ^ participants affected})))

    ;; Enumerate acceptable relation types
    (:! af-relation-type))

   ((process ((effect-type creative)))
    ;; Structure is Cr
    (participants ((carrier {^ created})
		   (created any)
		   (created ((function created)))))
    (oblique ((1 {^ ^ participants created})))

    ;; Enumerate acceptable relation types
    (alt cr-relation (:index (process relation-type))
      (
       ;; Cr/Ca+At:     The window popped wide
       ((process ((relation-type ascriptive)))
	(participants
	 ((attribute ((question-pronoun ((lex ((alt (given "how"))))))))
	  (fset (created carrier attribute))))
	(oblique ((fset (1 4))
		  (4 {^ ^ participants attribute})))
	(process ((lex given))))

       ;; Cr/Ca+Loc:    the window popped on the screen
       ((process ((relation-type locative)
		  (voice active)))
	(participants
	 ((location ((alt location-q-pronouns (:index question-embedded)
		       (((question-embedded no)
			 (question-pronoun ((lex ((alt (given "where")))))))
			((question-embedded yes))))
		     (alt location-r-pronouns (:index relative-embedded)
		       (((relative-embedded no)
			 (relative-marker ((lex ((alt (given "where")))))))
			((relative-embedded yes))))))
	  (location {^ attribute})
	  (located {^ carrier})
	  (fset (created carrier attribute located location))))
	(oblique ((fset (1 4))
		  (4 {^ ^ participants attribute})))
	(process ((lex given)))))))))


(def-alt af-relation-type (:index (process relation-type))
  (
   ;; Af/Ca+At:     the kettle boiled dry
   ((process ((relation-type ascriptive)))
    (participants
     ((attribute ((question-pronoun ((lex ((alt (given "how"))))))))
      (fset (affected carrier attribute))))
    (oblique ((fset (1 4))
	      (4 {^ ^ participants attribute})))
    ;; Default verb is "turn"
    (process ((alt (((lex "turn"))
		    ((lex given)))))))

   ;; Af/Ca+Pos:    the boss received cash
   ((process ((relation-type possessive)))
    (participants
     ((possessed any)
      (possessed {^ attribute})
      (possessor {^ carrier})
      (fset (affected carrier attribute
		      possessor possessed))))
    (oblique ((fset (1 3))
	      (3 {^ ^ participants possessed})))
    ;; Default verb is "get"
    (process ((alt (((lex "get"))
		    ((lex given)))))))

   ;; Af/Ca+Loc:    the box fell on the floor
   ((process ((relation-type locative)
	      (voice active)))
    (participants
     ((location ((alt location-q-pronouns (:index question-embedded)
		   (((question-embedded no)
		     (question-pronoun ((lex ((alt (given "where")))))))
		    ((question-embedded yes))))
		 (alt location-r-pronouns (:index relative-embedded)
		   (((relative-embedded no)
		     (relative-marker ((lex ((alt (given "where")))))))
		    ((relative-embedded yes))))))
      (location {^ attribute})
      (located {^ carrier})
      (fset (affected carrier attribute located location))))
    (oblique ((fset (1 4))
	      (4 {^ ^ participants location})))
    ;; Default verb is "move"
    (process ((alt (((lex "move")
		     (subject-clause none))
		    ((lex given)))))))))


;; ============================================================
(provide "transitivity")
;; ============================================================
