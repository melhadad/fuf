;;; --- Mode:Lisp; Syntax:Common-Lisp; Package:FUG5 ---
;;; ------------------------------------------------------------
;;; File        : GR3.L
;;; Description : FU grammar based on Halliday's transitivity
;;;               with only one process-type (action).
;;;               (System and Function in Language 159-173)
;;;               (Winograd Appendix B 470-553)
;;; Author      : Michael Elhadad
;;; Created     : 15-Jun-88
;;; Modified    : 26-Feb-89
;;;               18 May 90
;;; Language    : Common Lisp
;;; Package     : FUG5
;;; ------------------------------------------------------------

(in-package "FUG5")

(defun gsetup3 ()
  ;; No type declaration in effect.
  (clear-bk-class)
  (reset-typed-features)

  ;; This grammar takes a semantic input and maps it first into syntactic roles.
  ;; This is implemented by the "transitivity system"

  ;; Process-type     | Semantic roles
  ;; -----------------+----------------
  ;; - actions        | prot, goal, benef

  ;; Semantic information needed in verb:
  ;;      Feature     |               Possible values
  ;; -----------------+------------------------------------------------
  ;; transitive-class : neutral, intransitive, transitive, bitransitive
  ;; dative-prep      : "to", "for"

  (setq *u-grammar*
	'((alt
	   (
	    ;;==============================================================
	    ;; 01 CAT CLAUSE : clause --------------------------------------
	    ;;==============================================================
	    ((cat clause)
	     ;; Process 1: Action --> actions, events, natural phenomena
	     ;; In GR3 only actions done.
	     ;; inherent cases    --> prot, goal, benef.
	     ;; all are optional, but at least one of goal or prot must be present.
	     (process-type actions)
	     (prot ((alt (none ((cat np) (animate yes))))))
	     (goal ((alt (none ((cat np))))))
	     (benef ((alt (none ((cat np))))))
	     (verb ((process-class actions)
		    (lex given)))	;there must be a verb given

	     ;; Voice choice --> active/passive
	     ;; Choice active/passive should be based on focus (not done here)
	     ;; The voice alternation does the mapping semantic case -> syntactic roles
	     (alt voice (:index voice)
		  ;; Voice active
		  (((voice active)
		    (verb ((voice active)))
		    (subject {^ prot})
		    (object {^ goal})
		    (iobject {^ benef}))

		   ;; Voice passive
		   ((voice passive)
		    (verb ((voice passive)))
		    (alt 
		     ;; Is there an explicit prot in the input?
		     (((prot none)
		       (by-obj none))
		      ((prot given)
		       (by-obj ((np {^ ^ prot}))))))
		    (alt
		     ;; subject is either benef or goal
		     ;; "A book is given to mary by john"
		     ;; "Mary is given a book by John"
		     (((subject {^ goal})
		       (iobject {^ benef}))
		      ((subject {^ benef})
		       (object  {^ goal})))))))

	     ;; Number of inherent participants to the process
	     ;; Based on verb classification:
	     ;; Neutral: 1 or 2 participants
	     ;; Intransitive: 1 participant
	     ;; Transitive:   2 participants
	     ;; Bitransitive: 3 participants
	     (alt transitive (:index (verb transitive-class))
		  (((verb ((transitive-class intransitive)))
		    (object none)
		    (iobject none))
		   ((verb ((transitive-class transitive)))
		    (iobject none))
		   ((verb ((transitive-class neutral)))
		    (iobject none))
		   ((verb ((transitive-class bitransitive))))))

	     ;; General things: arrange syntactic roles together
	     ;; and do the agreements.
	     ;; The patterns are here.

	     ;; Number agreement
	     (verb ((cat verb-group)
		    (number {^ ^ subject number})))

	     ;; Arrange order of complements
	     (pattern (subject verb dots object dots))
	     (alt verb-voice (:index (verb voice))
		  ;; VERB VOICE ACTIVE
		  (((verb ((voice active)))
		    (alt dative
			 ;; John gave Mary the book
			 (((verb ((transitive-class bitransitive)
				  (dative-prep none)))
			   (pattern (dots verb iobject object dots)))
			  ;; John gives a book to Mary
			  ((verb ((dative-prep given)))
			   (dative ((cat pp) 
				    (prep ((lex {^ ^ ^ verb dative-prep})))
				    (np {^ ^ iobject})))
			   (pattern (dots verb object dative dots)))
			  ;; Catch all for non-bitransitive cases
			  ((verb ((dative-prep none)))))))

		   ;; VERB VOICE PASSIVE
		   ((verb ((voice passive)))
		    (alt
		     (((by-obj none))
		      ((by-obj ((cat pp)
				(prep ((lex "by")))))
		       (pattern (dots verb object by-obj dots)))))
		    (alt
		     (((iobject none))
		      ;; the book is given by John to Mary
		      ((verb ((dative-prep given)))
		       (dative ((cat pp)
				(prep ((lex {^ ^ ^ verb dative-prep})))
				(np {^ ^ iobject})))
		       (pattern (dots verb dots by-obj dative dots)))))))))
    
	    ;;==============================================================
	    ;; 02 CAT VERB-GROUP -------------------------------------------
	    ;;==============================================================
	    ;; No auxiliary / no person
	    ((cat verb-group)
	     (alt verb (:index voice)
		  (((voice active)
		    (pattern (v dots))
		    (v ((cat verb) (lex {^ ^ lex}) (number {^ ^ number}))))
		   ((voice passive)
		    (pattern (v1 v dots))
		    (v1 ((cat verb) (number {^ ^ number}) (lex "be")))
		    (v ((cat verb) (lex {^ ^ lex}) (ending past-participle)))))))

	    ;;==============================================================
	    ;; 03 CAT NP ---------------------------------------------------
	    ;;==============================================================
	    ;; Prototypical sequence: determiner describer head qualifier
	    ;; We expect in the input at the top-level constituents:
	    ;; - definite yes/no (default is yes).
	    ;; - a lex that will be interpreted as the lex of the head.
	    ;; - describer: an adj or similar modifier
	    ;; - qualifiers: a pp or relative clause

	    ((cat np)
	     ;; GENERAL NP =================================================
					; np inherits major features from head and definite from determiner
	     (head ((lex {^ ^ lex})
		    (number {^ ^ number})))
	     (alt np-type (:index np-type)
		  ;; Common nouns ------------------------------------------
		  (((np-type common)
		    (head ((cat noun)))
		    (pattern (determiner dots head dots))
		    (determiner ((cat det)
				 (definite {^ ^ definite}))))
		   ;; Proper nouns -------------------------------------------
		   ((np-type proper)
		    (head ((cat noun)))
		    (pattern (head))
		    (definite yes)
		    (determiner none)
		    (describer none)
		    (qualifier none))))

	     ;; NUMBER ====================================================
	     (number ((alt (singular plural))))

	     ;; DESCRIBER =================================================
	     (alt (((describer none))
		   ((pattern (dots describer head dots))
		    (describer
		     ((alt (:index cat)
			   (((cat adj))
			    ((cat verb)
			     (ending
			      ((alt (past-participle present-participle))))))))))))

	     ;; QUALIFIER ==================================================
	     (alt (((qualifier none))
		   ((pattern (dots head qualifier))
		    (qualifier ((cat pp))))))
	     )

	    ;; ==============================================================
	    ;; 04 CAT PP : for prepositional phrases ------------------------
	    ;; ==============================================================
	    ((cat pp)
	     (pattern (prep np))
	     (prep ((cat prep) (lex given)))
	     (np ((cat np))))

	    ;;==============================================================
	    ;; 05 CAT DET : for articles -----------------------------------
	    ;;==============================================================
	    ((cat det)
	     (number {^ ^ number})
	     (alt definite (:index definite)
		  (((definite yes)
		    (lex "the"))
		   ((definite no)
		    (opt ((number singular)
			  (lex "a")))))))

	    ((cat adj))
	    ((cat noun))
	    ((cat verb))
	    ))))

  (format t "~%(gr3) installed.~%")
  (values))


