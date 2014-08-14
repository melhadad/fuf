;;; -*- Mode:Lisp; Syntax:Common-Lisp; Package: -*-
;;; ------------------------------------------------------------
;;; File        : GR-MODULAR.Lisp
;;; Description : Update of gr.l:
;;;               For advisor system using def-alt.
;;; Author      : Michael Elhadad
;;; Created     : 18 Dec 91
;;; Modified    : 16 Jul 92 - added NP subtleties (partitive, measure...)
;;;               19 Aug 92 - added extended NP categories (JR)
;;;               ?? Nov 92 - added date and addresses (JR)
;;;               18 Jan 93 - removed semrs & kinds (JR)
;;;               21 Dec 93 - added np-propagate in NP (merged in 2.2)
;;;                5 Jul 95 - SURGE 2.2 VERSION
;;;                           added adv-p
;;;               05 May 96 - added (case objective) to np in (cat pp) (Yael)
;;;               24 Sep 96 - added call to date and address in np.
;;;               31 Mar 97 - added support for comparative/superlative (Yael)
;;; ============================================================

(in-package "FUG5")


;; NOTE: all non-implemented features, or things to do are marked in
;; comments with a ***** sign.


;; ============================================================
;; GRAMMAR
;; ============================================================

(defun surface (fd)
  (uni fd
       :grammar (gr)
       :cset-attribute 'cset
       :limit 2000))

(defun surface-fd (fd)
  (uni-fd fd
       :grammar (gr)
       :cset-attribute 'cset
       :limit 2000))

(defvar *realize-grammar* nil
  "A functional unification grammar for syntactic realization.")

(def-grammar gr ()
  ;; These are completely determined by the morphology features
  ;; they are not unified with the grammar
  (register-categories-not-unified
   'cat
   '(verb punctuation conj relpro modal))

  (clear-bk-class)
  ;; (define-bk-class 'dative-move 'dative-move)

  (setf *any-at-unification* nil)

  (reset-surge-types)

  (setq *realize-grammar*
	'((alt top-level-cat (:wait {^ cat})
	       ;;==============================================================
	       ;; 01 CAT DS : discourse segment -------------------------------
	       ;;==============================================================
	       ;; A structured paragraph with directive/subordinate and optional
	       ;; connective between them (can be recursive).
	       (((cat #(under ds))
		 (alt simple (:index simple)
		      ;; simple DS: only propositional content in the whole segment.
		      (((simple yes)
			(directive none)
			(subordinate none)
			(connective none)
			(cset ((= pc)))
			(pattern (pc)))
		       ((simple no)
			(cset ((= connective directive subordinate)))
			(alt connective
			     (((connective none)
			       (subordinate none)
			       (pattern (directive)))
			      ((connective ((cat connective) (break-sentence no)))
			       (subordinate given)
			       (subordinate ((punctuation ((after ",")))))
			       (alt ds-order (:wait {^ connective order})
				    (:index (connective order))
				    (((connective ((order dcs)))
				      (pattern (directive connective subordinate)))
				     ((connective ((order scd)))
				      (pattern (subordinate connective directive))))))
			      ((connective ((cat connective) (break-sentence yes)))
			       (subordinate given)
			       (pattern (subordinate connective directive))
			       (connective ((punctuation ((capitalize yes)))))
			       (subordinate ((punctuation ((after "."))))))
			      ((connective sentence)
			       (subordinate given)
			       (pattern (subordinate directive))
			       (directive ((punctuation ((capitalize yes)))))
			       (subordinate ((punctuation ((after "."))))))))))))

		;;==============================================================
		;; 02 CAT CONNECTIVE : discourse element -----------------------
		;;==============================================================
		;; For each connective specify preferred ordering Directive/Subord
		((cat #(under connective))
		 (pattern (head))
		 (head ((lex {^2 lex}) (cat conj)))
		 (alt connective-lex (:index (head lex))
		      (((head ((lex "so"))) (order scd))
		       ((head ((lex "and"))) (order scd))
		       ((head ((lex "since"))) (order dcs))
		       ((head ((lex "because"))) (order dcs))
		       ((head ((lex "although"))) (order dcs))
		       ((head ((lex "but"))) (order scd))
		       ((head ((lex "therefore"))) (order scd)))))

		;;==============================================================
		;; 03 CAT CLAUSE -----------------------------------------------
		;;==============================================================
		((:& simple-clause))

		;;==============================================================
		;; 04 CAT VERB-GROUP -------------------------------------------
		;;==============================================================
		((:& simple-verb-group))

		;;==============================================================
		;; 05 CAT NP ---------------------------------------------------
		;;==============================================================
		;; If there is a gap in the constituent, don't do anything at all.
		((cat #(under np))
		 (alt np-gap (:ignore-when gap)
		      (((gap none)
			(alt type-of-np (:index cat)
			     (((:& partitive))
			      ((:& trivial-proper)) ;; For proper nouns quick
			      ((:& measure))
			      ((:& date))
			      ((:& address))
			      ((:& simple-np)))))
		       ((gap #(under yes))
			;; When there is a gap, try to optimize: do only the minimal
			;; treatment required for agreement that could impact on
			;; constituents outside the gapped np.
			(alt type-of-np-gap (:index cat)
			     (((cat #(under partitive)))
			      ((cat #(under trivial-proper)))
			      ((cat #(under measure)))
			      ((cat #(under date)))
			      ((cat #(under address)))
			      ;; Do everything done in conj SIMPLE-NP except the alts
			      ;; affecting subconstituents of the NP which are useless.
			      ;; Still need to put the NP in "canonical form" - ie,
			      ;; the form expected by the rest of the grammar, but
			      ;; is not required in the input (right features under
			      ;; SEMANTICS, SYNTAX, REFERENCE, HEAD).
			      ((cat #(under np))
			       (:& np-propagate)
			       (:! np-type)
			       (:& np-number))))))))

		;;==============================================================
		;; 06 CAT NP-HEAD ----------------------------------------------
		;;==============================================================
		;; Filler for the head of an NP
		((cat np-head)
		 (complex none)
		 (alt np-head (((cat noun) (generic-cat noun))
			       ((:& noun-compound))
			       ((:& measure))
			       ((:& partitive))
			       ((:& person-name))
			       ((:& team-name)))))

		;; ==============================================================
		;; 07 CAT AP : adjectival phrases -------------------------------
		;; ==============================================================
		;; Winograd apendix B p. 539

		;; Adjectives (unmarked, superlative, comparative) can be
		;; used as describers.
		;; The use of superlative in an indefinite noun group calls
		;; for special interpretation or context. "An oldest man" -
		;; in isolation is a bit strange, but  in context as:
		;; Every village has an oldest man.
		;; comparative and unmarked select different forms:
		;; as (unmarked) as...
		;; (comparative) than...
		;; too (unmarked) to...
		;; Intensifiers of Superlative and Comparative: most,
		;; least, more and less.

		;; Quirk

		;; Comparison and intensification
		;;
		;; 1. inflected forms in -er and -est
		;; 2. periphrastic equivalents in 'more' and 'most'
		;; 3. less,least and as degrees of comparison

		;; comparative - comparison between two
		;; superlative - comparison between more than two.[
		;; comparatives are available for adjectives that refer to
		;; a quality that is thought of as having values on a scale.

		;; good-better-best
		;; bad-worse-worst
		;;       further - furthest
		;; far ~ farther - farthest

		;; old is regularly inflected (older,oldest)
		;; but in specialized use (human beings in family
		;; relationships) - elder, eldest (but you cannot say
		;; *elder than.)

		;; changes in spelling:
		;; 1. final base consonants are doubled when preceding
		;; vowel is stressed and spelled with a single letter:
		;; big-bigger-biggest; sad-sadder-saddest.
		;; 2. bases ending in a consonant+y, finaly y changed to i
		;; angry-angrier-angriest.
		;; 3. base ends in a mute -e, dropped before inflectional
		;; suffix:
		;; pure-purer-purest
		;; free-freer-freest

		;; monosyllabic words: freely form comparison by inflection
		;; disyllabic words: mostly can, and can also use
		;; periphrastic forms.
		;; disyllabic participle forms ending in -ing or -ed do not
		;; take inflections.

		;; other adjectives can only take periphrastic forms.

		;; comparatives of adjectives and adverbs, can be
		;; premodified by amplifying intensifiers that
		;; (so)(very) much      better
		;;      (all)  the      sooner
		;;             far      more  careful
		;;           a lot      less  carefully
		;;            lots
		;;
		;; some NP can use as intensifiers (a good/great deal, a
		;; hell of a lot..).
		;; also: downtoners - rather
		;;                  somewhat  better
		;;                    hardly  sooner
		;;                  a little  more ....careful...

		;; inflectional superlative may be premodified by degree
		;; intensifiers: very. If 'very' premodified a superlative,
		;; a determiner is obligatory.
		;; - She put her very best dress.
		;; - They are working the very least they can.

		;; Analytic superlative in not normally premodified by the
		;; intensifier very.
		;; * The very most careful man.

		((cat simple-ap)
		 (complex none)
		 (generic-cat ap)
		 (head ((cat adj)
			;; Superlative: yes/no
			;; Comparative: yes/no
			;; Inflected: yes/no (yes: er/est no: more/most)
			(concept {^ ^ concept})
			(polarity {^ ^ polarity})
                        (comparative {^ ^ comparative}) ;; YD add these
							;; features to the
							;; head
                        (superlative {^ ^ superlative}) ;;
                        (inflected {^ ^ inflected})   ;; can adj be inflected
			(lex {^ ^ lex})))
                 ;; YD superlative and compartive must be mutual exclusive
                 (alt (((comparative no)
                        (superlative no))
                       ((comparative yes)
                        (superlative no)
                        (alt (((inflected no)
                               (intensifier ((cat phrase) (lex "more"))))
                               ((inflected yes)))))
                       ((superlative yes)
                        (comparative no)
                        (alt (((inflected no)
                               (intensifier ((cat phrase) (lex "most"))))
                               ((inflected yes)))))))


                 ;; YD My problem: modifiers (or intensifiers) to
		 ;; superlatives and comparatives
                 ;; can only be: most, least, more and less (and some more...)
                 ;; here I assume a superlative cannot have a classifier.

		 ;; "light blue" (light is the classifier)
		 (alt (((classifier none))
		       ((classifier given)
                        (superlative no)    ;; YD assume a superlative
					    ;; cannot have a classifier.
			(classifier ((cat ((alt (adj #(under np-head)))))
				     (synt-funct classifier))))))
		 ;; "visible in the cutaway view" (qualifier)
		 (alt (((qualifier none))
		       ((qualifier given)
			(qualifier
			 ((alt adj-qualifier-cat
			       (((cat pp))
				;; JR-added 20 Jan 93
				;; difficult to solve
				;; capable of playing
                                ;; glad that you are here
				((cat clause)
				 (mood ((alt (present-participle
					      to-infinitive
					      bound-nominal-declarative))))))))))))

		 ;; modifier is an adverb: can be intensifier detensifier adv-p
                 (alt (((modifier none))
		       ((modifier given)
			(modifier ((cat adv))))))

                 (pattern (modifier classifier intensifier head
				    qualifier)))

		;; ==============================================================
		;; 08 CAT PP : prepositional phrases ----------------------------
		;; ==============================================================
		((cat simple-pp)
		 (complex none)
		 (generic-cat pp)
		 (pattern (prep np))
		 (prep ((cat prep) (lex given)))
		 (np ((cat ((alt (np #(under date)))))
		      (case objective))))

		;;==============================================================
		;; 09 CAT DET : Determiners ------------------------------------
		;;==============================================================
		((:& det))
		((:& fraction))

		;; ==============================================================
		;; 10 CAT LIST : agglutinated heterogeneous constituents --------
		;; ==============================================================
		;; List is for an agglutination of heterogeneous constituents all
		;; playing together the same syntactic function in a larger
		;; constituent.
		;; For example, a list of describers or qualifiers in an NP.
		;; Each element of the list can be of a different cat (unlike
		;; conjunction).
		;; Lists have one main feature:
		;; elements: a list of features in car/cdr form (~ macro is useful).
		;; Just recurse on all elements of the list with no additional
		;; processing
		((cat list)
		 (elements {^ distinct})  ;; for compatibility with complex
		 (cset ((- semr kind)))
		 (alt list
		      (:demo "How many elements are there in the list?")
		      (((elements none))                           ;; 0 elements
		       ((elements ((car given) (cdr none)))        ;; 1 elements
			(first {^  elements car})
			(cset (first))                 ;; to eliminate any others
			(pattern (first)))
		       ((first {^  elements car})      ;; more
			(first given)
			(rest ((cat list)
			       (elements {^ ^ elements cdr})
			       (elements given)))
			(cset (first rest))
			(pattern (first rest))))))

		;; ==============================================================
		;; 11 CAT COMPLEX : apposition and conjunction ------------------
		;; ==============================================================
		((:& complex))

		;; ==============================================================
		;; 12 MISCELLANEOUS CATS REGOGNIZED BY THE GRAMMAR
		;; ==============================================================
		((cat adj))

                ((cat prep)
                 ;; I sit **exactly on** the edge.
                 ;; Examples **just like** the unary ones.
                 ;; Or should it be adv on the whole PP?
                 ;; Movement seems to be stuck to the prep.
                 (alt adv-prep
                      (((adverb none))
                       ((adverb given)
                        (adverb ((cat adv)))
                        (head ((cat prep) (lex {^2 lex})))
                        (pattern (adverb head))))))

		((cat adv)
		 (generic-cat adv)
		 (complex none)
		 (alt adv-type (:index cat)
		      (((cat #(under detensifier))
			(lex ((ralt ("quite" "pretty" "rather" "somehow")))))
		       ((cat #(under intensifier))
			(lex ((ralt ("very" "extremely")))))
		       ((cat #(under adv-p))
			(pattern (head compl))
			(cset (compl))
			(head ((cat adv))))
		       ((cat adv)))))

		;; ==============================================================
		;; 13 MISCELLANEOUS CATEGORIES IGNORED BY THE GRAMMAR AND RECOGNIZED
		;; BY THE LINEARIZER.
		;; ==============================================================

		((cat phrase))
		((cat article))
		((cat pronoun))

		((cat simple-cardinal)
		 (complex none)
		 ;; Stylistic rule: numbers less than 10 in letters, others in digits.
		 (alt cardinal-value
		      (((value given)
			(control (and (integerp #@{^ value})
				      (> #@{^ value} 0)
				      (< #@{^ value} 11)))
			(digit no))
		       ((digit yes))
		       ((digit #(under roman))))))

		((cat simple-ordinal)
		 (complex none)
		 (alt (((value given)
			(control (and (integerp #@{^ value})
				      (> #@{^ value} 0)
				      (< #@{^ value} 11)))
			(digit no))
		       ((value +)
			(lex ((ralt ("next" "following" "subsequent")))))
		       ((value -)
			(lex ((ralt ("preceding")))))
		       ((value <>)
			(lex "other"))
		       ((value last)
			(lex "last"))
		       ((digit yes))
		       ((digit #(under roman))))))

		;; Compounds added on 4/24/93 to make "straight", "consecutive" and
		;; "in a row" complements of cardinals & ordinals
		;; NOTE: Too complicated to do conjunctions of these
		;; like "10 straight and 5 in a row"
		((cat compound-cardinal)
		 (value {^ numeral value})
		 (numeral ((cat cardinal)))
		 (alt compound-cardinal-cat
		   (((complement ((cat adj)))
		     (pattern (numeral complement)))
		    ((complement ((cat pp)))
		     (pattern (numeral))
		     ({^}
		      ((alt compound-cardinal-matrix-cat
			 (((cat #(under common))
			   (pattern (dots head {^ cardinal complement} dots)))
			  ((cat #(under measure))
			   (pattern (dots unit {^ quantity complement} dots)))))))))))

		((cat compound-ordinal)
		 (value {^ numeral value})
		 (numeral ((cat ordinal)))
		 (alt compound-ordinal-cat
		   (((complement ((cat adj)))
		     (pattern (numeral complement)))
		    ((complement ((cat pp)))
		     (pattern (numeral))
		     ({^} ((alt compound-ordinal-matrix-cat
			     (((cat #(under common))
			       (pattern (dots head {^ ordinal complement} dots)))
			      ((pattern (dots unit {^ quantity complement} dots))
			       (cat #(under measure)))))))))))

		;; ==============================================================
		;; 14 MULTI-DOMAIN SPECIALIZED CONSTRUCTS: addresses, dates etc
		;; ==============================================================
		;; Address is now a type of NP (Nov 5 95)
		;; ((:& address))

		;; Date is now a type of NP (Nov 5 95)
		;; ((:& date))


		;; ============================================================
		;; 15 DOMAIN DEPENDENT CATEGORIES.
		;; ============================================================
		;; NBA domain

		((:& score)))))))

(gr)


;; ============================================================
(provide "gr-modular")
;; ============================================================
