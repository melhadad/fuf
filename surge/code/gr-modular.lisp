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
;;;               31 Aug 14 - completed support for comp/superlative
;;; ============================================================
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


;; NOTE: all non-implemented features, or things to do are marked in
;; comments with a ***** or @@@ sign.


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

  (setq
   *realize-grammar*
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
       ;; superlative - comparison between more than two.
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

       ;; Controversial:
       ;; When used as clause-level constituent, use "the"
       ;; for superlative:
       ;; He is the tallest of all men.
       ;; The tallest led the group. (As opposed to a gapped nominal head)
       ((cat simple-ap)
        (complex none)
        (generic-cat ap)
        #+ignore(fset (cat generic-cat lex
                   concept polarity comparative superlative
                   inflected intensifier as the
                   head classifier qualifier modifier comparison
                   ;; from embedding phrase:
                   question-pronoun synt-funct))
        (head ((cat adj)
               ;; Superlative: yes/no
               ;; Comparative: yes/no
               ;; Inflected: yes/no (yes: er/est no: more/most)
               (concept {^ ^ concept})
               (polarity {^ ^ polarity})
               (comparative {^ ^ comparative})
               (superlative {^ ^ superlative})
               (inflected {^ ^ inflected})
               (lex {^ ^ lex})))

        ;; Superlative and compartive are mutually exclusive
        ;; Add intensifier (more / less / most / least)
        ;; if negative or adj not inflected
        (alt ap-comparative-superlative
             (((comparative no)
               (superlative no)
               (as none)
               (the none)
               (polarity none))
              ((comparative yes)
               (the none)
               (superlative no)
               (alt ap-comparative-polarity
                    (((polarity positive)
                      (as none)
                      (alt ap-comparative-pos-inflected
                           (((inflected yes))
                            ((inflected no)
                             (intensifier ((cat phrase) (lex "more")))))))
                     ((polarity #(under negative))
                      (as none)
                      (intensifier ((cat phrase) (lex "less"))))
                     ((polarity #(under equal))
                      (as ((cat phrase) (lex "as")))
                      ))))
              ((superlative yes)
               (comparative no)
               (as none)
               ;; When ap is a describer - no "the" and no comparison
               ;; Describer: the [tallest] man
               ;; Not describer:
               ;; He is [by far the tallest of all men].
               ;; The play [most talked about].
               (alt ap-superlative-the
                    (((synt-funct describer)
                      (the none)
                      (comparison none))
                     ((the ((cat phrase) (lex "the"))))))
               (alt ap-superlative-polarity
                    (((polarity positive)
                      (alt ap-superlative-positive-inflected
                           (((inflected yes)) ;; tallest
                            ((inflected no)
                             (intensifier ((cat phrase) (lex "most")))))))
                     ((polarity #(under negative))
                      (intensifier ((cat phrase) (lex "least")))))))))

        ;; Modifiers (or intensifiers) to superlatives and comparatives
        ;; can only be: most, least, more and less (and some more...)
        ;; here assume a superlative cannot have a classifier.

        ;; "light blue" (light is the classifier)
        (alt ap-classifier
             (((classifier none))
              ((classifier given)
               (superlative no)
               (classifier ((cat ((alt (adj #(under np-head)))))
                            (synt-funct classifier))))))

        ;; "visible in the cutaway view" (qualifier)
        (alt ap-qualifier
             (((qualifier none))
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
        (alt ap-modifier
             (((modifier none))
              ((modifier given)
               (modifier ((cat adv))))))

        ;; Comparison - for comparative and superlative, follow the qualifier
        ;; Use "than" as prep (as opposed to conj across clauses):
        ;; He is taller than her. (Objective = prep)
        ;; He is taller than she is. (Subjective = conj of 2 clauses)
        ;; He is the tallest of all men. (as AP)
        ;; He is as tall as heavy.
        ;; He is as tall as John.
        ;; He is [modifier very much]
        ;;       as
        ;;       [classifier light]
        ;;       [head blue]
        ;;       [qualifier under heavy light]
        ;;       [comparison as the sky].
        (alt ap-comparison
             (((comparison given)
               (alt ap-comparison-comparative
                    (((comparative yes)
                      (alt ap-comparison-as
                           (((polarity ((alt (positive #(under negative)))))
                             (comparison
                              ((cat pp)
                               (prep
                                ((lex ((alt (given "than")))))))))
                            ((polarity #(under equal))
                             (comparison
                              ((cat pp)
                               (prep ((lex "as")))))))))
                     ((superlative yes)
                      (comparison ((cat pp)
                                   (prep ((lex ((alt (given "of")))))))))))
               (pattern (modifier as the classifier intensifier
                                  head qualifier comparison)))
              ((comparison none)
               (pattern (modifier as the classifier intensifier
                                  head qualifier))))))


       ;; ==============================================================
       ;; 08 CAT PP : prepositional phrases ----------------------------
       ;; ==============================================================
       ((cat simple-pp)
        (complex none)
        (generic-cat pp)
        (pattern (prep np))
        (prep ((cat prep) (lex given)))
        (np ((cat ((alt (np #(under date)))))
             (syntax ((case objective))))))

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
        (elements {^ distinct}) ;; for compatibility with complex
        (cset ((- semr kind)))
        (alt list
             (:demo "How many elements are there in the list?")
             (((elements none)) ;; 0 elements
              ((elements ((car given) (cdr none))) ;; 1 elements
               (first {^  elements car})
               (cset (first)) ;; to eliminate any others
               (pattern (first)))
              ((first {^  elements car}) ;; more
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
        (adverb none)
        (pre-comp none)
        (comparative none)
        (orientation none)
        ;; Compute number if not specified in input.
        (alt cardinal-number
             (((number given))
              ((value given)
               (control (and (numberp #@{^ value})
                             (/= #@{^ value} 1)))
	       (number plural))
              ((number singular))))
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
       ;;
       ;; 07 Sep 14: comparative and approx cardinal
       ;; (comparative [none/bound/comparative])
       ;; (orientation [none/+/-/=])
       ;; (adverb [none/adv])
       ;; bound: at least/most 5
       ;;        over/under 5
       ;; comparative: [adv] more/less than 5
       ;;              [adv] as many as 5
       ;; approx: [adv] 5 (exactly 5, roughly 5)
       ;;
       ;; Recursive structure (must check cooccurrence restrictions)
       ;; - Not much more than approximately 100
       ;; - Vastly less than under 100
       ;; - [At least more than 100 ?]
       ;; - [Under more than 100 ?]
       ;; - More than about 100
       ;;   [About more than 100?]
       ;;
       ((cat compound-cardinal)
        (value {^ numeral value})
        (digit {^ numeral digit})
        (number {^ numeral number})
        (numeral ((cat ((alt (cardinal compound-cardinal))))))
        (alt cardinal-number
             (((number given))
              ((value given)
               (control (and (numberp #@{^ value})
                             (/= #@{^ value} 1)))
               (number plural))
              ((numeral ((cat compound-cardinal)))
               (number {^ numeral number}))
              ((number singular))))
        (alt compound-cardinal-comp
             (((comparative none)
               (orientation none)
               (pre-comp none))
              ((pre-comp given)
               (pre-comp ((cat phrase))))
              ((comparative #(under bound))
               (alt compound-comparative-bound
                    (((orientation -)
                      (pre-comp ((cat phrase)
                                 (lex ((alt ("at most" "under")))))))
                     ((orientation =)
                      (pre-comp ((cat phrase)
                                 (lex ((alt ("as many as" "as few as")))))))
                     ((orientation +)
                      (pre-comp ((cat phrase)
                                 (lex ((alt ("at least" "over"))))))))))
              ((comparative #(under comparative))
               ;; comparative accept adverb
               (alt compound-comparative-comparative
                    (((orientation +)
                      (pre-comp ((cat phrase)
                                 (lex "more than"))))
                     ((orientation -)
                      (pre-comp ((cat phrase)
                                 (lex "less than")))))))))
        (alt compound-comparative-adv
             (((adverb none))
              ((adverb given)
               (adverb ((cat adv))))))
        (alt compound-cardinal-cat
             (((complement none)
               (pattern (adverb pre-comp numeral)))
              ((complement ((cat adj)))
               (pattern (adverb pre-comp numeral complement)))
              ((complement ((cat pp)))
               (pattern (adverb pre-comp numeral))
               ({^}
                ((alt compound-cardinal-matrix-cat
                      (((cat #(under common))
                        (pattern (dots head {^ cardinal complement} dots)))
                       ((cat #(under measure))
                        (pattern (dots unit {^ quantity complement} dots)))))))
               ))))

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
