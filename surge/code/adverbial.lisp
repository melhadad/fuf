;;; -*- Mode:Lisp; Syntax:Common-Lisp; Package:  -*-
;;; -----------------------------------------------------------------------
;;; File:         adverbial.lisp
;;; Description:  New adverbial roles for SURGE:
;;;               predicate-modifiers mapped onto predicate-adjuncts,
;;;                circumstantials mapped onto sentence-adjuncts & disjuncts.
;;; Author:       Jacques Robin
;;; Created:      26 Nov 1992
;;; Modified:      5 Jul 1995: SURGE 2.2 VERSION
;;;               22 Apr 1996: Added matter to fset of predicate-modifiers
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

;;=============================================================================
;; INVENTORY AND RELATIVE ORDERING OF ADVERBIAL ROLES
;;=============================================================================
;;
(def-alt old-circum
  (((circum none))
   ((circum given)
    (:! from-loc)
    (:! to-loc)
    (:! on-loc)
    (:! in-loc)
    (:! at-loc))))

;; If you add a predicate modifier to the grammar, add its name here too so
;; that the binding module will know how to look for it in the total fd.
;; Assumption: all pred-modifs appear under the feature (pred-modif ...)
;; within clauses.
(defun get-pred-modifiers ()
  '(score manner means instrument comparison matter direction distance
	  location path origin destination duration time))

(def-alt predicate-modifiers
  (((pred-modif none))
   ((pred-modif given)
    (pred-modif
     ((fset (score manner means instrument comparison matter direction
		   distance location path origin destination duration time))))

    ;; Watch out!
    ;; The order of the following alts is MEANINGFUL. It corresponds to
    ;; the default relative sentential ordering of the predicate adjuncts
    ;; realizing the corresponding semantic roles.
    (:! score)
    (:! manner-pred-modif)
    (:! means-pred-modif)
    (:! instrument-pred-modif)
    (:! comparison-pred-modif)
    (:! matter-pred-modif)
    (:! direction-pred-modif)
    (:! distance-pred-modif)
    (:! location-pred-modif)
    (:! path-pred-modif)
    (:! origin-pred-modif)
    (:! destination-pred-modif)
    (:! time-pred-modif)
    (:! duration-pred-modif))))

(def-alt circumstantials
  (((circum none))
   ((circum given)
    (circum ((fset (location distance origin time duration frequency
		    co-event reason result purpose behalf
                    condition concessive-condition concession contrast
		    exception inclusion substitution addition accompaniment
                    opposition manner means comparison matter standard
                    perspective to-loc from-loc at-loc on-loc in-loc))))

    ;; Watch out!
    ;; The order of the following alts is MEANINGFUL. It corresponds to
    ;; the default relative sentential ordering of the movable syntactic
    ;; constituents when they co-occur in the same position.
    (:! manner-circum)
    (:! accompaniment-circum)
    (:! opposition-circum)
    (:! matter-circum)
    (:! standard-circum)
    (:! perspective-circum)
    (:! behalf-circum)
    (:! distance-circum)
    (:! location-circum)
    ;; (:! result-circum)
    (:! duration-circum)
    (:! frequency-circum)
    (:! time-circum)
    (:! means-circum)
    (:! reason-circum)
    (:! purpose-circum)
    (:! addition-circum)
    (:! comparison-circum)
    (:! concession-circum)
    (:! contrast-circum)
    (:! exception-circum)
    (:! substitution-circum)
    (:! condition-circum)
    (:! concessive-condition-circum)
    (:! inclusion-circum)
    (:! co-event-circum)
    (:! result-circum)

    ;; Add punctuation to the header and its immediately following constituent
    (:! header))))


(def-conj relaters
  (relaters ((fset (time cond))))
  (:! time-relater)
  (:! cond-relater))

(def-alt time-relater (:demo "Is there a time-relater?")
  ;; Time-relaters = fronted "first", "next", "then" etc.
  (((relaters ((time none))))
   ((relaters ((time given)))
    (front-adverbial-1 {^ relaters time})
    (relaters ((time ((cat adv)
		      (synt-funct conjunct)
		      (punctuation ((after ","))))))))))

(def-alt cond-relater (:demo "Is there a cond-relater?")
  ;; Cond-relaters = fronted "then", "otherwise", "else" etc.
  (((relaters ((cond none))))
   ((relaters ((cond given)))
    (front-adverbial-2 {^ relaters cond})
    (relaters ((cond ((cat adv)
		      (synt-funct conjunct))))))))


(defmacro make-map-pred-modif (sem-role)
  (let ((path (make-path :l (list '^ 'pred-modif sem-role))))
    `(def-alt ,(read-from-string
		(concatenate 'string "map-" (string sem-role) "-pred-adjunct"))
       (((end-adverbial-4 given) (end-adverbial-5 ,path))
	((end-adverbial-3 given) (end-adverbial-4 ,path))
	((end-adverbial-2 given) (end-adverbial-3 ,path))
	((end-adverbial-1 given) (end-adverbial-2 ,path))
	((end-adverbial-1 ,path))))))


(defmacro make-pred-modif (sem-role)
  (let ((qrel-conj (read-from-string
		    (concatenate 'string (string sem-role) "-qrel")))
	(map-alt (read-from-string
		  (concatenate 'string
                               "map-" (string sem-role) "-pred-adjunct")))
	(cat-alt (read-from-string
		  (concatenate 'string
                               "cat-" (string sem-role) "-pred-adjunct"))))
    `(def-alt ,(read-from-string
		(concatenate 'string
                             (string sem-role) "-pred-modif"))
       (:demo ,(concatenate 'string
                            "Is there a " (string sem-role)
                            " predicate-modifier?"))
       (((pred-modif ((,sem-role none))))
	((pred-modif ((,sem-role given)
		      (,sem-role ((:& ,qrel-conj)
				  (synt-funct pred-adjunct)
				  (:! ,cat-alt)))))
         (cset ((+ ,(make-path :l (list '^ 'pred-modif sem-role)))))
	 (:! ,map-alt))))))


(defmacro make-map-circum (sem-role synt-funct)
  (let ((path (make-path :l (list '^ 'circum sem-role))))
    `(def-alt ,(read-from-string
		(concatenate 'string "map-" (string sem-role)
			             "-" (string synt-funct)))
       (((circum ((,sem-role ((position given)))))
	 (alt circum-position-given (:index ,(path-append path '{position}))
	   (((circum ((,sem-role ((position front)
				  (opt ((punctuation ((after ",")))))))))
	     (alt front-circum
                  (((front-adverbial-1 given) (front-adverbial-2 ,path))
                   ((front-adverbial-1 ,path)))))
	    ((circum ((,sem-role
                       ((position end)
                        (opt ((synt-funct #(under disjunct))
                              (punctuation ((before ",")))))))))
	     (alt end-circum (((end-adverbial-4 given) (end-adverbial-5 ,path))
			      ((end-adverbial-3 given) (end-adverbial-4 ,path))
			      ((end-adverbial-2 given) (end-adverbial-3 ,path))
			      ((end-adverbial-1 given) (end-adverbial-2 ,path))
			      ((end-adverbial-1 ,path)))))
	    ((circum ((,sem-role ((position header)
				  (cat ((alt header-cat (address date))))))))
	     (alt header-circum
                  (((headers ((1 given)))
                    (headers ((2 ,(path-append '{^} path))
                              (2 ((punctuation ((before ","))))))))
                   ((headers ((1 ,(path-append '{^} path)))))))))))
	((alt circum-position-unspecified
           (((end-adverbial-5 given)
	     (front-adverbial-2 ,path)
	     (opt ((circum ((,sem-role ((punctuation ((after ","))))))))))
	    ((end-adverbial-4 given)
	     (front-adverbial-1 given)
	     (end-adverbial-5 ,path)
	     (opt ((circum ((,sem-role
                             ((synt-funct #(under disjunct))
                              (punctuation ((before ","))))))))))
	    ((end-adverbial-4 given)
	     (front-adverbial-1 ,path)
	     (opt ((circum ((,sem-role ((punctuation ((after ","))))))))))
	    ((end-adverbial-3 given)
	     (front-adverbial-1 given)
	     (opt ((circum ((,sem-role
                             ((synt-funct #(under disjunct))
                              (punctuation ((before ",")))))))))
	     (end-adverbial-4 ,path))
	    ((end-adverbial-3 given)
	     (front-adverbial-1 ,path)
	     (opt ((circum ((,sem-role ((punctuation ((after ","))))))))))
	    ((end-adverbial-2 given)
	     (front-adverbial-1 given)
	     (opt ((circum ((,sem-role
                             ((synt-funct #(under disjunct))
                              (punctuation ((before ",")))))))))
	     (end-adverbial-3 ,path))
	    ((end-adverbial-2 given)
	     (front-adverbial-1 ,path)
	     (opt ((circum ((,sem-role
                             ((punctuation ((after ","))))))))))
	    ((end-adverbial-1 given)
	     (front-adverbial-1 given)
	     (opt ((circum ((,sem-role
                             ((synt-funct #(under disjunct))
                              (punctuation ((before ",")))))))))
	     (end-adverbial-2 ,path))
	    ((end-adverbial-1 given)
	     (front-adverbial-1 ,path)
	     (opt ((circum ((,sem-role
                             ((punctuation ((after ","))))))))))
	    ((end-adverbial-1 ,path)
	     (opt ((circum ((,sem-role
                             ((synt-funct #(under disjunct))
                              (punctuation ((before ",")))))))))))))))))


(defmacro make-sent-adjunct (sem-role)
  (let ((qrel-conj (read-from-string
		    (concatenate 'string (string sem-role) "-qrel")))
	(map-alt (read-from-string
		  (concatenate 'string
                               "map-" (string sem-role) "-sent-adjunct")))
	(cat-alt (read-from-string
		  (concatenate 'string
                               "cat-" (string sem-role) "-sent-adjunct"))))
    `(def-alt ,(read-from-string
		(concatenate 'string (string sem-role) "-circum"))
       (:demo ,(concatenate 'string "Is there a "
			    (string sem-role)
			    " circumstantial?"))
       (((circum ((,sem-role none))))
	((circum ((,sem-role given)
		  (,sem-role ((:& ,qrel-conj)
			      (synt-funct sent-adjunct)
			      (:! ,cat-alt)))))
	 (:! ,map-alt))))))


(defmacro make-disjunct (sem-role)
  (let ((qrel-conj (read-from-string
		    (concatenate 'string (string sem-role) "-qrel")))
	(map-alt (read-from-string
		  (concatenate 'string
                               "map-" (string sem-role) "-disjunct")))
	(cat-alt (read-from-string
		  (concatenate 'string
                               "cat-" (string sem-role) "-disjunct"))))
    `(def-alt ,(read-from-string
                (concatenate 'string (string sem-role) "-circum"))
       (:demo ,(concatenate 'string "Is there a "
			    (string sem-role)
			    " circumstantial?"))
       (((circum ((,sem-role none))))
	((circum ((,sem-role given)
		  (,sem-role ((synt-funct disjunct)
			      (:! ,cat-alt)))))
	 (:! ,map-alt))))))


(defmacro make-final-circum (sem-role)
  (let ((path (make-path :l (list '^ 'circum sem-role)))
	(qrel-conj (read-from-string
		    (concatenate 'string (string sem-role) "-qrel")))
	(map-alt (read-from-string
		  (concatenate 'string
                               "map-" (string sem-role) "-disjunct")))
	(cat-alt (read-from-string
		  (concatenate 'string
                               "cat-" (string sem-role) "-disjunct"))))
    `(def-alt ,(read-from-string
                (concatenate 'string (string sem-role) "-circum"))
       (:demo ,(concatenate 'string "Is there a "
			    (string sem-role)
			    " circumstantial?"))
       (((circum ((,sem-role none))))
	((circum ((,sem-role given)
		  (,sem-role ((synt-funct disjunct)
			      (opt ((punctuation ((before ",")))))
			      (:! ,cat-alt)))))
	 (final-adverbial ,path))))))


;; Macro to define stereotypical conjs grouping the features concerning
;; questions and relatives
(defmacro make-qrel (sem-role &key (q-prep 'none)
			           (q-pron 'none)
             			   (rel-mark 'none)
				   (q-embedded 'yes)
				   (rel-embedded 'yes))
  `(def-conj ,(read-from-string
               (concatenate 'string (string sem-role) "-qrel"))
     (question-prep ((lex ((alt (given ,q-prep))))))
     (question-pronoun ((lex ((alt (given ,q-pron))))))
     (relative-marker ((lex ((alt (given ,rel-mark))))))
     (question-embedded ((alt (given ,q-embedded))))
     (relative-embedded ((alt (given ,rel-embedded))))))


;; Putting punctuation in the header and capitalize the first constituent
;; after it.
(def-alt header
  (((headers none))
   ((headers given)
    (headers ((fset (1 2))))
    (alt last-header
      (((headers ((2 given)))
	(headers ((2 ((punctuation ((after "--"))))))))
       ((headers ((1 given)))
	(headers ((1 ((punctuation ((after "--"))))))))))
    (alt next-to-header
      (((front-adverbial-1 given)
	(front-adverbial-1 ((punctuation ((capitalize yes))))))
       ((complex none)
	(synt-roles ((subject given)
		     (subject ((punctuation ((capitalize yes))))))))
       ((complex given)
	(alt complex-next-to-header (:wait {^ distinct car synt-roles})
	  (((distinct
	     ((car
	       ((synt-roles
		 ((subject given)
		  (subject ((punctuation ((capitalize yes))))))))))))
	   ((distinct
	     ((car
	       ((distinct
		 ((car
		   ((synt-roles
		     ((subject given)
		      (subject ((punctuation
                                 ((capitalize yes))))))))))))))))))))))))




;;=============================================================================
;; LOCATION predicate modifier, e.g., "Bo kissed her ON THE CHEEK."
;;=============================================================================
;;
(make-pred-modif location)

(def-alt cat-location-pred-adjunct (:index cat)
  (((cat pp)
    (ALT (((prep ((lex given))))
	  ((distinct ((car ((prep ((lex given)))))))))))
   ((cat adv))
   ((cat clause)
    (mood ((alt location-mood (bound-adverbial verbless))))
    (binder ((lex ((alt (given "where")))))))))

(make-map-pred-modif location)

(make-qrel location
           :q-pron "where" :rel-mark "where" :q-embedded no :rel-embedded no)


;;=============================================================================
;; LOCATION circumstantial, e.g., "ON THE PLATFORM, Bo kissed her."
;;=============================================================================
;;
(make-sent-adjunct location)

(def-alt cat-location-sent-adjunct (:index cat)
  (((cat pp)
    (ALT (((prep ((lex given))))
	  ((distinct ((car ((prep ((lex given)))))))))))
   ((cat adv))
   ((cat address))))

(make-map-circum location sent-adjunct)


;; Note old formalism roles can't be scoped or controlled.
(def-alt on-loc (:demo "Is there a on-loc role (old input formalism)?")
  (((circum ((on-loc none))))
   ((circum ((on-loc given)
	     (location ((cat pp)
			(prep ((lex "on")))
			(np {^2 on-loc})))))
    (cset ((- {^ circum on-loc}))))))

(def-alt in-loc (:demo "Is there a in-loc role (old input formalism)?")
  (((circum ((in-loc none))))
   ((circum ((in-loc given)
	     (location ((cat pp)
			(prep ((lex "in")))
			(np {^2 in-loc})))))
    (cset ((- {^ circum in-loc}))))))

(def-alt at-loc (:demo "Is there a at-loc role (old input formalism)?")
  (((circum ((at-loc none))))
   ((circum ((at-loc given)
	     (location ((cat pp)
			(prep ((lex "at")))
			(np {^2 at-loc})))))
    (cset ((- {^ circum at-loc}))))))


;;=============================================================================
;; ORIGIN predicate modifier, e.g., "Bo called her FROM KANSAS CITY."
;;=============================================================================
;;
(make-pred-modif origin)

(def-alt cat-origin-pred-adjunct
    (((cat pp) (prep ((lex ((alt (given "from")))))))))

(make-map-pred-modif origin)

(make-qrel origin :q-prep "from" :q-pron "where" :rel-mark "which")

;; Note old formalism roles can't be scoped or controlled.
(def-alt from-loc (:demo "Is there a from-loc role (old input formalism)?")
  (((circum ((from-loc none))))
   ((circum ((from-loc given)))
    (pred-modif ((origin ((cat pp)
			  (np {^3 circum from-loc})))))
    (cset ((- {^ circum from-loc}))))))


;;=============================================================================
;; DESTINATION predicate modifier, e.g., "Bo sent a letter TO LOS ANGELES."
;;=============================================================================
;;
(make-pred-modif destination)

(def-alt cat-destination-pred-adjunct (:index cat)
  (((cat pp) (prep ((lex ((alt (given "to")))))))
   ((cat adv))
   ((cat clause)
    (mood bound-adverbial)
    (binder ((lex ((alt (given "where")))))))))

(make-map-pred-modif destination)

(make-qrel destination :q-prep "to" :q-pron "where" :rel-mark "which")

;; Note old formalism roles can't be scoped or controlled.
(def-alt to-loc (:demo "Is there a to-loc role (old input formalism)?")
  (((circum ((to-loc none))))
   ((circum ((to-loc given)))
    (pred-modif ((destination ((cat pp)
			       (np {^3 circum to-loc})))))
    (cset ((- {^ circum to-loc}))))))


;;=============================================================================
;; DIRECTION predicate modifier, e.g., "Bo slid DOWN."
;;=============================================================================
;;
(make-pred-modif direction)

(def-alt cat-direction-pred-adjunct (:index cat)
  (((cat pp) (prep ((lex ((alt (given "towards")))))))
   ((cat adv))
   ((cat common))))

(make-map-pred-modif direction)

(make-qrel direction :q-prep "towards" :q-pron "where" :rel-mark "which")


;;=============================================================================
;; PATH predicate modifier, e.g., "Bo came VIA DENVER."
;;=============================================================================
;;
(make-pred-modif path)

(def-alt cat-path-pred-adjunct
    (((cat pp) (prep ((lex ((alt (given "via")))))))))

(make-map-pred-modif path)

(make-qrel path :q-prep "towards" :q-pron "where" :rel-mark "which")


;;=============================================================================
;; DISTANCE predicate modifier, e.g., "You came A LONG WAY."
;;=============================================================================
;;
(make-pred-modif distance)

(def-alt cat-distance-pred-adjunct (((cat ((alt (measure common)))))))

(make-map-pred-modif distance)

(make-qrel distance :q-pron "how far" :q-embedded no :rel-embedded no)


;;=============================================================================
;; DISTANCE circumstantial, e.g., "ALL ACROSS THE COUNTRY, Tony biked."
;;=============================================================================
;;
(make-sent-adjunct distance)

(def-alt cat-distance-sent-adjunct (((cat pp)
				     (prep ((lex ((alt (given "for"))))))
				     (np ((cat ((alt (common measure)))))))))

(make-map-circum distance sent-adjunct)


;;=============================================================================
;; TIME adverbial, e.g., "LAST YEAR, Bo triumphed."
;;=============================================================================
;;
(make-sent-adjunct time)

(def-alt cat-time-sent-adjunct (:index cat)
  (((cat pp)
    (alt time-circum-pp
         (((prep ((lex given))))
          ((complex given)))))
   ((cat np))
   ((cat adv))
   ((cat date))
   ((cat clause)
    (alt time-circum-clause
         (((mood
            ((alt time-mood
                  (bound-adverbial present-participle
                                   past-participle verbless))))
           (binder ((lex ((alt (given "when")))))))
          ((complex given)))))
   ((cat list))))

(make-map-circum time sent-adjunct)

(make-qrel time
           :q-pron "when" :rel-mark "when" :q-embedded no :rel-embedded no)

;;=============================================================================
;; TIME pred-modif, e.g., "You filed LATE."
;; NOTE: There is a time pred-modif and a time adjunct (circum).
;;       "In 1986, you filed late."
;;=============================================================================
;;
(make-pred-modif time)

(def-alt cat-time-pred-adjunct (:index cat)
  (((cat pp)
    (alt time-circum-pp
         (((prep ((lex given))))
          ((complex given)))))
   ((cat np))
   ((cat adv))
   ((cat date))
   ((cat clause)
    (alt time-circum-clause
         (((mood
            ((alt time-mood
                  (bound-adverbial present-participle
                                   past-participle verbless))))
           (binder ((lex ((alt (given "when")))))))
          ((complex given)))))
   ((cat list))))

(make-map-pred-modif time)

;;=============================================================================
;; DURATION predicate modifier, e.g., "Bo stayed there FOREVER."
;;=============================================================================
;;
(make-pred-modif duration)

(def-alt cat-duration-pred-adjunct (:index cat)
  (((cat common)) ((cat measure)) ((cat adv))))

(make-map-pred-modif duration)
(make-qrel duration :q-prep "for" :q-pron "how long" :rel-mark "during which")

;;=============================================================================
;; DURATION adjunct (There is a duration adjunct and a duration pred-modif).
;;=============================================================================

(make-sent-adjunct duration)

(def-alt cat-duration-sent-adjunct (:index cat)
  (((cat pp)
    (alt cat-duration-pp
         (((prep ((lex ((alt (given "for"))))))
           (np ((cat ((alt (common measure)))))))
          ((complex given)))))
   ((cat clause)
    (alt cat-duration-clause
         (((mood ((alt duration-mood
                       (bound-adverbial present-participle
                                        past-participle verbless))))
           (binder ((lex given))))
          ((complex given)))))))

(make-map-circum duration sent-adjunct)


;;=============================================================================
;; FREQUENCY adverbial, e.g., "OFTEN, Bo suffered from injury."
;;=============================================================================
;;
(make-sent-adjunct frequency)

(def-alt cat-frequency-sent-adjunct (:index cat)
  (((cat pp) (prep ((lex given))))
   ((cat np))
   ((cat adv))))

(make-map-circum frequency sent-adjunct)
(make-qrel frequency :q-pron "how often" :rel-mark "at which"
                     :q-embedded no :rel-embedded no)


;;====================------===================================================
;; CO-EVENT adverbial e.g.,
;; "GIVING THE LAKERS THEIR FIRST WIN, Magic had 14 assists."
;;=============================================================================
;;
(make-disjunct co-event)

(def-alt cat-co-event-disjunct
  (((cat clause)
    (alt co-event-habitual (:index habitual)
      (((habitual none)
	(mood ((alt co-event-mood
                    (present-participle past-participle verbless))))
	(binder ((alt co-event-binder (none ((lex given)))))))
       ((habitual #(under yes))
	(mood ((alt co-event-habitual-mood (bound-adverbial verbless))))
	(binder ((lex ((alt (given "whenever"))))))))))))

(make-map-circum co-event disjunct)


;;=============================================================================
;; REASON adverbial e.g., "BECAUSE OF HIS INJURY, Bo did not play."
;;=============================================================================
;;
(def-alt reason-circum (:demo "Is there a reason adverbial?")
  (((circum ((reason none))))
   ((circum ((reason given)))
    (alt since-because
	(
	 ;; By default map it to a sentence-adjunct bound by "because"
	 ((circum
           ((reason ((:& reason-qrel)))
            (reason ((alt because-cat
                          (((cat clause)
                            (mood bound-adverbial)
                            (binder ((lex ((alt (given "because")))))))
                           ((cat pp)
                            (prep ((lex ((alt (given "because of")))))))))))))
	  (:! map-reason-sent-adjunct))

	 ;; But if it is a clause bound by "since" then map it to a disjunct
	 ((circum
           ((reason ((cat clause)
                     (mood bound-adverbial)
                     (binder ((cat #(under conj)) (lex #(under since))))))))
	  (:! map-reason-disjunct)))))))

(make-map-circum reason sent-adjunct)
(make-map-circum reason disjunct)

(make-qrel reason
           :q-pron "why" :rel-mark "why" :q-embedded no :rel-embedded no)


;;=========================-===================================================
;; RESULT adverbial e.g.,
;; "Detroit waived Bo, TO SEE HIM FLOURISH WITH PHOENIX."
;;=============================================================================
;;
(make-final-circum result)

(def-alt cat-result-disjunct
  (((cat clause)
    (alt result-mood (((mood bound-adverbial)
		       (binder ((lex ((alt (given "so")))))))
		      ((mood present-participle))
		      ((mood to-infinitive)
		       (binder none)))))))


;;=========================-===================================================
;; OPPOSITION adverbial e.g., "AGAINST THE KNICKS, Jordan scored 37 points."
;;=============================================================================
;;
(make-sent-adjunct opposition)

(def-alt cat-opposition-sent-adjunct
  (((cat pp) (prep ((lex ((alt (given "against")))))))))

(make-map-circum opposition sent-adjunct)

(make-qrel opposition :q-pron "who" :q-prep "against" :rel-mark "whom")


;;=========================-===================================================
;; ADDITION adverbial e.g., "They traded Smith AS WELL AS KIMBLE."
;;=============================================================================
;;
(def-alt addition-circum (:demo "Is there a addition adverbial?")
  (((circum ((addition none))))
   ((circum ((addition given)))
    (alt addition-cat
	(
	 ;; If it is a clause then map it as a sentence adjunct
	 ((circum ((addition ((cat clause)
			      (mood present-participle)
			      (binder ((lex ((alt (given "in addition to"))))))
			      (:& addition-qrel)))))
	  (:! map-addition-sent-adjunct))

	 ;; But if it is a PP then map it as a disjunct
	 ((circum ((addition ((cat pp)))))
	  (:! map-addition-disjunct)))))))

(make-map-circum addition disjunct)
(make-map-circum addition sent-adjunct)

(make-qrel addition :q-pron "what" :q-prep "in addition to" :rel-mark "which")


;;====================------===================================================
;; INCLUSION adverbial e.g.,
;; "Bo did everything to help, INCLUDING LENDING MONEY."
;;=============================================================================
;;
(make-disjunct inclusion)

(def-alt cat-inclusion-disjunct (:index cat)
  (((cat pp)
    (prep ((lex ((alt (given "including")))))))
   ((cat clause)
    (binder ((lex ((alt (given "including"))))))
    (mood present-participle))
   ((cat clause)
    (binder ((lex ((alt (given "including"))))))
    (mood verbless))))

(make-map-circum inclusion disjunct)

;;=============================================================================
;; MEANS predicate modifier, e.g., "Bo was treated SURGICALLY."
;;=============================================================================
;;
(make-pred-modif means)

(def-alt cat-means-pred-adjunct (:index cat)
  (((cat adv))
   ((cat pp)
    (np ((determiner none)))
    (prep ((lex ((alt (given "by")))))))))

(make-map-pred-modif means)

(make-qrel means :q-pron "how" :q-embedded no :rel-embedded no)


;;=============================================================================
;; MEANS circumstantial, e.g., "BY ACQUIRING HIM, they completed their roster."
;;=============================================================================
;;
(make-sent-adjunct means)

(def-alt cat-means-sent-adjunct
  (((cat clause)
    (mood ((alt means-mood (present-participle past-participle))))
    (binder ((lex ((alt (given "by")))))))
   ((cat pp)
    (np ((determiner none)))
    (prep ((lex ((alt (given "by")))))))))

(make-map-circum means sent-adjunct)


;;=========================-===================================================
;; INSTRUMENT adverbial e.g., "Bo pushed WITH BOTH HANDS."
;;=============================================================================
;;
(make-pred-modif instrument)

(def-alt cat-instrument-pred-adjunct
  (((cat pp)
    (alt instrument-polarity
	(((instr-polarity +)
	  (prep ((lex ((alt (given "with")))))))
	 ((instr-polarity -)
	  (prep ((lex ((alt (given "without"))))))))))))

(make-map-pred-modif instrument)

(make-qrel instrument :q-pron "how" :rel-mark "with which"
	              :q-embedded no :rel-embedded no)


;;=============================================================================
;; PURPOSE adverbial, e.g., "TO GAIN FREE-AGENCY, Bo sued."
;;=============================================================================
;;
(make-sent-adjunct purpose)

(def-alt cat-purpose-sent-adjunct (:index cat)
  (((cat pp)
    (prep ((lex ((alt (given "for")))))))
   ((cat clause)
    (mood bound-adverbial)
    ;; need to add here modality constraint
    ;; forcing presence of "would"
    (binder ((lex ((alt (given "so")))))))
   ((cat clause)
    (mood ((alt (to-infinitive for-to-infinitive))))
    (binder ((lex ((alt (given "in order" none)))))))))

(make-map-circum purpose sent-adjunct)

(make-qrel purpose :q-pron "why" :q-embedded no :rel-embedded no)


;;=============================================================================
;; BEHALF adverbial, e.g., "FOR THE LAKERS, Magic scored 29 points."
;;=============================================================================
;;
(make-sent-adjunct behalf)

(def-alt cat-behalf-sent-adjunct
    (((cat pp) (prep ((lex ((alt (given "for")))))))))

(make-map-circum behalf sent-adjunct)

(make-qrel behalf :q-pron "who" :q-prep "for" :rel-mark "whom")


;;=============================================================================
;; ACCOMPANIMENT adverbial, e.g., "WITH HER, Bo would go anywhere."
;;=============================================================================
;;
(make-sent-adjunct accompaniment)

(def-alt cat-accompaniment-sent-adjunct
  (((cat pp)
    (alt accompaniment-polarity
	(((accomp-polarity +)
	  (prep ((lex ((alt with (given "with")))))))
	 ((accomp-polarity -)
	  (prep ((lex ((alt without (given "without"))))))))))))

(make-map-circum accompaniment sent-adjunct)

(make-qrel accompaniment :q-pron "who" :q-prep "with" :rel-mark "whom")


;;=============================================================================
;; MANNER predicate modifier, e.g., "Bo kissed her WITH TENDERNESS."
;;=============================================================================
;;
(make-pred-modif manner)

;;;; MODIF 24 Dec 96: Added manner as adverbial as pred-modif as well.
(def-alt cat-manner-pred-adjunct
  (((cat pp) (prep ((lex ((alt (given "with")))))))
   ((cat adv))))

(make-map-pred-modif manner)

(make-qrel manner :q-pron "how" :q-embedded no :rel-embedded no)


;;=============================================================================
;; MANNER circumstantial, e.g., "TENDERLY, Bo kissed her."
;;=============================================================================
;;
(make-sent-adjunct manner)

(def-alt cat-manner-sent-adjunct
  (((cat adv))
   ((cat pp) (prep ((lex ((alt (given "with")))))))))

(make-map-circum manner sent-adjunct)


;;=============================================================================
;; CONDITION adverbial e.g., "IF IN SHAPE, Bo will be unstoppable."
;;=============================================================================
;;
(make-disjunct condition)

(def-alt cat-condition-disjunct
  (((cat clause)
    (mood ((alt condition-mood
	       (bound-adverbial past-participle verbless))))
    (alt cond-polarity (:index polarity)
      (((cond-polarity +)
	(binder ((lex ((alt (given "if")))))))
       ((cond-polarity -)
	(binder ((lex ((alt (given "unless"))))))))))))

;; Note:
;; should distinguish between real, predictive, hypothetical & counterfactual
;; conditions. But that requires to code subjunctive moods for adverbial &
;; represent tense coordination constraints.

(make-map-circum condition disjunct)


;;====================------===================================================
;; CONCESSIVE-CONDITION adverbial e.g., "ALTHOUGH INJURED, Bo started."
;;=============================================================================
;;
(make-disjunct concessive-condition)

(def-alt cat-concessive-condition-disjunct
  (((cat clause)
    (mood
     ((alt concessive-condition-mood
	  (bound-adverbial past-participle present-participle verbless))))
    (binder ((lex ((alt (given "even if")))))))))

;; Note: should distinguish between real, predictive & counterfactual
;; concessive-conditions.

(make-map-circum concessive-condition disjunct)


;;====================------===================================================
;; CONCESSION adverbial e.g., "ALTHOUGH INJURED, Bo started."
;;=============================================================================
;;
(make-disjunct concession)

(def-alt cat-concession-disjunct (:index cat)
  (((cat pp) (prep ((lex ((alt (given "in spite of")))))))
   ((cat adv))    ;; at least
   ((cat clause)
    (mood ((alt concession-mood
	       (bound-adverbial present-participle past-participle verbless))))
    (binder ((lex ((alt (given "although")))))))))

(make-map-circum concession disjunct)


;;====================------===================================================
;; CONTRAST adverbial e.g., "WHILE MAGIC WAS A GUARD, Bird was a forward."
;;=============================================================================
;;
(make-disjunct contrast)

(def-alt cat-contrast-disjunct (:index cat)
  (((cat pp) (prep ((lex ((alt (given "in contrast to")))))))
   ((cat clause)
    (mood bound-adverbial)
    (binder ((lex ((alt (given "whereas")))))))))

(make-map-circum contrast disjunct)


;;====================------===================================================
;; EXCEPTION adverbial e.g., "EXCEPT LAETTNER, all US Olympians were pro."
;;=============================================================================
;;
(make-disjunct exception)

(def-alt cat-exception-disjunct (:index cat)
  (((cat pp) (prep ((lex ((alt (given "except")))))))
   ((cat clause)
    (mood present-participle)
    (binder ((lex ((alt (given "except")))))))))

(make-map-circum exception disjunct)


;;====================------===================================================
;; SUBSTITUTION adverbial e.g., "INSTEAD OF JORDAN, Portland drafted Bowie."
;;=============================================================================
;;
(make-disjunct substitution)

(def-alt cat-substitution-disjunct (:index cat)
  (((cat pp) (prep ((lex ((alt (given "instead of")))))))
   ((cat clause)
    (mood ((alt (present-participle bare-infinitive))))
    (binder ((lex ((alt (given "rather than")))))))))

(make-map-circum substitution disjunct)


;;=============================================================================
;; COMPARISON predicate modifier, e.g., "Bo scored AS IF IN PEEK FORM."
;;=============================================================================
;;
(make-pred-modif comparison)

(def-alt cat-comparison-pred-adjunct  (:index cat)
  (((cat clause)
    (mood ((alt (bound-adverbial present-participle past-participle
                 to-infinitive verbless))))
    (binder ((lex ((alt (given "as if")))))))))

(make-map-pred-modif comparison)

(make-qrel comparison :q-pron "how" :q-embedded no :rel-embedded no)


;;=============================================================================
;; COMPARISON circumstantial, e.g., "LIKE MIKE, he soared above the rim."
;;=============================================================================
;;
(make-sent-adjunct comparison)

(def-alt cat-comparison-sent-adjunct (:index cat)
  (((cat pp)
    (alt compar-polarity
	(((comp-polarity +)
	  (prep ((lex ((alt compar-prep+ (given "like")))))))
	 ((comp-polarity -)
	  (prep ((lex ((alt compar-prep- (given "unlike"))))))))))))

(make-map-circum comparison sent-adjunct)


;;=============================================================================
;; MATTER predicate modifier, e.g., "Bo talked to them ABOUT HIS CONTRACT."
;;=============================================================================
;;
(make-pred-modif matter)

(def-alt cat-matter-pred-adjunct (((cat pp) (prep ((lex ((alt (given "about")))))))))

(make-map-pred-modif matter)

(make-qrel matter :q-pron "what" :q-prep "about" :rel-mark "about which")


;;=============================================================================
;; MATTER circumstantial, e.g.,
;; "CONCERNING HIS CONTRACT, Bo talked to them soon."
;;=============================================================================
;;
(make-disjunct matter)

(def-alt cat-matter-disjunct
    (((cat pp) (prep ((lex ((alt (given "concerning")))))))))

(make-map-circum matter disjunct)


;;=========================-===================================================
;; STANDARD adverbial e.g., "FOR A 7 FOOTER, Divac handles the ball very well."
;;=============================================================================
;;
(make-disjunct standard)

(def-alt cat-standard-disjunct (((cat pp) (prep ((lex ((alt (given "for")))))))))

(make-map-circum standard disjunct)


;;=========================-===================================================
;; PERSPECTIVE adverbial e.g., "AS A FLOOR LEADER, Magic was incomparable."
;;=============================================================================
;;
(make-disjunct perspective)

(def-alt cat-perspective-disjunct
    (((cat pp) (prep ((lex ((alt (given "as")))))))))

(make-map-circum perspective disjunct)


;;=========================-===================================================
;; DOMAIN SPECIFIC ROLES
;; Score is specific to the NBA domain
;; Features concerning relative clauses whose scope would be the score role,
;; e.g., "The score by which Utah defeated the Knicks"
;; or concerning questions whose scope would be the score role, e.g.,
;; "By what score did Utah beat the Knicks?"
;; are ignored here, since not encountered in the NBA corpus
;;=============================================================================
;;
(def-alt score (:demo "Is there a score role?")
  (((pred-modif ((score none))))
   ((pred-modif ((score ((cat #(under score))))))
    (end-adverbial-1 {^ pred-modif score}))))


;; ============================================================
(provide "adverbial")
;; ============================================================
