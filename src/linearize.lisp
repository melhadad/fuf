;;; -*- Mode:Lisp; Syntax:Common-Lisp; Package:FUG5 -*-
;;; -----------------------------------------------------------------------
;;; File:         LINEARIZE.L
;;; Description:  Linearizer and Morphology for FUG5
;;; Author:       Michael Elhadad
;;; Created:      16-Oct-88
;;; Modified:     03-Nov-89 (ME: checked lex is a string - convert otw)
;;;               30 Apr 90: moved exports to fug5
;;;               10 May 90: path is a special type.
;;;               03 May 91: add support for ordinal and cardinal
;;;               05 May 91: allow for paths in patterns in linearize
;;;               21 May 91: added relative pronouns
;;;               16 Aug 91: fixed linearize to support paths in patterns.
;;;               18 Aug 91: use attr-p instead of symbolp
;;;               09 Oct 91: use digit inside cardinal to distinguish
;;;                          between 20 and twenty.
;;;               26 Dec 91: added fraction
;;;               27 Dec 91: accept specializations of plural.
;;;               28 Dec 91: accept specializations of pronoun.
;;;               21 Jul 92: add (digit roman) to cardinal and ordinal.
;;;               13 Sep 92: fixed punctuate to avoid Jr..
;;;               26 Oct 92: added capitalize processing.
;;;               22 Nov 92: fixed bug in punctuate with sequences of
;;;                          punctuation (ex: 114th St., Manhattan).
;;;               26 Oct 93: use cat-attribute instead of 'cat
;;;                          update callers of call-linearizer.
;;;               29 Dec 93: changed punctuate to avoid sentences finishing
;;;                          in ",".
;;;               25 May 94: added check for "a euphemism" instead of "an eu"
;;;                          theorist@cs.utexas.edu (Charles Brendan Callaway)
;;;               05 Jul 95: added simple-ordinal and simple-cardinal
;;;               05 Nov 95: fixed morph-pron for which/that rule
;;; Package:      FUG5
;;; Status:       Experimental
;;; -----------------------------------------------------------------------
;;;
;;; FUF - a functional unification-based text generation system. (Ver. 5.4)
;;;  
;;; Copyright (c) 1987-2011 by Michael Elhadad. all rights reserved.
;;;  
;;; Permission to use, copy, and/or distribute for any purpose and
;;; without fee is hereby granted, provided that both the above copyright
;;; notice and this permission notice appear in all copies and derived works.
;;; Fees for distribution or use of this software or derived works may only
;;; be charged with express written permission of the copyright holder.
;;; THIS SOFTWARE IS PROVIDED ``AS IS'' WITHOUT EXPRESS OR IMPLIED WARRANTY.
;;; -----------------------------------------------------------------------


(in-package "FUG5")
(format t "Linearizer...~%")


;; ------------------------------------------------------------
;; TOP LEVEL : (call-linearizer fd)
;; Original by Jay Meyer (USC/ISI)
;; ------------------------------------------------------------
(defun call-linearizer (list &key (cat-attribute *cat-attribute*))
  (let ((*input* list)
	(*unknown-cat* nil))
    (declare (special *input* *unknown-cat*))
    (if (leaf-p list)
	(list "<fail>")
      (values
       (capitalize 
	(punctuate (linearize list (make-path) :cat-attribute cat-attribute)
		   (subsume 'interrogative (second (safe-assoc 'mood list))))
	(safe-assoc 'capitalize list))
       *unknown-cat*))))



;; ------------------------------------------------------------
;; CAPITALIZE and PUNCTUATE
;; Original written by Jay Meyer (USC/ISI)
;; ------------------------------------------------------------
(defun CAPITALIZE (sentence &optional dontcapitalize)
  "Capitalizes the first word in a sentence." 
  (if (and dontcapitalize (eq (second dontcapitalize) 'no))
      sentence
    (cons (string-capitalize (car sentence) :end 1)(cdr sentence))))

(defun punctuation (letter)
  (member letter '(#\, #\. #\; #\: #\! #\?)))

(defun bracket-open-char (letter)
  (member letter '(#\( #\[ #\" #\' #\`)))

(defun bracket-close-char (letter)
  (member letter '(#\' #\` #\" #\] #\))))

(defun bracket-char (letter)
  (or (bracket-open-char letter)
      (bracket-close-char letter)))

(defun VOWEL (char) 
  (member char '(#\a #\e #\i #\o #\u)))

(defun consonant-dipthong (word)
  (and (>= (length word) 2)
       (member (subseq word 0 2) '("eu") :test #'equal)))

(defun last-char (str)
  (char str (- (length str) 1)))

(defun butlast-char (str)
  (subseq str 0 (- (length str) 1)))

;; Rules: 
;; 0/ deal with "an empty" "an RST relation"
;; 1/ don't put spaces before closing brackets.
;; 2/ don't put spaces after opening brackets.
;; 3/ don't put spaces before punctuation.
;; 4/ filter out ",p" into "p" for every punctuation p
;; 5/ filter out ".p." into ".p" except ".." into "."
;; 6/ for all other "pq", "q"
(defun PUNCTUATE (sentence question-flag)
  (setf sentence (remove "" sentence :test #'equalp))

  ;; Remove punctuation at beginning of sentence
  (while (and sentence (punctuation (char (car sentence) 0)))
    (let ((x (car sentence)))
      (while (and (not (equal x "")) (punctuation (char x 0)))
	(setq x (subseq x 1)))
      (if (equal x "") 
	(setf sentence (cdr sentence))
	(setf (car sentence) x))))

  ;; Add punctuation at end of sentence if necessary.
  ;; Hard to do that in grammar because does not know if constituent is
  ;; embedded or not - so leave it to here.
  (setf sentence (append sentence (list (if question-flag "?" "."))))

  (if (null sentence)
      (list "<fail>")
    (append 
     (mapcan 
      #'(lambda (x y) 
	  (when (noun-marked-as-an x)
	    (setq x (unmark-noun-as-an x)))
	  (cond
           ;; Rule 0
           ((equal x *indefinite-article*)
            (if (and (not (consonant-dipthong y))
                     (or (vowel (elt y 0))
                         (noun-marked-as-an y)))
                (list "an" " ")
              (list "a" " ")))
           ;; Rules 1 and 2 
	   ((or (bracket-open-char (char x 0))
		(bracket-close-char (char y 0)))
	    (list x))
	   ;; Rule 4
	   ((and (equal (last-char x) #\,)
		 (punctuation (char y 0)))
	    (list (butlast-char x)))
	   ;; Rule 5
	   ((and (equal (last-char x) #\.)
		 (punctuation (char y 0))
		 (not (equal (char y 0) #\.)))
	    (list x))
	   ;; Rule 6
	   ((and (punctuation (last-char x))
		 (punctuation (char y 0)))
	    (list (butlast-char x)))
	   ;; Rule 3
	   ((punctuation (char y 0))
	    (list x))
	   ;; Default: put a space between words.
	   (t (list x " "))))
      (butlast sentence)
      (cdr sentence))
     (last sentence))))
		  


;; ------------------------------------------------------------
;; LINEARIZER
;; ------------------------------------------------------------
(defun linearize (fd path &key (cat-attribute *cat-attribute*))
  "Linearizes fd (found at level path from the root of the total
       fd) and returns a list of strings.
       Handles gaps and punctuation.
       If a feature (gap xxx) where xxx is not none is found in a
       constituent, the constituent does not appear in the result.
       The features punctuation have a special effect described
       in documentation of function list-punctuation."
  (cond 
    ((leaf-p fd) nil)
    (t
      (let ((pattern     (incr-gdp 'pattern fd path))
	    (gap         (incr-gdp 'gap fd path))
	    (punctuation (incr-gdp 'punctuation fd path))
	    (punct-path  (path-extend path 'punctuation)))
	(cond 
	  ((and gap (not (eq gap 'none)))
	    ;; constituent is gapped - don't put it in linearized 
	    nil)
	  (pattern 
	    ;; recurse on all constituents
	    (let ((order (clean-pattern pattern)))
	      (list-punctuation
		punctuation punct-path
		(mapcan 
		 #'(lambda (constituent)
		     (let ((new-path (safe-path-extend path constituent)) 
			   (new-fd 
			    (if (attr-p constituent)
			      (incr-gdp constituent fd path)
			      (gdp *input* 
				   (absolute-path 
				    constituent 
				    (path-extend path 'pattern))))))
		       (linearize new-fd new-path)))
		 order))))
	  (t
	    ;; simple constituent: call morphology
	    (let ((cat (incr-gdp cat-attribute fd path))
		  (lex (incr-gdp 'lex fd path)))
	      (list-punctuation punctuation punct-path
				(morph cat lex fd path
				       :cat-attribute cat-attribute)))))))))
		  


;; ------------------------------------------------------------
;; MORPHOLOGY
;; From a draft by Jay Meyer (USC/ISI).
;; Changed by Michael Elhadad.
;; ------------------------------------------------------------
(defun list-if-atom (item)
  "Returns the list of an atom when passed an atom other than nil."
  (if (listp item) item (list item)))

(defun MORPH (cat lex fd path &key (cat-attribute *cat-attribute*))
  "Dispatch according to cat to specialized morphology routine.
       Handles verbs, nouns, pronouns and dets."
  (declare (special *unknown-cat*))
  (unless (stringp lex)
    (setq lex (string-downcase (format nil "~s" lex))))
  (when (subsume 'pronoun cat) (setf cat 'pronoun))
  (list-if-atom 
   (case cat
     ((adj adv conj modal prep relpro punctuation phrase) lex)
     (noun (morph-noun
	    lex
	    (incr-gdp 'number fd path)
	    (check-a-an (incr-gdp 'a-an   fd path))
	    (incr-gdp 'feature fd path)))
     (verb (morph-verb
	    lex
	    (incr-gdp 'ending fd path)
	    (check-number (incr-gdp 'number fd path))
	    (check-person (incr-gdp 'person fd path))
	    (check-tense (incr-gdp 'tense fd path))))
     (pronoun
      (morph-pronoun
       lex
       (incr-gdp 'pronoun-type fd path)
       (check-case (incr-gdp 'case fd path))
       (check-gender (incr-gdp 'gender fd path))
       (check-number (incr-gdp 'number fd path))
       (check-distance (incr-gdp 'distance fd path))
       (check-animate (incr-gdp 'animate fd path))
       (check-person (incr-gdp 'person fd path))
       (check-restrictive (incr-gdp 'restrictive fd path))))
     ((article det)
      (morph-determiner
       lex
       (incr-gdp 'number fd path)))
     ((ordinal cardinal simple-cardinal simple-ordinal)
      (morph-numeric lex cat 
		     (incr-gdp 'value fd path)
		     (incr-gdp 'digit fd path)))
     (fraction
      (morph-fraction lex 
		      (incr-gdp 'num fd path) 
		      (incr-gdp 'den fd path)
		      (incr-gdp 'digit fd path)))
     (t (cond ((member cat (categories-not-unified cat-attribute)) lex)
	      (t
	       (push cat *unknown-cat*)
	       (format nil "<unknown cat ~s: ~a>" cat lex)))))))

;; -----------------------------------------------------------------------
;; MORPHOLOGY-HELP
;; -----------------------------------------------------------------------
(defun morphology-help (&optional unknown-cats)
  "Gives feedback to users who have done something wrong with morpho."
  (when unknown-cats
    (format 
     t 
     "You have used the following categories which are not known ~@
      by the morphology module: ~s~%" unknown-cats))
  (format 
   t 
   "The categories known by the morphology module are: ~@
    adj, adv, conj, modal, prep, relpro, punctuation, phrase:
              lex is sent unmodified.~@
    noun:         agreement done on number, a-an and feature.~@
    verb:         agreement done on ending, number, person and tense.~@
    pronoun:      agreement done on pronoun-type, case, gender, number,
              distance, animate and person.~@
    det, article: agreement done on number. A/an is processed.~@
    fraction: find string based on num, den (numbers) and digit (yes, roman or no).~@
    ordinal, cardinal: find string based on value (a number) and
              digit (yes, roman or no) to use letters or digits.~%")
  (values))


;; ------------------------------------------------------------
;; Checks: verify validity of features influencing morphology 
;; and chooses default values.
;; ------------------------------------------------------------
(defun CHECK-PERSON (person) 
  "Returns person, DEFAULT is THIRD." 
  (cond ((member person '(first second third)) person)
        (t 'third)))

(defun CHECK-TENSE (tense)
  "Returns tense, DEFAULT is PRESENT." 
  (cond ((member tense '(present past)) tense)
	(t 'present)))

(defun CHECK-NUMBER (number) 
  "Returns number, DEFAULT is SINGULAR." 
  (cond ((member number '(plural dual not-one)) number)
        (t 'singular)))

(defun CHECK-CASE (case)
  "Returns case, DEFAULT is SUBJECTIVE."
  (cond ((member case '(subjective objective possessive reflexive)) case)
	(t 'subjective)))

(defun CHECK-GENDER (gender)
  "Return gender, DEFAULT is MASCULINE."
  (cond ((member gender '(masculine feminine neutral)) gender)
	(t  'masculine)))

(defun CHECK-DISTANCE (distance)
  "Return distance, DEFAULT is NEAR."
  (cond ((member distance '(near far)) distance)
	(t  'near)))

(defun CHECK-ANIMATE (animate)
  "Return animate, DEFAULT is NO."
  (cond ((member animate '(yes no)) animate)
	(t 'no)))

(defun CHECK-A-AN (a-an)
  "Return feature A-AN, DEFAULT is CONSONANT."
  (cond ((null a-an) 'consonant)
	(t a-an)))

(defun CHECK-RESTRICTIVE (restrictive)
  "Return restrictive, DEFAULT is NO"
  (cond ((member restrictive '(yes no)) restrictive)
	(t 'no)))

;; ------------------------------------------------------------
;; Specialized Morph. Routines by Jay Meyer (USC/ISI) and Michael Elhadad
;; ------------------------------------------------------------

(defun MORPH-NOUN (word number a-an feature) 
  "If feature is possessive, then return the apostrephised form of the noun
   appropriate to the number.
   If a-an is 'an mark noun with mark-noun-as-an to agree with determiner.
   return word with number suffix." 
  (setq word (morph-number word number)) 
  (setq word (cond ((eq feature 'possessive)
		    (cond ((lexfetch word 'possessive))
			  ((and (subsume 'plural number)
				(eql (elt word (1- (length word))) #\s))
			   (concatenate 'string word "'"))
			  (t (concatenate 'string word "'s"))))
		   (t word)))
  (if (eql a-an 'an) (mark-noun-as-an word) word))


;; Utils to deal with a-an feature. A-an marks are dealt with in punctuate.
(defun mark-noun-as-an (word)
  (concatenate 'string *a-an-marker* word))

(defun unmark-noun-as-an (word)
  (subseq word (length *a-an-marker*)))

(defun noun-marked-as-an (word)
  (and (> (length word) (length *a-an-marker*))
       (string= (subseq word 0 (length *a-an-marker*)) *a-an-marker*)))


(defun MORPH-NUMBER (word number) 
  "Adds the plural suffix to word if number is plural
   note that the default is singular." 
  (cond ((null number) word)
	((null word) word)
        ((subsume 'plural number)
         (cond ((lexfetch word 'plural))
               (t (pluralize word))))
        (t word)))


(defun MORPH-VERB (word ending number person tense) 
  "Adds the proper suffix to the verb root taking into account
   ending, number, person, and tense." 
  (cond ((eq ending 'root) word)
        ((eq ending 'infinitive) (list "to" word)) 
        ((equal word "be") (morph-be number person (or ending tense))) 
        ((eq ending 'present-participle)
         (cond ((lexfetch word 'present-participle))
               (t (form-ing word))))
        ((eq ending 'past-participle) 
         (cond ((lexfetch word 'past-participle))
               (t (form-past word)))) 
        ((lexfetch word (if (and (eq tense 'present)
                            (eq person 'third)
                            (eq number 'singular))
                       'present-third-person-singular
                       tense)))
        ((eq tense 'present) (form-present-verb word number person))
        ((eq tense 'past) (form-past word)) 
        (t nil)))


(defun MORPH-BE (number person tense)
  (case tense
    (present (case number 
	       (singular (case person
			   (first "am")
			   (second "are")
			   (third "is"))) 
	       (t  "are"))) 
    (past (case number
	    (singular (case person
			(first "was")
			(second "were")
			(third "was")))
	    (t "were")))
    (present-participle "being")
    (past-participle "been")))


(defun FORM-PRESENT-VERB (word number person) 
  "Forms the suffix for the present tense of the verb WORD." 
  (when word
    (case person
      (first word)
      (second word)
      (third (case number
	       (singular (pluralize word))
	       (plural word)
	       (dual word)
	       (not-one word)
	       (otherwise nil)))
      (otherwise nil))))


(defun FORM-PAST (word) 
  "Form past tense by adding 'ed to word,
  handles duplication of final consonant and special cases. " 
  (let ((char (elt word (1- (length word)))))
    (case char
      (#\e (concatenate 'string word "d")) 
      (#\y (if (vowel (elt word (- (length word) 2)))
	       (concatenate 'string word "ed")
	     (concatenate 'string (subseq word 0 (1- (length word))) 
			  "ied")))
      (#\r (if (char-equal (elt word (- (length word) 2)) #\a)
	       (concatenate 'string word (list char) "ed")
	     (concatenate 'string word "ed")))
      ((#\b #\d #\g #\m #\n #\p #\t)
       (if (and (vowel (elt word (- (length word) 2)))
		(not (vowel (elt word (- (length word) 3)))))
	   (concatenate 'string word (list char) "ed")
	 (concatenate 'string word "ed")))
      (t (concatenate 'string word "ed")))))


(defun FORM-ING (word)
  "Add 'ing to word dropping final 'e if any,
  handles duplication of final consonant and special cases."
  (let ((char (elt word (1- (length word)))))
    (case char
      (#\e (concatenate 'string (subseq word 0 (1- (length word))) "ing")) 
      (#\r (if (char-equal (elt word (- (length word) 2)) #\a)
	       (concatenate 'string word (list char) "ing")
	     (concatenate 'string word "ing")))
      ((#\b #\d #\g #\m #\n #\p #\t)
       (if (and (vowel (elt word (- (length word) 2)))
		(not (vowel (elt word (- (length word) 3)))))
	   (concatenate 'string word (list char) "ing")
	 (concatenate 'string word "ing")))
      (t (concatenate 'string word "ing"))))) 


(defun MORPH-DETERMINER (word number)
  "Returns the correct article for the given type.
       DOES NOT CHECK for next word after indefinite - this is
       done in PUNCTUATE, but here just put special mark
       *indefinite-article* to indicate something is to be done."
  (cond ((equal word "a")
         (cond ((subsume 'plural (check-number number)) nil)
	       (t *indefinite-article*)))
        (t word)))


(defun MORPH-PRONOUN (lex pronoun-type case gender number
			  distance animate person restrictive)
  "Returns the correct pronoun given the features."
  (cond ((and (stringp lex)
	      (not (string= lex "nil"))
	      (not (string= lex "none"))) lex)
	((eq pronoun-type 'personal)
	 ;; start with "he" then augment by person, then by
	 ;; number, gender and finally by case.
	 (when (eq animate 'no) (setq gender 'neuter))
	 (setq lex "he")
	 (unless (eq person 'third)
	   (setq lex (cond ((lexfetch lex person)) (t lex))))
	 (unless (eq number 'singular)
	   (setq lex (cond ((lexfetch lex number)) (t lex))))
	 (unless (eq gender 'masculine)
	   (setq lex (cond ((lexfetch lex gender)) (t lex))))
	 (unless (eq case 'subjective)
	   (setq lex (cond ((lexfetch lex case)) (t lex))))
	 lex)
	((eq pronoun-type 'demonstrative)
	 (cond ((subsume 'plural number)
		(if (eq distance 'far) "those" "these"))
	       ((eq distance 'far) "that")
	       (t "this")))
	((eq pronoun-type 'relative)
	 (when (eq animate 'no) (setq gender 'neuter))
	 (cond
	  ;; Rule for Restrictive - cf Winograd p.479
	  ;; which/that only for non-animate.
	  ((and (eq restrictive 'yes) (eq animate 'no)) "that")
	  ((eq case 'possessive) "whose")
	  ((eq case 'objective) 
	   (if (eq gender 'neuter) "which" "whom"))  
	  ((eq gender 'neuter) "which")
	  ((eq animate 'no) "which")
	  (t "who")))
	((eq pronoun-type 'question)
	 (when (eq animate 'no) (setq gender 'neuter))
	 (cond ((eq restrictive 'yes) "which")
	       ((eq case 'possessive) "whose")
	       ((eq case 'objective) 
		(if (eq gender 'neuter) "what" "whom"))
	       ((eq gender 'neuter) "what")
	       ((eq animate 'no) "what")
	       (t "who")))
	((eq pronoun-type 'quantified)
	 lex)))


(defun format-ordinal (value)
  ;; Integer to string of the form "nth" 
  ;; 1 -> st except for 11
  ;; 2 -> nd except for 12
  ;; 3 -> rd except for 13
  ;; otw th
  (let* ((lasttwo (mod value 100))
	 (lastone (mod value 10))
	 (ext (cond ((member lasttwo '(11 12 13)) "th")
		    ((= lastone 1) "st")
		    ((= lastone 2) "nd")
		    ((= lastone 3) "rd")
		    (t "th"))))
    (format nil "~s~a" value ext)))
	 

(defun MORPH-NUMERIC (lex ord-or-card value digit)
  "Return a string given a number - either ordinal or cardinal"
  ;; Lucky common-lisp does all for us!!!
  (cond ((and (stringp lex)
	      (not (numberp value))     ;; if value is given use it
	      (not (string= lex "nil"))
	      (not (string= lex "none"))) lex)
	((integerp value)
	 (case digit
	   (no (case ord-or-card
		 ((ordinal simple-ordinal) (format nil "~:r" value))
		 ((cardinal simple-cardinal) (format nil "~r" value))
		 (t (format nil "~r" value))))
	   (roman (format nil "~@R" value))
	   (t  (case ord-or-card
		 ((ordinal simple-ordinal) (format-ordinal value))
		 (t (format nil "~s" value))))))
	((numberp value)
	 (format nil "~s" value))
	((stringp value) value)
	(t (format nil "<unknown-number: ~s>" value))))
	 

(defun MORPH-FRACTION (lex num den digit)
  "Return a string for a fraction"
  (cond ((and (integerp num) (integerp den))
	 (cond ((= den 1) 
		(case digit 
		  (no (format nil "~r" num))
		  (roman (format nil "~@R" num))
		  (t (format nil "~s" num))))
	       ((and (= num 1) (= den 2))
		(case digit
		  (no "half")
		  (t "1/2")))
	       ((= den 2)
		(case digit
		  (no "~r halves" num)
		  (t "~s/2" num)))
	       (t
		(case digit
		  (no (format nil "~r-~:r~P" num den num))
		  (t (format nil "~s/~s" num den))))))
	((and (stringp lex)
	      (not (string= lex "nil"))
	      (not (string= lex "none"))) lex)
	(t (format nil "~s/~s" num den))))


;; ------------------------------------------------------------
;; List-punctuation
;; ------------------------------------------------------------

(defun list-punctuation (punctuation path lstring)
  "Punctuation is an fd with 3 relevant features: before, after and
       capitalize. 
       The value of both before and after must be a string of
       punctuation. Capitalize can be yes or no.  Default is no.
       Path is the path from the root of the total fd to punctuation.
       Lstring is a list of string.
       Returns a list of strings with punctuation before and
       after lstring as indicated by punctuation."
  (setq lstring (trim-lstring lstring))
  (cond ((null punctuation) lstring) 
	(t 
	 (let ((before (incr-gdp 'before punctuation path))
	       (after  (incr-gdp 'after  punctuation path))
	       (capitalize (incr-gdp 'capitalize punctuation path)))
	   (unless (or (null capitalize) (eq 'none capitalize)
		       (eq 'no capitalize))
	     (setq lstring 
		   (cons (string-capitalize (car lstring) :end 1)
			 (cdr lstring))))
	   (when (stringp before)
	     (setq lstring (cons before lstring)))
	   (when (stringp after)
	     (setq lstring (append lstring (list after))))
	   lstring))))


(defun trim-lstring (lstring)
  (remove "" lstring :test #'string-equal))

;; ------------------------------------------------------------
;; Pluralize (written by Jay Meyer)
;; ------------------------------------------------------------

(defun PLURALIZE (word-string)
  "Handles word-string ending in ch, sh, o, s, x, y, and z 
  as well as regular forms."
  (let* ((n (length word-string))
         (next-to-final (elt word-string (- n 2)))
         (final (elt word-string (1- n))))
    (case final
          ((#\s #\z #\x) (concatenate 'string word-string "es"))
          (#\h (if (member next-to-final '(#\c #\s))
                   (concatenate 'string word-string "es")
                   (concatenate 'string word-string "s")))
          (#\o (if (not (vowel next-to-final))
                   (concatenate 'string word-string "es")
                   (concatenate 'string word-string "s")))
          (#\y (if (not (vowel next-to-final))
                   (concatenate 'string (subseq word-string 0 (1- n)) "ies")
                   (concatenate 'string word-string "s"))) 
          (t (concatenate 'string word-string "s")))))



;; -----------------------------------------------------------------------
(provide "$fug5/linearize")
;; -----------------------------------------------------------------------
