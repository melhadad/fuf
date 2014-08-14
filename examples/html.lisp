;;; -*- Mode:Lisp; Syntax:Common-Lisp; Package: FUG5 -*-
;;; -----------------------------------------------------------------------
;;; File:         html.l
;;; Description:  Examples for using the HTML attribute
;;; Author:       Michael Elhadad
;;; Created:       2 Jul 1996
;;; Modified:     
;;; Package:      FUG5
;;; -----------------------------------------------------------------------

(in-package "FUG5")

(format t "~%These examples should only work with SURGE 2.0.~%~
  They show how to use the HTML attribute in FUF.")

;; This is all very straightforward except for the use of html-alt instead
;; of the HTML tag alt to avoid confusion with FUF's reserved keyword alt.

;; ============================================================
;; HTML examples
;; ============================================================

;; HTML tags go around constituents.
;; HTML tags can have parameters (like the A tag).
(def-test t422
  "<A HREF=\"http://www.film.com\">I am not a smart man</a>, but I know <B>what love is</B>."
  ((cat ds)
   (subordinate ((cat clause)
		 (html ((a ((href "http://www.film.com")))))
		 (process ((type ascriptive)))
		 (polarity negative)
		 (partic ((carrier ((cat personal-pronoun)
				    (person first)))
			  (attribute ((cat common)
				      (definite no)
				      (describer === "smart")
				      (lex "man")))))))
   (connective ((lex "but")))
   (directive ((cat clause)
	       (process ((type mental)
			 (lex "know")))
	       (partic ((processor ((cat personal-pronoun)
				    (person first)))
			(phenomenon ((cat clause)
				     (html ((b +)))
				     (mood bound-nominal)
				     (binder ((lex "what")))
				     (controlled {^ partic attribute})
				     (process ((type ascriptive)))
				     (partic ((carrier ((cat common)
							(lex "love")
							(countable no)))))))))))))

(def-test t423-a
  "The recipe has 3 steps:"
  ((cat clause)
   (punctuation ((after ":")))
   (process ((type possessive)))
   (partic 
    ((possessor ((cat common) (lex "recipe") (definite yes)))
     (possessed ((cat common) (lex "step") (definite no) 
		 (cardinal ((value 3) (digit yes)))))))))
				  
	       
(def-test t423
  "The recipe has 3 steps: <OL><LI>start the fire </LI><LI>burn the olives </LI><LI>eat the pits </LI></OL>."
  ((cat list)
   (distinct 
    ~( 
      ((cat clause)
       (punctuation ((after ":")))
       (process ((type possessive)))
       (partic 
	((possessor ((cat common) (lex "recipe") (definite yes)))
	 (possessed ((cat common) (lex "step") (definite no) 
		     (cardinal ((value 3))))))))
      ((cat list)
       (html ((ol +)))
       (distinct 
	~(
	  ((cat clause)
	   (html ((li +)))
	   (mood imperative)
	   (process ((type material) (lex "start")))
	   (partic ((agent ((cat personal-pronoun) (person second)))
		    (affected ((cat common) (lex "fire"))))))
	  ((cat clause)
	   (html ((li +)))
	   (mood imperative)
	   (process ((type material) (lex "burn")))
	   (partic ((agent ((cat personal-pronoun) (person second)))
		    (affected ((cat common) (lex "olive") (number plural))))))
	  ((cat clause)
	   (html ((li +)))
	   (mood imperative)
	   (process ((type material) (lex "eat")))
	   (partic 
	    ((agent ((cat personal-pronoun) (person second)))
	     (affected ((cat common) (lex "pit") (number plural)))))))))))))


(def-test t424
  "This picture contains the secret <B>for a <I>happy </I> life </B>: <IMG
SRC=\"happy.gif\" ALT=\"HAPPY LIFE SECRET\">."
  ((cat list)
   (distinct 
    ~(
      ((cat clause)
       (proc ((type locative) (mode equative) (lex "contain")))
       (partic
	((identified ((cat common) (lex "picture") (distance near)))
	 (identifier ((cat common) (lex "secret")
		      (qualifier ((cat pp)
				  (prep ((lex "for")))
				  (html ((b +)))
				  (np ((cat common)
				       (definite no)
				       (lex "life")
				       (describer ((html ((i +)))
						   (lex "happy")))))))))))
       (punctuation ((after ":"))))
      ((cat phrase)
       (html ((img ((src "happy.gif")
		    (end-tag none)
		    (html-alt "HAPPY LIFE SECRET")))))
       (lex ""))))))
