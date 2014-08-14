;;; -*- Mode:Lisp; Syntax:Common-Lisp; Package: FUG5 -*-
;;; -----------------------------------------------------------------------
;;; File:         binding-examples.lisp
;;; Description:  Examples for binding implemantation for SURGE
;;; Author:       Yael Dahan Netzer.
;;; Created:      22 Apr 1996
;;; Modified:     
;;; Package:      FUG5
;;; -----------------------------------------------------------------------

(in-package "FUG5")


;;; Examples:
(def-test b1
  "Mary likes herself."
  ((cat clause)
   (process ((type mental) (lex "like")))
   (partic ((processor ((cat proper) (lex "Mary") 
			(gender feminine) (animate yes)))
	    (phenomenon ((cat proper) (lex "Mary") 
			 (index {^2 processor index})))))))

(try (get-test 'b1))

;; Note on b2: no anaphora but obligatory pronominalization.
;; No anaphora because "her" is locally bound by John.
;; Pronominalization because "her" is bound by Mary.
(def-test b2
  "Mary thinks that John likes her."
  ((cat clause)
   (process ((type mental) (lex "think") (object-clause that)))
   (partic ((processor ((cat proper) (lex "Mary") 
			(gender feminine) (animate yes)))
	    (phenomenon 
	     ((cat clause)
	      (process ((type mental) (lex "like")))
	      (partic ((processor ((cat proper) (lex "John")))
		       (phenomenon ((cat proper) (lex "Mary")
				    (index {^4 processor index})))))))))))

(try (get-test 'b2))
;; Note on b3: need to implement cleft.
;; After clefting, get a trace that is co-indexed (not shared) with the
;; focus in the base clause.  Then process binding -- check how
;; NOT CLEAR.
;(def-test b3  ;;;
;  "It is herself that Mary likes."
;  ((cat clause)
;   (process ((type mental) (lex "like")))
;   (partic ((processor ((cat proper) (lex "Mary") (gender feminine)))
;	    (phenomenon ((cat proper) (index {^2 processor index})))))
;   (cleft ((focus {^2 partic phenomenon})))))


;; Note on b4: special category of "picture" nouns. -- probably don't need.
;; Need to take the complement of the noun as a subcategorized element
;; that is taken into account by the o-command relation.
(def-test b4
  "John bought a picture of himself."
  ((cat clause)
   (process ((type material) (lex "buy") (tense past)))
   (partic ((agent ((cat proper) (lex "John") (animate yes)))
	    (affected ((cat common) (lex "picture") (definite no)
		       (picture-type yes)
		       (qualifier ((cat pp)
				   (prep ((lex "of")))
				   (np ((cat proper) (lex "John")
                                        (reflexive-type reflexive) 
					(index {^4 agent index})))))))))))

(try (get-test 'b4))

;;; from Pollard and Sag 
;(def-test b5
;  "John thought that it would be illegal to undress himself."
;  ((cat clause)
;   (proc ((type mental)
;          (lex "think")
;          (tense past)))
;   (partic ((processor ((lex "John") (cat proper)))
;            (phenomenon ((cat clause)


(store-verbs '(("meet" "meets" "met" "meeting" "met")))

(def-test b7 ;; try reciprocal
  "They met each other"
  ((cat clause)
   (tense past)
   (proc ((type material) (lex "meet")))
   (partic ((agent ((cat personal-pronoun) (number plural)))
            (affected ((index {^2 agent index}) (reflexive-type reciprocal)))))))
  
(try (get-test 'b7))

;; The following does not work because a conjunction does not have a common index of
;; its components.
;(def-test b8
;  "Anne and Cathy met each other."
;  ((cat clause)
;   (tense past)
;   (proc ((type material) (lex "meet")))
;   (partic ((agent ((cat proper)
;                    (complex conjunction)
;                    (distinct ~(((lex "Anne"))
;                                ((lex "Cathy"))))))
;            (affected ((index {^2 agent index})
;                       (reflexive-type reciprocal)))))))


(def-test b6
  "John talked to Paul about himself."
  ;; himself can refer both to Paul and John
  ;; "John talked about himself to Paul"
  ;; himself can refer only to John
  ((cat clause)
   (tense past)
   ;;; I purposely make it material, cannot be verbal.
   (proc ((type material)
          (lex "talk")))
   (partic ((agent ((lex "John") (cat proper) (animate yes)))
            (affected ((cat pp) (prep ((lex "to"))) 
		       (np ((lex "Paul") (cat proper)))))))
   (pred-modif ((matter ((cat pp)
                         (prep ((lex "about")))
                         (np ((lex "John") (cat proper) 
			      (index {^4 partic agent index})))))))))


(try (get-test 'b6))


;;; The following does not work properly, because coreference checks the index
;;; of the path given. in this case, 'affected' is the {synt-role object} but
;;; the index features are under {synt-roles object np}
(def-test b6a
  "John talked to Paul about himself."
  ;; himself can refer both to Paul and John
  ;; If the order is:
  ;; "John talked about himslef to Paul"
  ;; himself can refer only to John
  ((cat clause)
   (tense past)
   ;;; I purposely make it material, cannot be verbal.
   (proc ((type material)
          (lex "talk")))
   (partic ((agent ((lex "John") (cat proper) (animate yes)))
            (affected ((cat pp) (prep ((lex "to"))) 
		       (np ((lex "Paul") (cat proper)
			    (animate yes)))))))
   (pred-modif ((manner ((cat pp)
                         (prep ((lex "about")))
                         (np ((lex "Paul") (cat proper) 
			      (index {^4 partic affected np index})))))))))


(try (get-test 'b6a))

				    
;;; John's book that his mother wrote, about himself.

(setf test-list-within-np
      '((cat common)
	(possessor ((cat proper) (lex "John") (gender masculine)))
	(lex "book")
	(qualifier 
	 ((cat list)
	  (distinct
	   ~(
	     ((cat clause)
	      (restrictive yes)
	      (tense past)
	      (proc ((type material)
		     (lex "write")))
	      (scope {^ partic affected})
	      (partic ((agent ((cat common)
			       (lex "mother")
			       (possessor ((cat personal-pronoun)
					   (index {^7 possessor index}))))))))
	     ((cat pp)
	      (prep ((lex "about")))
	      (np ((cat personal-pronoun)
		   (index {^6 possessor index}))))))))))

(try2 test-list-within-np)
        


(setf aa
      '((cat clause)
	(proc ((lex "eat")
	       (type material)))
	(partic ((agent ((lex "man") (cat common) (animate yes) (definite no)))
		 (affected ((lex "man") (cat common) 
			    (index {^2 agent index})))))))

(try aa)
;;;"The analysis completed, they treated him homeopatically as if to avoid conventional treatments." 

;; John sold the slave to himself

;;; John's book [that his mother wrote] [ about himself] - is it possible?



