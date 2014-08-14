;;; -*- Mode:Lisp; Syntax:Common-Lisp; Package: FUG5 -*-
;;; -----------------------------------------------------------------------
;;; File:         ir-bk-class.lisp
;;; Description:  Tests for Trento paper / For grammar gr10.l
;;; Author:       Michael Elhadad
;;; Created:      19 Aug 1992
;;; Modified:     
;;; Package:      FUG5
;;; -----------------------------------------------------------------------

(in-package "FUG5")

;; ----------------------------------------------------------------------
;; TEST BK-CLASS 
;; ----------------------------------------------------------------------

(defun isetup-bk-class ()

  (clear-tests)

  (def-test t143 
      ("The Denver Nuggets edged the Celtics."
       "The Denver Nuggets nipped the Celtics.")
    ((process ((concept c-game-result)
	       (tense past)
	       (type material)))
     (partic ((agent ((lex "Denver Nugget")
		      (concept c-nuggets)
		      (cat proper)
		      (definite yes)
		      (number plural)))
	      (affected ((definite yes)
			 (concept c-celtics)
			 (lex "Celtic")
			 (number plural)))))
     (circum ((manner ((concept c-narrow)))))))


  (store-verbs '(("beat" "beats" "beat" "beating" "beaten")))

  (def-test t144 
      "The Denver Nuggets narrowly beat the Celtics."
    ((process ((concept c-game-result)
	       (lex "beat")))
     (partic ((agent ((lex "Denver Nugget")
		      (concept c-nuggets)
		      (cat proper)
		      (number plural)))
	      (affected ((definite yes)
			 (lex "Celtic")
			 (cat proper)
			 (concept c-celtics)
			 (number plural)))))
     (tense past)
     (circum ((manner ((concept c-narrow)))))))



  (def-test t145 
      ("The hapless Denver Nuggets beat the Celtics."
       "The rock-bottom Denver Nuggets beat the Celtics.")
    ((process ((concept c-game-result)
	       (lex "beat")))
     (partic ((agent ((definite yes)
		      (lex "Denver Nugget")
		      (concept c-nuggets)
		      (cat common)
		      (number plural)))
	      (affected ((definite yes)
			 (lex "Celtic")
			 (cat common)
			 (concept c-celtics)
			 (number plural)))))
     (tense past)
     (adjuncts ((ao none))) ;; don't use an adverb
     (ao ((concept c-team-rating)
	  (partic ((carrier ((concept {partic agent concept})))))
	  (orientation -)))))





  (def-test t146 
      "The Denver Nuggets surprisingly beat the Celtics."
    ((process ((concept c-game-result)
	       (lex "beat")))
     (partic ((agent ((definite yes)
		      (concept c-nuggets)
		      (cat proper)
		      (describer none) ;; don't use an adjective
		      (lex "Denver Nugget")
		      (number plural)))
	      (affected ((definite yes)
			 (concept c-celtics)
			 (cat proper)
			 (lex "Celtic")
			 (number plural)))))
     (tense past)
     (ao ((concept c-team-rating)
	  (partic ((carrier ((concept {partic agent concept})))))
	  (orientation -)))))




  (def-test t147 
      ("The Denver Nuggets stunned the Celtics."
       "The Denver Nuggets surprised the Celtics.")
    ((process ((concept c-game-result)))
     (partic ((agent ((definite yes)
		      (lex "Denver Nugget")
		      (cat common)
		      (concept c-nuggets)
		      (number plural)))
	      (affected ((definite yes)
			 (concept c-celtics)
			 (cat common)
			 (lex "Celtic")
			 (number plural)))))
     (tense past)
     (ao ((concept c-team-rating)
	  (partic ((carrier ((concept {partic agent concept})))))
	  (orientation -)))))




  ;; Now try them together!!! ao AND manner...
  (def-test t148 
      ("The hapless Denver Nuggets nipped the Celtics."
       "The rock-bottom Denver Nuggets nipped the Celtics."
       "The hapless Denver Nuggets edged the Celtics."
       "The rock-bottom Denver Nuggets edged the Celtics.")
    ((process ((concept c-game-result)))
     (partic ((agent ((definite yes)
		      (cat common)
		      (concept c-nuggets)
		      (lex "Denver Nugget")
		      (number plural)))
	      (affected ((definite yes)
			 (concept c-celtics)
			 (cat common)
			 (lex "Celtic")
			 (number plural)))))
     (tense past)
     (ao ((concept c-team-rating)
	  (partic ((carrier ((concept {partic agent concept})))))
	  (orientation -)))
     (circum ((manner ((concept c-narrow)))))))




  (def-test t149 
      ("The Denver Nuggets beat the Celtics 101 - 99."
       "The Denver Nuggets defeated the Celtics 101 - 99."
       "The Denver Nuggets downed the Celtics 101 - 99.")
    ((process ((concept c-game-result)
	       (tense past)
	       (subcat ((1 {lex-roles winner})
			(2 {lex-roles loser})
			(4 {lex-roles score})
			(4 ((cat score)))))))
     (lex-roles ((winner ((lex "Denver Nugget")
			  (concept c-nuggets)
			  (number plural)))
		 (loser ((concept c-celtics)
			 (lex "Celtic")
			 (number plural)))
		 (score ((win 101)
			 (lose 99)))))))




  (def-test t150 
      ("The Denver Nuggets nipped the Celtics 101 - 99."
       "The Denver Nuggets edged the Celtics 101 - 99.")
    ((cat clause)
     (process ((concept c-game-result)
	       (tense past)
	       (subcat ((1 {lex-roles winner})
			(2 {lex-roles loser})
			(4 {lex-roles score})
			(4 ((cat score)))))))
     (lex-roles ((winner ((lex "Denver Nugget")
			  (concept c-nuggets)
			  (number plural)))
		 (loser ((concept c-celtics)
			 (lex "Celtic")
			 (number plural)))
		 (score ((win 101)
			 (lose 99)))))
     (circum ((manner ((concept c-narrow)))))))




  (def-test t151 
      "The Denver Nuggets narrowly beat the Celtics 101 - 99."
    ((cat clause)
     (process ((concept c-game-result)
	       (tense past)
	       (lex "beat")
	       (subcat ((1 {lex-roles winner})
			(2 {lex-roles loser})
			(4 {lex-roles score})
			(4 ((cat score)))))))
     (lex-roles ((winner ((lex "Denver Nugget")
			  (concept c-nuggets)
			  (number plural)))
		 (loser ((concept c-celtics)
			 (lex "Celtic")
			 (number plural)))
		 (score ((win 101)
			 (lose 99)))))
     (circum ((manner ((concept c-narrow)))))))



  (def-test t152 
      ("The Denver Nuggets stunned the Celtics 101 - 99."
       "The Denver Nuggets surprised the Celtics 101 - 99.")
    ((cat clause)
     (process ((concept c-game-result)
	       (tense past)
	       (subcat ((1 {lex-roles winner})
			(2 {lex-roles loser})
			(4 {lex-roles score})
			(4 ((cat score)))))))
     (ao ((concept c-team-rating)
	  (partic ((carrier ((concept c-nuggets)))))
	  (orientation -)))
     (lex-roles ((winner ((lex "Denver Nugget")
			  (concept c-nuggets)
			  (number plural)))
		 (loser ((concept c-celtics)
			 (lex "Celtic")
			 (number plural)))
		 (score ((win 101)
			 (lose 99)))))))




  (def-test t153 
      ("The hapless Denver Nuggets beat the Celtics 101 - 99."
       "The rock-bottom Denver Nuggets beat the Celtics 101 - 99.")
    ((cat clause)
     (process ((concept c-game-result)
	       (tense past)
	       (lex "beat")
	       (subcat ((1 {lex-roles winner})
			(2 {lex-roles loser})
			(4 {lex-roles score})
			(4 ((cat score)))))))
     (ao ((concept c-team-rating)
	  (partic ((carrier ((concept c-nuggets)))))
	  (orientation -)))
     (adjuncts ((ao none)))
     (lex-roles ((winner ((lex "Denver Nugget")
			  (concept c-nuggets)
			  (number plural)))
		 (loser ((concept c-celtics)
			 (lex "Celtic")
			 (number plural)))
		 (score ((win 101)
			 (lose 99)))))))



  (def-test t154 
      "The Denver Nuggets surprisingly beat the Celtics 101 - 99."
    ((cat clause)
     (process ((concept c-game-result)
	       (tense past)
	       (lex "beat")
	       (subcat ((1 {lex-roles winner})
			(2 {lex-roles loser})
			(4 {lex-roles score})
			(4 ((cat score)))))))
     (ao ((concept c-team-rating)
	  (partic ((carrier ((concept c-nuggets)))))
	  (orientation -)))
     (lex-roles ((winner ((lex "Denver Nugget")
			  (concept c-nuggets)
			  (describer none)
			  (number plural)))
		 (loser ((concept c-celtics)
			 (lex "Celtic")
			 (number plural)))
		 (score ((win 101)
			 (lose 99)))))))



  (def-test t155 
      ("The hapless Denver Nuggets nipped the Celtics 101 - 99."
       "The rock-bottom Denver Nuggets nipped the Celtics 101 - 99."
       "The hapless Denver Nuggets edged the Celtics 101 - 99."
       "The rock-bottom Denver Nuggets edged the Celtics 101 - 99.")
    ((cat clause)
     (process ((concept c-game-result)
	       (tense past)
	       (subcat ((1 {lex-roles winner})
			(2 {lex-roles loser})
			(4 {lex-roles score})
			(4 ((cat score)))))))
     (ao ((concept c-team-rating)
	  (partic ((carrier ((concept c-nuggets)))))
	  (orientation -)))
     (circum ((manner ((concept c-narrow)))))
     (lex-roles ((winner ((lex "Denver Nugget")
			  (concept c-nuggets)
			  (number plural)))
		 (loser ((concept c-celtics)
			 (lex "Celtic")
			 (number plural)))
		 (score ((win 101)
			 (lose 99)))))))

  (format t "ir-bk-class installed.~%")
  (values))




