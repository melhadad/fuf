;;; -*- Mode:Lisp; Syntax:Common-Lisp; Package: FUG5 -*-
;;; -----------------------------------------------------------------------
;;; File:         t.lisp
;;; Description:  A few more examples
;;; Author:       Michael Elhadad
;;; Created:      15 Dec 1996
;;; Modified:
;;; Package:      FUG5
;;; -----------------------------------------------------------------------

(in-package "FUG5")

(def-test c100
  "Resist with obstination."
  ((cat clause)
   (mood imperative)
   (process ((type material) (effective no) (lex "resist")))
   (partic ((agent ((cat personal-pronoun) (person second)))))
   (pred-modif
    ((manner ((cat pp)
	      (np ((cat common) (lex "obstination") (countable no)))))))))


(def-test c101
  "How do you resist?"
  ((cat clause)
   (mood wh)
   (scope {^ pred-modif manner})
   (process ((type material) (effective no) (lex "resist")))
   (partic ((agent ((cat personal-pronoun) (person second)))))))


(def-test c102
  "By what is TPR controled?"
  ((cat clause)
   (mood wh)
   (scope {^ partic agent})
   (proc ((type material)
	  (voice passive)
	  ;; (agentless no)
	  (lex "control")))
   (partic ((affected ((cat np) (head === "TPR") (countable no)))))))


(def-test c103
  "How is TPR controled?"
  ((cat clause)
   (mood wh)
   (scope {^ pred-modif manner})
   (process ((type material)
	     (voice passive)
	     (lex "control")))
   (partic ((affected ((cat np) (head === "TPR") (countable no)))))))

;; be-deleted -- should be reworked
#+ignore(def-test be-deleted-1
  "The man crushed by love, now knowing what love is and happy about it."
  ((cat np)
   (lex "man")
   (qualifier ((cat clause)
	       (complex conjunction)
	       (common ((mood be-deleted-relative)))
	       (distinct ~((
			    (proc ((type material)
				   (lex "crush")
				   (agentless no)
				   (voice passive)))
                            (controlled {^ partic affected})
			    (partic ((agent ((lex "love"))))))
			   ((proc ((type mental)
				   (lex "know")
				   (voice active)))
			    (partic ((phenomenon ((cat phrase)
						  (lex "what love is")))))
			    (circum ((time ((cat adv)
					    (lex "now")
					    (position front))))))
			   ((proc ((type ascriptive)
				   (mode attributive)))
			    (partic ((attribute ((cat phrase)
						 (lex "about it"))))))))))))






