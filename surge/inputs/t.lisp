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
