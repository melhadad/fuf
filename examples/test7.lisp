;;; -*- Mode:Lisp; Syntax:Common-Lisp; Package: FUG5 -*-
;;; -----------------------------------------------------------------------
;;; File:         test7.l
;;; Description:  Test the ^~ notation to escape from a list and ~n notation.
;;; Author:       Michael Elhadad
;;; Created:       9 Jun 1993
;;; Modified:     
;;; Package:      FUG5
;;; -----------------------------------------------------------------------

(in-package "FUG5")

;; An fd extracted from Jacques Robin's Basketball domain.
;; This is a list of entities and a list of relations in which they
;; participate. 
(defun input ()
 (setf input
       '((cat gen-rule)
	 (ents ~(((concept player) (name malone))
		 ((concept stat) (value 28pts))
		 ((concept player) (name stockton))
		 ((concept stat) (value 27pts))
		 ((concept stat) (value 24asts))))

	 ;; Note the usage of ^n~ to escape to the beginning of the
	 ;; containing list and ~n to go down nth elt of a list.
	 (rels ~(((concept stat-rel)
		  (args ((carrier {^2~ ents ~1}) 
			 (stat {^2~ ents ~2}))))
		 ((concept stat-rel)
		  (args ((carrier {^2~ ents ~3}) 
			 (stat {^2~ ents ~4}))))
		 ((concept stat-rel)
		  (args ((carrier {^2~ ents ~3}) 
			 (stat {^2~ ents ~4})))))))))


;; A grammar to find matching elements in the lists and compose the matched
;; elements 
(def-grammar rules ()
 (clear-bk-class)
 '((ALT
    (((cat gen-rule)
      (ent1 ((cat find2)
	     (in2 {^2 ents})
	     (1st-matchA ((concept player) (name stockton)))
	     (1st-matchB ((concept stat)))))
      (rel1 ((cat find1)
	     (in1 {^2 rels})
	     (1st-match ((concept stat-rel)
			 (args ((carrier {^4 ent1 1st-matchA})
				(stat {^4 ent1 1st-matchB})))))))
      (cset ((= ent1 rel1)))
      (clause ((cat clause)
	       (process stat)
	       (args {^2 rel1 1st-match args}))))

     ;; Find one element in a list in1 that matches 1st-match and unifies
     ;; it with 1st-match.
     ;; If no element in list in1, fail
     ;; If first element matches 1st-match, unify and stop recursion
     ;; Else recurse on rest of in1.
     ;; 1st-match is always linked to the top-level 1st-match feature in
     ;; the embedded structure of recursive calls.
     ((cat find1)
      (fset (in1 1st-match rest cat cset))
      (ALT find1 (((1st-match {^ in1 car})
		   (cset ()))
		  ((rest ((cat find1)
			  (1st-match {^2 1st-match})
			  (in1 {^2 in1 cdr})))
		   (cset ((= rest)))))))

     ;; Find 2 elements in a list in2 that match 1st-matchA and 1st-matchB
     ;; and unify them.
     ((cat find2)
      (fset (in2 1st-matchA 1st-matchB restA restB cat cset))
      (cset ((= restA restB)))
      (restA ((cat find1) 
	      (1st-match {^2 1st-matchA})
	      (in1 {^2 in2})))
      (restB ((cat find1) 
	      (1st-match {^2 1st-matchB})
	      (in1 {^2 in2}))))))))




;; To test:
;; (setq output (uni-fd (input) (grammar)))
;; (top-gdp output {clause args carrier})
;; (top-gdp output {clause args stat})
