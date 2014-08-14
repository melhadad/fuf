;;; --- Mode:Lisp; Syntax:Common-Lisp; Package:FUG5 ---
;;; ------------------------------------------------------------
;;; File        : IR3.L
;;; Description : Simple inputs to test GR3
;;; Author      : Michael Elhadad
;;; Created     : 26-Feb-89
;;; Modified    : 18 May 90
;;; Language    : Common Lisp
;;; Package     : FUG5
;;; ------------------------------------------------------------

(in-package "FUG5")


(defun isetup3 ()

  (clear-tests)

  (def-test ir30
      "John eats a meal."
    ((cat clause)
     (prot ((lex "John")
	    (np-type proper)))
     (verb ((lex "eat")
	    (transitive-class transitive)))
     (goal ((lex "meal")
	    (definite no)))))

  (def-test ir31
      "John gives a blue book to Mary."
    ((cat clause)
     (prot ((lex "John")
	    (np-type proper)))
     (goal ((lex "book")
	    (np-type common)
	    (definite no)
	    (describer === "blue")))
     (benef ((lex "Mary")
	     (np-type proper)))
     (verb ((transitive-class bitransitive)
	    (dative-prep "to")
	    (lex "give")))))

  (def-test ir32
      "A book is given by John to Mary."
    ((cat clause)
     (voice passive)
     (prot ((lex "John")
	    (np-type proper)))
     (goal ((lex "book")
	    (np-type common)
	    (definite no)))
     (benef ((lex "Mary")
	     (np-type proper)))
     (verb ((dative-prep "to")
	    (transitive-class bitransitive)
	    (lex "give")))))

  (def-test ir33 
      "Mary is thrown a heavy ball by John."
    ((cat clause)
     (prot ((lex "John")
	    (np-type proper)))
     (goal ((lex "ball")
	    (np-type common)
	    (definite no)
	    (describer === heavy)))
     (benef ((lex "Mary")
	     (np-type proper)))
     (subject {benef})
     (verb ((transitive-class bitransitive)
	    (dative-prep "to")
	    (lex "throw")))))

  (format t "~%ir3 installed. 4 tests.~%")
  (values))







