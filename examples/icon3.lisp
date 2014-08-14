;;; -*- Mode:Lisp; Syntax:Common-Lisp; Package:FUG5 -*-
;;; ------------------------------------------------------------
;;; File        : ICON3.LISP
;;; Description : Simple inputs to test GCON3
;;; Author      : Michael Elhadad 
;;; Created     : 27-Nov-88
;;; Modified    : 09-Nov-90
;;; Language    : Common Lisp
;;; Package     : FUG5
;;; ------------------------------------------------------------

(in-package "FUG5")

(defun isetup-con3 ()

  (clear-tests)

  (def-test a1 
      "John takes a book from Mary but John is honest."
    ((cat discourse-segment)
     (subordinate
      ((directive 
	((th ~(John Mary Book Transfer))
	 (u  u1)
	 (if ((force assert)))
	 (ao ((scale dishonesty)
	      (conclusion 
	       ((process-type attributive)
		(carrier === John)
		(attribute === dishonest)))))
	 (pc ((cat clause)
	      (process-type action)
	      (concept Transfer)
	      (agent ((lex "John") (np-type proper)))
	      (benef ((lex "Mary") (np-type proper)))
	      (medium ((lex "book") 
		       (definite no) 
		       (np-type common)))))))))
     (directive 
      ((th ~(John Honest))
       (u  u2)
       (if ((force assert)))
       (ao ((scale honesty)))
       (pc ((cat clause)
	    (process-type attributive)
	    (carrier ((lex "John") (np-type proper)))
	    (attribute ((cat adj)
			(lex "honest")))))))))

  (def-test a2 
      "John steals a book from Mary."
    ((cat discourse-segment)
     (directive ((th ~(John Mary Book Transfer))
		 (if ((force assert)))
		 (ao ((scale dishonesty)
		      (conclusion 
		       ((process-type attributive)
			(carrier === John)
			(attribute === dishonest)))))
		 (pc ((cat clause)
		      (process-type action)
		      (concept Transfer)
		      (agent ((lex "John") (np-type proper)))
		      (benef ((lex "Mary") (np-type proper)))
		      (medium ((lex "book") 
			       (definite no)
			       (np-type common)))))))))


  (def-test a3 
      "<fail>"
    ((cat discourse-segment)
     (directive ((th ~(John Mary Book Transfer))
		 (if ((force assert)))
		 (ao ((scale honesty)
		      (conclusion 
		       ((process-type attributive)
			(carrier === John)
			(attribute === honest)))))
		 (pc ((cat clause)
		      (process-type action)
		      (concept Transfer)
		      (agent ((lex "John") (np-type proper)))
		      (benef ((lex "Mary") (np-type proper)))
		      (medium ((lex "book") 
			       (definite no)
			       (np-type common)))))))))

  
  (def-test a4 
      "John takes a book from Mary."
    ((cat discourse-segment)
     (directive ((th ~(John Mary Book Transfer))
		 (if ((force assert)))
		 (pc ((cat clause)
		      (process-type action)
		      (concept Transfer)
		      (agent ((lex "John") (np-type proper)))
		      (benef ((lex "Mary") (np-type proper)))
		      (medium ((lex "book") 
			       (definite no)
			       (np-type common)))))))))

  (format t "~%icon3 installed. 4 tests~%")
  (values))
