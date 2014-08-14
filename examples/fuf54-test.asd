(asdf:defsystem #:fuf54-test
  :name "FUF Examples and Tests"
  :author "Michael Elhadad"
  :license "GPL"
  :description "Tests for Functional Unification Formalism -- Platform for Natural Language Generation / Syntactic Realization."
  :depends-on (#:fuf54)
  :serial t
  :components (
	       (:file "tpat")
	       (:file "gr0")
	       (:file "ir0")
	       (:file "gr1")
	       (:file "ir1")
	       (:file "gr2")
	       (:file "ir2")
	       (:file "gr3")
	       (:file "ir3")
	       (:file "gr4")
	       (:file "ir4")
	       (:file "gr5")
	       (:file "ir5")
	       (:file "gr6")
	       (:file "ir6")
	       (:file "gr7")
	       (:file "ir7")
	       (:file "gr8" :depends-on ("tpat"))
	       (:file "ir8")
	       (:file "gr9" :depends-on ("tpat"))
	       (:file "ir9")
	       (:file "gr10" :depends-on ("tpat"))
	       (:file "ir10")
	       (:file "ir-bk-class")
	       (:file "gcon3")
	       (:file "icon3")
	       (:file "tests")))


(defmethod asdf:perform :after ((o asdf:load-op)
                           (c (eql (asdf:find-system "fuf54-test"))))
  (funcall (read-from-string "fug5::fuf54-test")))
