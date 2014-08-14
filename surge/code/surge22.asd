(asdf:defsystem #:surge22
  :name "Surge 2.2"
  :author "Michael Elhadad"
  :license "GPL"
  :description "The SURGE Generation Grammar for English - FUF Platform for Natural Language Generation / Syntactic Realization."
  :depends-on (#:fuf54)
  :serial t
  :components (
	       (:file "tpat")
	       (:file "types")
	       (:file "transitivity")
	       (:file "voice")
	       (:file "adverbial")
	       (:file "mood")
	       (:file "clause")
	       (:file "verb-group")
	       (:file "np")
	       (:file "complex")
	       (:file "determiner")
	       (:file "gr-modular")
	       (:file "special")
	       (:file "nba")))

