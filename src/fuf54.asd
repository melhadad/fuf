(asdf:defsystem #:fuf54
  :name "FUF and SURGE"
  :author "Michael Elhadad"
  :license "GPL"
  :description "Functional Unification Formalism -- platform for Natural Language Generation / Syntactic Realization."
  :components ((:file "fug54")
	       (:file "vars"
		      :depends-on ("fug54"))
	       (:file "macros"
		      :depends-on ("vars"))
	       (:file "psgraph"
		      :depends-on ("fug54"))
	       (:file "define"
		      :depends-on ("macros" "psgraph"))
	       (:file "path"
		      :depends-on ("define"))
	       (:file "trace"
		      :depends-on ("define"))
	       (:file "generator"
		      :depends-on ("define"))
	       (:file "backtrack"
		      :depends-on ("define"))
	       (:file "external"
		      :depends-on ("define"))
	       (:file "fd-to-graph"
		      :depends-on ("define"))
	       (:file "determine"
		      :depends-on ("define"))
	       (:file "ignore"
		      :depends-on ("define"))
	       (:file "wait"
		      :depends-on ("define"))
	       (:file "alt"
		      :depends-on ("define"))
	       (:file "ralt"
		      :depends-on ("define"))
	       (:file "fset"
		      :depends-on ("define"))
	       (:file "control"
		      :depends-on ("define"))
	       (:file "type"
		      :depends-on ("define"))
	       (:file "pattern"
		      :depends-on ("define"))
	       (:file "findcset"
		      :depends-on ("define"))
	       (:file "graph3"
		      :depends-on ("define"))
	       (:file "top"
		      :depends-on ("define"))
	       (:file "lexicon"
		      :depends-on ("define"))
	       (:file "linearize3"
		      :depends-on ("define"))
	       (:file "checker"
		      :depends-on ("define"))
	       (:file "complexity"
		      :depends-on ("define"))
	       (:file "fdlist"
		      :depends-on ("define"))
	       (:file "continue"
		      :depends-on ("define"))
	       (:file "test"
		      :depends-on ("define"))))

