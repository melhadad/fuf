(asdf:defsystem #:surge22-test
  :name "Surge 2.2 Tests"
  :author "Michael Elhadad"
  :license "GPL"
  :description "Tests for the SURGE Generation Grammar for English - FUF Platform for Natural Language Generation / Syntactic Realization."
  :depends-on (#:surge22)
  :serial t
  :components (
               (:file "ir")
               (:file "circum")
               (:file "np")
               (:file "np-tests")
               (:file "inputex")
               (:file "t")
               (:file "test")))

(defmethod asdf:perform :after ((o asdf:load-op)
                           (c (eql (asdf:find-system "surge22-test"))))
  (funcall (read-from-string "fug5::surge-test")))


