;;;; thing-lister.asd

(asdf:defsystem #:thing-lister
  :serial t
  :description "Describe thing-lister here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:gadgets #:optima #:thing-labels)
  :components ((:file "package")
	       (:file "thing-lister")))
