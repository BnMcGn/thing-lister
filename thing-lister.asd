;;;; thing-lister.asd

(asdf:defsystem #:thing-lister
  :serial t
  :description "Describe thing-lister here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:gadgets #:optima)
  :components ((:file "package")
               (:file "early")
               (:file "thing-labels" :depends-on ("early"))
	       (:file "thing-lister" :depends-on ("thing-labels"))))
