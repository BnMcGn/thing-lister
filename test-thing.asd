;;;; test-thing.asd

(asdf:defsystem #:test-thing
  :serial t
  :description "Test suite for thing-lister"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:thing-lister #:json-thing-lister
	#:html-thing-lister
        #:webhax-test-tools
        #:drakma
        #:alexandria
        #:fiveam
        #:webhax
        #:cl-json)
  :components ((:file "t/package")
               (:file "t/core")
	       (:file "t/json")))
