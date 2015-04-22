;;;; html-thing-lister.asd

(asdf:defsystem #:html-thing-lister
  :serial t
  :description "Html implementation of thing-lister"
  :author "Ben McGunigle <bnmcgn@gmail.com>"
  :license "Apache 2.0"
  :depends-on (#:thing-lister 
	#:cl-who 
	#:ystok-uri
	#:cl-ppcre)
  :components ((:file "html-package")
               (:file "html-s-exp")
	       (:file "page-bits")
	       (:file "html-thing")
               (:file "web-input")))

