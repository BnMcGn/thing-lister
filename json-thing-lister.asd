;;;; json-thing-lister.asd

(asdf:defsystem #:json-thing-lister
  :serial t
  :description "JSON implementation of thing-lister"
  :author "Ben McGunigle <bnmcgn@gmail.com>"
  :license "Apache 2.0"
  :depends-on 
    (#:thing-lister 
     #:webhax
     #:cl-json)
  :components ((:file "json-package")
	       (:file "json-thing")))

