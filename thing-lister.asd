;;;; thing-lister.asd

(asdf:defsystem #:thing-lister
  :serial t
  :description "A tool for listing things. Part of webhax."
  :author "Ben McGunigle <bnmcgn@gmail.com>"
  :license "Apache 2.0"
  :depends-on (#:gadgets #:cl-who #:quri #:cl-utilities #:webhax)
  :components ((:file "package")
               (:file "thing-lister")))
