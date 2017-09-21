;;;; thing-lister.asd

(asdf:defsystem #:thing-lister
  :serial t
  :description "A declarative tool for listing things. Part of webhax."
  :author "Ben McGunigle <bnmcgn@gmail.com>"
  :license "Apache 2.0"
  :depends-on (#:gadgets #:sql-stuff)
  :components ((:file "package")
	       (:file "thing-lister")))
