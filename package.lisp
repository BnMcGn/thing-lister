;;;; package.lisp

(defpackage #:thing-lister
  (:use #:cl #:gadgets #:optima #:alexandria #:anaphora)
  (:export #:def-labels
	   #:def-label-filters
	   #:not-null
	   #:tail-as
	   #:thing-label
	   #:def-thing
	   #:get-thing
	   #:thing-summary
	   #:def-thing-connector
	   #:get-connector-func
	   #:*thing-set*
	   #:*thing-connection-set*
	   #:*thing-context*
	   #:get-list-of-things
	   #:get-things-length
	   #:get-things-thingtype
	   #:thing-call-keyfunc
	   #:with-label-context-added))
