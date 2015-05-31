;;;; package.lisp

(defpackage #:thing-lister
  (:use #:cl #:gadgets #:optima #:alexandria #:anaphora #:thing-labels)
  (:export 
	   #:def-thing
	   #:get-thing
	   #:thing-summary
	   #:def-thing-connector
	   #:get-connector-func
	   #:*thing-set*
	   #:*thing-connection-set*
	   #:get-list-of-things
	   #:get-things-length
	   #:get-things-thingtype
	   #:thing-call-keyfunc))
