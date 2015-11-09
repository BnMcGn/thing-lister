;;;; package.lisp

(defpackage #:thing-lister
  (:use #:cl #:gadgets #:alexandria #:anaphora #:thing-labels)
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
	   #:thing-call-keyfunc
	   #:def-db-thing
     #:thing-symbols
     #:*thing-types*
     #:with-thingset
     #:thing-connector-names
     #:thing-next
     #:thing-previous
     #:thing-all-next
     #:thing-all-previous))
