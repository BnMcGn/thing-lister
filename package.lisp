;;;; package.lisp

(defpackage #:thing-lister
  (:use #:cl #:gadgets)
  (:export #:deflabel
	   #:thing-label
	   #:def-thing
	   #:get-thing
	   #:def-thing-connector
	   #:get-connector-func
	   #:*thing-set*
	   #:*thing-connection-set*))
