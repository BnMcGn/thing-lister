(defpackage #:test-thing
  (:use #:cl #:gadgets
        #:thing-lister
        #:webhax-test-tools
        #:json-thing-lister
        #:cl-json
        #:fiveam
        #:alexandria
        #:webhax-json-call
        #:drakma))

(in-package :test-thing)
(def-suite test-thing)
