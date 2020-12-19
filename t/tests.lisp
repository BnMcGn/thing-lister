
(defpackage #:thing-test
  (:use #:cl #:thing-lister #:fiveam #:gadgets))

(in-package :thing-test)

(eval-always 
  (def-suite thing-test))

(in-suite thing-test)

(test basic-label
  (is (