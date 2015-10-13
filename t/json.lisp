(in-package :test-thing)

(in-suite :test-thing)

(test json-thing
  (with-alternate-thingset *test-thingset*
    (with-clack-app (clack.builder:builder
                     (make-instance 'webhax-json-call:json-call :login-p nil))
      (is (= (car (decode-json-from-string
                   (http-request (localhost "json/thing-symbols"))))
             "a")))))
