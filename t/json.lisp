(in-package :test-thing)

(in-suite :test-thing)

(test init
  (start-test-app (clack.builder:builder
                   (make-instance 'json-thing
                                  :login-p nil
                                  :thingset *test-thingset*))))

(test (json-thing :depends-on init)
  (with-alternate-thingset *test-thingset*
    (is (member "a" (decode-json-from-string
                     (http-request (localhost "json/thing-symbols")))))))
