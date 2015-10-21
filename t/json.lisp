(in-package :test-thing)

(in-suite test-thing)

(test init
  (start-test-app (clack.builder:builder
                   (make-instance 'json-thing
                                  :login-p nil
                                  :thingset *test-thingset*))))

(test (json-thing :depends-on init)
  (is (member "a" (decode-json-from-string
                   (http-request (localhost "json/thing-symbols")))
              :test #'equal))
  (print (decode-json-from-string
          (http-request (localhost "json/things?lister-type=thing&thing=a"))))
  (stop-test-app))


