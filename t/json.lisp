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
  (is  (= 0 (car
             (decode-json-from-string
              (http-request
               (localhost "json/things?lister-type=thing&thing=a"))))))
  (is (= 9 (decode-json-from-string
            (http-request
             (localhost "json/things-length?lister-type=thing&thing=b")))))
  (is (equal "b" (decode-json-from-string
            (http-request
             (localhost "json/things-thingtype?lister-type=thing&thing=b")))))
  (is (equal "ochre"
             (decode-json-from-string
              (http-request
               (localhost "json/thing-details/b/2")))))
  (is (equal "FOX"
             (decode-json-from-string
              (http-request
               (localhost "json/thing-summary/a/3")))))
  (is (= 7 (car
            (decode-json-from-string
             (http-request
              (localhost "json/things?lister-type=connector&lister-name=haz-letters&thing=b&lister-param=0"))))))
  (decode-json-from-string
   (http-request
    (localhost "json/next/3?lister-type=thing&thing=b")))
  (stop-test-app))


 
