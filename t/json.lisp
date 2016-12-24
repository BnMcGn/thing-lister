(in-package :test-thing)

(in-suite test-thing)


(let ((comp (funcall (webhax-json-call::json-call-component)
                     (lambda (&rest x)
                       (declare (ignore x))))))
  (defun call-json-component (url)
    (with-thingset *test-thingset*
      (car (third (funcall comp (webhax-core:env-from-url url)))))))

(test (json-thing)
  (is (member "a" (decode-json-from-string
                   (call-json-component "json-call/thing-symbols"))
              :test #'equal))
  (is  (= 0 (car
             (decode-json-from-string
              (call-json-component "json-call/things?lister-type=thing&thing=a")))))
  (is (= 9 (decode-json-from-string
            (call-json-component "json-call/things-length?lister-type=thing&thing=b"))))
  (is
   (equal "b"
          (decode-json-from-string
           (call-json-component
            "json-call/things-thingtype?lister-type=thing&thing=b"))))
  (is (equal "ochre"
             (decode-json-from-string
              (call-json-component "json-call/thing-details/b/2"))))
  (is (equal "FOX"
             (decode-json-from-string
              (call-json-component "json-call/thing-summary/a/3"))))
  (is (= 7 (car
            (decode-json-from-string
             (call-json-component "json-call/things?lister-type=connector&lister-name=haz-letters&thing=b&lister-param=0"))))))


