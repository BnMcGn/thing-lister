(in-package #:test-thing)

(eval-always
  (defparameter *test-thingset*
    (list (make-hash-table :test #'eq)
          (make-hash-table :test #'eq))))

(defparameter data-a
  '(the quick brown fox jumped over the lazy dogs))

(defparameter data-b
  '(a rapid ochre vulpus lofted above some prone canines))

(with-thingset *test-thingset*
  (def-thing
      'a
      (rcurry #'nth data-a)
    #'princ-to-string
    :lister (list
             (lambda (&rest drop)
               (declare (ignore drop))
               data-a)))
  (def-thing
      'b
    (rcurry #'nth data-b)
    #'princ-to-string
    :lister (list
             (lambda (&rest drop)
               (declare (ignore drop))
               data-b)))
  (def-thing-connector 'a 'same-length
    (lambda (&rest x &aux (xlen (length (nth x data-a))))
      (remove-if-not (lambda (y)
                       (= (length (nth y data-b)) xlen))
                     (range (xlen)))))
  (def-thing-connector 'b 'haz-letters
    (lambda (&rest x &aux (xval (coerce (nth x data-b) 'list)))
      (remove-if-not (lambda (y) (intersection (coerce (nth y data-a) 'list)
                                               xval))
                     (range (length data-a))))))

(def-suite test-thing)

