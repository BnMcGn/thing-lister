;;;; thing-lister.lisp

(in-package #:thing-lister)

;;; "thing-lister" goes here. Hacks and glory await!

(defvar *thing-set* (make-hash-table :test #'eq))

(defun def-thing (thingname keyfunc summary &key label)
  (setf (gethash thingname *thing-set*)
	(list
	 (cons :keyfunc
	       keyfunc)
	 (cons :label
	       (or label #'thing-label))
	 (cons :summary
	       summary))))

(defun get-thing (thing)
  (gethash thing *thing-set*))

(defvar *thing-connection-set* (make-hash-table :test #'eq))

(defun def-thing-connector (thing1 thing2 connfunc)
  (push (list thing2 connfunc)
	(gethash thing1 *thing-connection-set*)))

(defun get-connector-func (thing1 thing2)
  (dolist (x (gethash thing1 *thing-connection-set*))
    (when (eq (car x) thing2)
      (return-from get-connector-func (second x)))))
