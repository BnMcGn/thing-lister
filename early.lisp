
(in-package :thing-lister)

(defparameter *thing-label-store* nil)
(defparameter *thing-label-filters* nil)
(defparameter *thing-context* nil)
(defparameter *thing-label-flags* nil)
(defparameter *thing-plural* nil)

(defun %pad-spec (labelspec)
  (match labelspec
    ((list x) (list nil nil nil x))
    ((list x y) (list x nil nil y))
    ((list x y z) (list x y nil z))
    ((list _ _ _ _) labelspec)))

(defun %flag-clause (inner flagspec)
  (if flagspec
      `(and (guard x (funcall (condition-release-closure t ',flagspec) x))
	     ,inner)
      inner))

(defun %label-match-clause (thing contextspec flagspec func)
      `(,(%flag-clause 
	  `(list ,(or thing '_) ,(or contextspec '_))
	  flagspec)
	 ,func))

(defun not-null (x)
  (when x t))

(defun %def-labels-core (storvar labelspecs)
    (let ((labelspecs (mapcar #'%pad-spec labelspecs)))
      `(push
	(lambda (thing &optional (context *thing-context*))
	  (match (list thing context)
	    ,@(collecting
	       (dolist (lspec labelspecs)
		 (collect (apply #'%label-match-clause lspec))))
	    (_ :fail)))
	,storvar)))