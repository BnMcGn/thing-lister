

(in-package :thing-lister)


(defvar *thing-label-store*)
(defvar *thing-label-filters*)
(defvar *thing-context*)
(defvar *thing-label-flags*)
(defvar *thing-plural*)

(defun thing-label (thingspec &optional (context *thing-context*))
  (let* ((*thing-context* context)
	 (label
	  (loop for matcher in *thing-label-store*
	      for res = (funcall matcher thingspec context)
	      until (not (eq res :fail))
	      finally (return (if (eq res :fail) nil res))))
	 (label (if (stringp label) label (funcall label thingspec)))
	 (filter
	  (loop for matcher in *thing-label-filters*
	     for res = (funcall matcher thingspec context)
	     until (not (eq res :fail))
	     finally (return (if (eq res :fail) nil res)))))
    (if filter
	(funcall filter label)
	label)))


(defpattern tail-as (arg)
  (let ((it (gensym)))
    `(guard ,it 
	    (funcall (lambda (thing)
		       (labels ((proc (thg) (match thg
					      ((cons _ (guard x (proc x))) t)
					      (,arg t))))
			 (proc thing)))
		     ,it))))

;;;;
; def-labels macro stuff
;;;;

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
 
(defmacro def-labels (&rest labelspecs)
  (%def-labels-core '*thing-label-store* labelspecs))

(defmacro def-label-filters (&rest labelspecs)
  (%def-labels-core '*thing-label-filters* labelspecs))

(defun condition-release-closure (item condition-specs)
  "Creates a closure that will release (return) item if all of the condition-specs are met. Condition-specs are cons pairs: varname, predicate. Varname is presumed to be a special variable. Predicate will be applied to it."
  (lambda (x)
    (declare (ignore x))
    (loop for (var pred) in condition-specs
       do (unless (funcall pred (symbol-value var))
	    (return nil))
       finally (return item))))

(defun capitalize-first (sym)
  (format nil "~@(~A~)" (symbol-name sym)))

(defun context-label (sym)
  (format nil "~a: ~a" (capitalize-first (car *thing-context*)) 
	  (capitalize-first sym)))

(defun pluralize (label)
  (format nil "~as" label))

(def-labels
    (_ (list _) #'context-label)
    (#'capitalize-first))

(def-label-filters
    (_ _ ((*thing-plural* #'not-null)) #'pluralize))