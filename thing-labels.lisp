

(in-package :thing-lister)

(defpattern tail-as (arg)
  (let ((it (gensym)))
    `(guard ,it 
	    (funcall (lambda (thing)
		       (labels ((proc (thg) (match thg
					      ((cons _ (guard x (proc x))) t)
					      (,arg t))))
			 (proc thing)))
		     ,it))))



 
(defmacro def-labels (&rest labelspecs)
  (%def-labels-core '*thing-label-store* labelspecs))

(defmacro def-label-filters (&rest labelspecs)
  (%def-labels-core '*thing-label-filters* labelspecs))

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

(defun condition-release-closure (item condition-specs)
  "Creates a closure that will release (return) item if all of the condition-specs are met. Condition-specs are cons pairs: varname, predicate. Varname is presumed to be a special variable. Predicate will be applied to it."
  (lambda (x)
    (declare (ignore x))
    (loop for (var pred) in condition-specs
       do (unless (funcall-in-macro pred (symbol-value var))
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
    (_ _ ((*thing-plural* (function not-null))) #'pluralize))

(defmacro with-label-context-added (context &body body)
  `(let ((*thing-context*
	  (concatenate 'list *thing-context* (ensure-list ,context))))
     ,@body))