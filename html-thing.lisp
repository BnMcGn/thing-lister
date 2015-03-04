

(in-package :html-thing-lister)

(defvar *thing-display-set* (make-hash-table :test #'eq))

(defun def-thing-display (thingname dispfunc)
  (setf (gethash thingname *thing-display-set*)
	dispfunc))

(defvar *html-thing-baseurl* "/")

(defun thing-link (thing key)
  (format nil "~athing/~(~a~)/~a" *html-thing-baseurl* thing key))

(defun connection-display-func (thing thing2)
  (let ((label1 (funcall (assoc-cdr :label (get-thing thing)) thing))
	(label2 (funcall (assoc-cdr :label (get-thing thing2)) thing2))
	(summary (assoc-cdr :summary (get-thing thing2)))
	(connfunc (get-connector-func thing thing2))
	(fthingfunc (assoc-cdr :keyfunc (get-thing thing2))))
    (lambda (key)
      (pbit-featurebox-side nil
	(:h3 (str (format nil "~a -> ~a" label1 label2)))
	(dolist (fkey (funcall connfunc key))
	  (htm (:a :href (thing-link thing2 fkey) 
		   (str (funcall summary (funcall fthingfunc fkey))))))))))
				
(defun get-display-functions (thing)
  (values
   (let ((dfunc (or (gethash thing *thing-display-set*) #'->html)))
     (lambda (key)
       (funcall dfunc (funcall (assoc-cdr :keyfunc (get-thing thing)) key))))
   (collecting
     (dolist (conn (gethash thing *thing-connection-set*))
       (collect (connection-display-func thing (car conn)))))))

(defun thing-display-core (thing key)
  (multiple-value-bind (maindisp conns) (get-display-functions thing)
    (pbit-content-area
     nil
     ((funcall maindisp key))
     ((dolist (c conns)
       (funcall c key))))))

(defun thing-display-page (thing key)
  (let ((*pbit-title* (format nil "Thing: ~a" 
			      (funcall (assoc-cdr :label (get-thing thing))
				       thing)))
	(*pbit-css-sources* (list "/static/css/style.css")))
    (pbit-main-template nil
      (thing-display-core thing key))))