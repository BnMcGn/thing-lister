

(in-package :html-thing-lister)

(defvar *thing-display-set* (make-hash-table :test #'eq))

(defun def-thing-display (thingname dispfunc)
  (setf (gethash thingname *thing-display-set*)
	dispfunc))

(defvar *html-thing-baseurl* "/")
(defvar *html-thing-current-url* "")

(defun thing-link (thing key)
  (format nil "~athing/~(~a~)/~a" *html-thing-baseurl* thing key))

(defun connection-display-func (thing thing2)
  (let ((connfunc (get-connector-func thing thing2)))
    (lambda (key)
      (pbit-featurebox-side nil
	(:h3 (str (thing-label thing2)))
	(dolist (fkey (funcall connfunc key))
	  (htm (:div (:a :href (thing-link thing2 fkey) 
		   (str (thing-summary thing2 fkey))))))))))
				
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
       ((let ((*thing-context* 
	       (concatenate 'list *thing-context* (list thing))))
	 (dolist (c conns)
	   (funcall c key))))
       ((html-out
	  (:h2 (str (concatenate 
		     'string 
		     (thing-label thing) ": " (thing-summary thing key)))))
	(funcall maindisp key))
       nil)))

(defun thing-display-page (thing key)
  (let ((*pbit-title* (format nil "Thing: ~a" 
			      (funcall (assoc-cdr :label (get-thing thing))
				       thing)))
	(*pbit-css-sources* (list "/static/css/style.css")))
    (pbit-main-template nil
      (thing-display-core thing key))))

;;;;;;;;;;
;Lister
;;;;;;;;;;

(defun url-reset-keys (url &rest newvals)
  (let ((purl (ystok.uri:parse-uri url))
        (keys (mapcar #'car newvals)))
    (setf 
     (ystok.uri:uri-query purl)
     (concatenate 
      'list
      (remove-if-member (ystok.uri:uri-query purl) keys 
                        :key (lambda (x) (car x))
                        :test #'equal)
      newvals))
    (ystok.uri:render-uri purl nil t t)))

(defvar ~pageindex~)
(defvar ~pagequantity~)

(defun simple-pager-display (&key total-length (url *html-thing-current-url*) 
			     page-quantity (page-index 1))
  (let* ((page-quantity (or ~pagequantity~ page-quantity))
         (page-index (or ~pageindex~ page-index))
         (next-val (+ page-index (or page-quantity 1)))
         (prev-val (- page-index (or page-quantity 1)))
         ;(url (or url (get-request-url))) ;FIXME: Long term solution?
         (url-params (if page-quantity
                         `("~pagequantity~" . ,page-quantity)
                         nil)))
    (html-out
      (:span
       (if (> prev-val 0)
           (htm 
            (:a 
             :href (url-reset-keys 
                    url 
                    `("~pageindex~" . ,prev-val)
                    url-params)
             "&lt; Previous"))
           (str "&lt; Previous"))
       (if (and total-length (< total-length 
                                (* page-index (or page-quantity 1))))
           (str " Next &gt;")
           (htm
            (:a
             :href (url-reset-keys
                    url
                    `("~pageindex~" . ,next-val)
                    url-params)
             " Next &gt;")))))))

(defun lister-page (lspec &rest params)
  (declare (ignore params))
  (let ((*pbit-title* (format nil "Things: ~a" 
			      (funcall (assoc-cdr :label 
						  (get-thing (car lspec)))
				       (car lspec))))
	(*pbit-css-sources* (list "/static/css/style.css"))
	(~pagequantity~ (or ~pagequantity~ 3)) ;FIXME: Set default somewhere
	(llength (get-things-length lspec))
	(thingtype (get-things-thingtype lspec)))
    (pbit-main-template nil
      (pbit-content-area
       nil
       ((simple-pager-display :total-length llength)
	(html-out
	  (dolist (itm (get-list-of-things 
			lspec :limit ~pagequantity~
			:offset (* (1- (or ~pageindex~ 1)) ~pagequantity~)))
	    (htm (:div 
		  (:a :href (thing-link thingtype itm)
		      (str (thing-summary thingtype itm)))))))
	(simple-pager-display :total-length llength))
       nil))))
       
