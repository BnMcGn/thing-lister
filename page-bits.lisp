

(in-package :html-thing-lister)

(defmacro def-page-bit (name cat label &optional (tag :div))
  `(defmacro ,name (params &body body)
     (declare (ignore params))
     `(html-out
	(,,tag ,,cat ,,label
	       (htm ,@body)))))

(def-page-bit pbit-footer :id "footer")
(def-page-bit pbit-featurebox-side :class "featurebox-side")
(def-page-bit pbit-featurebox-content :class "featurebox-center")
(def-page-bit pbit-content :id "content")

(defmacro pbit-header (params header nav)
  (declare (ignore params))
  `(html-out
     (:div :id "header_wrapper"
	   (:div :id "header"
		 ,header)
	   (:div :id "navcontainer"
		 (:ul :id "navlist"
		      ,nav)))))

(defmacro pbit-content-area (leftside content rightside)
  `(html-out
     (:div :id "left_side"
	   (htm ,@leftside))
     (:div :id "right_side"
	   (htm ,@rightside))
     (pbit-content nil
	   (htm ,@content))))

(defun script-lister ()
  (html-out
    (dolist (itm *pbit-javascript-sources*)
      (htm (:script :src (str itm))))))

(defun css-lister ()
  (html-out
    (dolist (itm *pbit-css-sources*)
      (htm (:link :href (str itm) :rel "stylesheet" :type "text/css")))))

(defvar *pbit-title*)
(defvar *pbit-title-func* (lambda () *pbit-title*))
(defvar *pbit-javascript-sources* nil)
(defvar *pbit-script-handler* #'script-lister)
(defvar *pbit-css-sources* nil)
(defvar *pbit-css-handler* #'css-lister)
(defvar *pbit-menu-func* (lambda () nil))
(defvar *pbit-header-func* (lambda () nil))
(defvar *pbit-footer-func* (lambda () nil))

(defmacro pbit-page-base (params &body body)
  (declare (ignore params))
  `(html-out
     (:html
      (:head
       (funcall *pbit-title-func*)
       (funcall *pbit-script-handler*)
       (funcall *pbit-css-handler*))
      (:body ,@body))))

(defmacro pbit-main-template (params &body body)
  (declare (ignore params))
  `(pbit-page-base nil
     (pbit-header nil (funcall *pbit-header-func*) (funcall *pbit-menu-func*))
     ,@body
     (pbit-footer nil (funcall *pbit-footer-func*))))