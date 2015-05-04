

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


(defun script-lister (&optional (data *pbit-javascript-sources*))
  (html-out
    (dolist (itm data)
      (htm (:script :src (str itm))))))

(defun css-lister (&optional (data *pbit-css-sources*))
  (html-out
    (dolist (itm data)
      (htm (:link :href (str itm) :rel "stylesheet" :type "text/css")))))


(defparameter *page-part-names*
  '(:@css :@javascript :@site-index :@title :@main-content 
    :@side-content :@site-search :@notifications :@external-links :@logo 
    :@account-info :@footnotes :@copyright :@messages))

(defmacro define-page-parts (name &body parts)
  `(eval-always
     (defun ,name (previous)
       (collecting-hash-table (:existing previous)
	 ,@parts))))

(defun %make-part-func (key keyclauses)
  (let ((lines (collecting
		 (dolist (x keyclauses)
		   (when (eq (car x) key)
		     (dolist (y x)
		       (collect y)))))))
    (when lines
      `(lambda (previous)
	 (collecting-hash-table (:existing previous)
	   ,@lines)))))

(defmacro define-page-template ((name &key wrapper) &body template)
  (multiple-value-bind (keyclauses template)
      (extract-keywords '(:prepend-parts :append-parts) template :in-list t)
    `(eval-always
       (defun ,name ()
	 (values
	  (quote ,(if wrapper 
		      (tree-search-replace (funcall-in-macro wrapper)
					   :match :@inner :value (car template))
		      template))
	  (quote ,(%make-part-func :prepend-parts keyclauses))
	  (quote ,(%make-part-func :append-parts keyclauses)))))))

(defun %render-part (key data params)
  (html-out
    (dolist (x (gethash key data))
      (if (stringp x)
	  (str x)
	  (apply #'funcall-in-macro x params)))))

(defun %render-title (key data params)
  (assert (eq key :@title))
  (html-out
    (:title (str
     (apply #'concatenate 'string
       (collecting 
	 (dolist (x (gethash :@title data))
	   (if (stringp x)
	       (collect x)
	       (collect (apply-in-macro x params))))))))))

(defun %render-javascript (key data params)
  (declare (ignore params))
  (assert (eq key :@javascript))
  (html-out
    (dolist (itm (gethash :@javascript data))
      (htm (:script :src itm)))))

(defun %render-css (key data params)
  (declare (ignore params))
  (assert (eq key :@css))
  (html-out
    (dolist (itm (gethash :@css data))
      (htm (:link :href itm :rel "stylesheet" :type "text/css")))))

(defun %get-render-func (key)
  (assert (member key *page-part-names*))
  (case key
    (:@css '%render-css)
    (:@javascript '%render-javascript)
    (:@title '%render-title)
    (otherwise '%render-part)))

(defun %expand-templates (templates parts-sym params-sym)
  (labels ((walk-tree (tree)
	     (if (atom tree)
		 (cond 
		   ((eq tree :@inner)
		    (unless (cdr templates)
		      (error "Last template should not contain :@inner"))
		    (%expand-templates (cdr templates) parts-sym params-sym))
		   ((member tree *page-part-names*)
		    `(,(%get-render-func tree) ,tree 
		       ,parts-sym ,params-sym))
		   (t tree))
		 (cons (walk-tree (car tree))
		       (walk-tree (cdr tree))))))
    (walk-tree (car templates))))

(defun %collate-parts (parts)
  "Each part will be a function specifier - #'function or a lambda expression,
or an expression that otherwise evaluates to a function. This function will
take a pre-existing hash table as its sole parameter, and will return a hash
table"
  (if (null parts)
      '(make-hash-table)
      (if (functionp-in-macro (car parts))
	  `(funcall-in-macro ,(car parts) ,(%collate-parts (cdr parts)))
	  `(funcall ,(car parts) ,(%collate-parts (cdr parts))))))

(defmacro define-page (name parts templates)
  (let (prepend-parts append-parts)
    (labels ((proc-template (tmpl)
	       (multiple-value-bind (tmp pre app)
		   (funcall-in-macro tmpl)
		 (and pre (push pre prepend-parts))
		 (and app (push app append-parts))
		 tmp)))
      (with-gensyms (parts-sym params-sym)
	(let ((template (%expand-templates 
			 (collecting
			   (dolist (tm templates)
			     (collect (if (functionp-in-macro tm)
					  (proc-template tm)
					  tm))))
			 parts-sym params-sym)))
	  `(let ((,parts-sym 
		  ,(%collate-parts (concatenate 'list
				     prepend-parts parts append-parts))))
	     (,@(if name `(defun ,name) '(lambda)) (&rest ,params-sym)
		,@template)))))))

(define-page-template (page-base) 
    (html-out
     (:html
      (:head
       :@title
       :@javascript
       :@css)
      (:body
       :@inner))))

(define-page-template (two-side-columns :wrapper #'page-base)
  (:prepend-parts 
   (collect :@css "/static/css/style.css"))
  (html-out
    ;Header
    (:div :id "header_wrapper"
      (:div :id "header" :@logo)
      (:div :id "navcontainer"
	    (:ul :id "navlist" :@menu)))
    ;Main content
    (:div :id "left_side"
	  :@site-index :@side-content)
    (:div :id "right_side"
	  :@site-search :@account-info :@external-links)
    (:div :id "content"
	  :@messages :@main-content :@footnotes)
    ;Footer
    (:div :id "footer" :@copyright)))