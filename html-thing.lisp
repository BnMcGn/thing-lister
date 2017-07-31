

(in-package :html-thing-lister)

(defvar *thing-display-set* (make-hash-table :test #'eq))

(defun def-thing-display (thingname dispfunc)
  (setf (gethash thingname *thing-display-set*)
	dispfunc))

(defvar *html-thing-baseurl* "/things/")
(defvar *html-thing-current-url* "")
;;FIXME: Not sure if this is the best; consider reconsidering.
(defvar *html-thing-user-parts* nil
  "A hook for inserting dependencies into the thing-lister pages. Any parts placed
here will go into all thing-lister pages.")

(defvar *thing-summary-sidebar-width* nil)

(defun %thing-link (thing key)
  (format nil "~athing/~(~a~)/~a" *html-thing-baseurl* thing key))

(defun thing-link (thing key)
  (or (aand (assoc-cdr :html-thing-link (get-thing thing))
            (funcall it key))
      (%thing-link thing key)))

(defun things-link (thing)
  (format nil "~athings/~(~a~)" *html-thing-baseurl* thing))

(defun connector-link (thing thing2 key)
  (format nil "~aconnector/~(~a~)/~(~a~)/~a" *html-thing-baseurl*
	  thing thing2 key))

(defun search-link (thing)
  (format nil "~athing-search/~(~a~)" *html-thing-baseurl* thing))

;Number of items that a sidebox should display before adding a More... link
(defvar *html-thing-sidebox-limit* 10)

(defun connection-display-func (thing name)
  (declare (type symbol thing name))
  (let ((connfunc (get-connector-func thing name))
        (other-thing (car (get-connector-other-things thing name))))
    (declare (type symbol other-thing))
    (lambda (key)
      (multiple-value-bind (keep remainder)
          (divide-on-index (funcall connfunc key) *html-thing-sidebox-limit*)
        (when keep
          (in-label-context thing
            (html-out
              (:div :class "featurebox_side"
                    (:h3 (str (thing-label-context name thing)))
                    (dolist (fkey keep)
                      (htm (:div (:a :href (thing-link other-thing fkey)
                                     ;;FIXME: connspec override of width
                                     (str
                                      (let ((*thing-summary-width*
                                             *thing-summary-sidebar-width*))
                                        (thing-summary other-thing fkey)))))))
                    (when remainder
                      (htm
                       (:div :class "navigation"
                             (:a :href (connector-link thing name key)
                                 "See more"))))))))))))

(defun searchbox-display-func (thing)
  (if (assoc :searcher (get-thing thing))
      (lambda ()
        (html-out
          (:h3 "Search " (str (thing-label thing)))
          (:form :method "get" :action (search-link thing)
                 (:input :type "text" :name "query")
                 (:input :type "submit" :value "Search"))))
      (lambda () nil)))

(defvar *thing-thingtype*)
(defvar *thing-key*)
(define-parts thing-display-parts
  :@inner
  (lambda ()
    (html-out
      (:h2
       (str (concatenate 'string
                         (thing-label *thing-thingtype*) ": "
                         ;;FIXME: implement summary-width settings?
                         (thing-summary *thing-thingtype* *thing-key*))))))
  :@inner
  (lambda ()
    (display-thing-actions *thing-thingtype* *thing-key*))
  :@inner
  (let ((dfunc (or (gethash *thing-thingtype* *thing-display-set*) #'->html)))
    (lambda ()
      (funcall dfunc (thing-call-keyfunc *thing-thingtype* *thing-key*))))
  :@side-content (searchbox-display-func *thing-thingtype*)
  :@side-content
  (lambda ()
    (dolist
        (name (aand (gethash *thing-thingtype* *thing-connection-set*)
                    (alexandria:hash-table-keys it)))
      (funcall (connection-display-func *thing-thingtype* name) *thing-key*)))
  :@title (format nil "Thing: ~a" (thing-label *thing-thingtype*)))

(defun thing-pages (thing key)
  (let ((*thing-thingtype* thing)
        (*thing-key* key))
    (apply #'display-page
           (flatten-when #'listp
                         (list #'thing-display-parts
                               *metaplate-default-parts*
                               *html-thing-user-parts*
                               *metaplate-default-layout*)))))

;;FIXME: Receiving end of source listerspec support is not implemented
(defun url-encode-lister-position (listerspec
                                   &key
                                     (extension "source")
                                     (uri ""))
  ;;FIXME: Order-by not implemented generally. Verify that the correct name
  ;; here. Should be in listerspec, but not sure
  (let* ((keys '(:thing :name :lister-param :lister-type :lister-orderby))
         (data
          (collecting
              (dolist (k keys)
                (let ((newkey (format nil "~a-~(~a~)" extension k)))
                  (when-let ((val (getf listerspec k)))
                    (collect (cons newkey (to-lowercase val)))))))))
    (apply #'url-reset-keys uri data)))

(defun thing-link/source (thing key)
  "Create a thing link that passes on info about the current listerspec."
  (url-encode-lister-position *listerspec* :uri (thing-link thing key)))

;;;;;;;;;;
;Lister
;;;;;;;;;;

(defun url-reset-keys (url &rest newvals)
  (let ((purl (quri:uri url))
        (keys (mapcar #'car newvals)))
    (setf
     (quri:uri-query-params purl)
     (concatenate
      'list
      (remove-if-member (quri:uri-query-params purl) keys
                        :key (lambda (x) (car x))
                        :test #'equal)
      newvals))
    (quri:render-uri purl)))

;;FIXME: Could use webhax-validate?
(def-webspecial ~pageindex~ nil (>>integer :emsg "~pageindex~: not an integer"))
(def-webspecial ~pagequantity~ nil
  (>>integer :emsg "~pagequantity~: not an integer"))

(defun simple-pager-display (&key total-length (url *html-thing-current-url*)
			     page-quantity (page-index 1))
  (let* ((page-quantity (or ~pagequantity~ page-quantity))
         (page-index (or ~pageindex~ page-index))
         (next-val (+ page-index (or page-quantity 1)))
         (prev-val (- page-index (or page-quantity 1)))
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
                                (+ page-index (or page-quantity 1))))
           (str " Next &gt;")
           (htm
            (:a
             :href (url-reset-keys
                    url
                    `("~pageindex~" . ,next-val)
                    url-params)
             " Next &gt;")))))))

(define-parts lister-parts
  :@title
  (lambda ()
    (let ((lspec *listerspec*))
      (format nil "Things: ~a"
              (funcall (assoc-cdr :label (get-thing (getf lspec :thing)))
                       (getf lspec :thing)))))
  :@inner
  (lambda ()
    (let* ((lspec *listerspec*)
           (llength (get-things-length lspec))
           ;;FIXME: Set ~pagequantity~ default somewhere
           (~pagequantity~ (or ~pagequantity~ 40)))
      (simple-pager-display :total-length llength)
      (html-out
        (loop for (itm thingtype) in (get-list-of-things
                                      lspec :limit ~pagequantity~
                                      :offset (1- (or ~pageindex~ 1)))
          do (htm (:div :class "thing_lister"
                        (:span
                         (:a :href (thing-link/source thingtype itm)
                             ;;FIXME: Summary-width settings?
                             (str (thing-summary thingtype itm)))
                         (display-thing-actions thingtype itm))))))
      (simple-pager-display :total-length llength))))

(defparameter *listerspec* nil)
(defun lister-page (listerspec)
  (let ((*listerspec* listerspec))
    (apply #'display-page
           (flatten-when #'listp
                         (list #'lister-parts
                               *metaplate-default-parts*
                               *html-thing-user-parts*
                               *metaplate-default-layout*)))))

;;;;
; Actions
;;;;

(defvar *html-thing-actions* nil)

(defun def-thing-action (thing action pscode)
  (push (list thing action pscode) *html-thing-actions*))

(defun get-thing-actions (thing)
  (remove-if-not (lambda (x) (eq (car x) thing)) *html-thing-actions*))

(defun display-thing-actions (thingtype thing)
  (dolist (act (get-thing-actions thingtype))
    (html-out
      (:button
       :onclick (ps-inline* `(funcall ,(third act) ,thing))
       (str (thing-label (second act)))))))


;;;;
;; Thing-lister as clack middleware
;;;;

(define-middleware thing-component ()
  (init
   (dolist (sym (thing-symbols))
     (register-link sym (things-link sym) :label (capitalize-first sym)))
   ;;FIXME: Search link to be added?
   (main
    (url-case
      (:things
       (with-output-to-string (*webhax-output*)
         (url-case
           (:thing
            (bind-validated-input
                ((thing (:pickone :options (thing-symbols)))
                 (key (:or :integer :string)))
              ;;FIXME: webspecials?
              (thing-pages thing key)))
           (:things
            (bind-validated-input
                ((thing (:pickone :options (thing-symbols))))
              (lister-page (list :thing thing :lister-type :thing))))
           (:thing-search
            (bind-validated-input
                ((thing (:pickone :options (thing-symbols)))
                 &key
                 (query :string))
              (let ((*html-thing-current-url* (url-from-env *web-env*)))
                (lister-page (list :thing thing :lister-type :search
                                   :lister-param query)))))
           (:connector
            (bind-validated-input
                ((thing (:pickone :options (thing-symbols)))
                 ;;FIXME: Should also include thing-symbols under name? Can't
                 ;; remember how that is implemented.
                 (name (:pickone :options (thing-connector-names)))
                 (key :integer))
              (lister-page (list :thing thing :lister-type :connector
                                 :name name :lister-param key)))))))
      (otherwise
       (call-endware))))))
