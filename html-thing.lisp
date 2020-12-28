;; Non functional stuff, kept for historical interest, random inspiration, etc

(in-package :html-thing-lister)

;;FIXME: Not sure if this is the best; consider reconsidering.
(defvar *html-thing-user-parts* nil
  "A hook for inserting dependencies into the thing-lister pages. Any parts placed
here will go into all thing-lister pages.")

(defun url-encode-lister-position (listerspec
                                   &key
                                     (extension "source")
                                     (uri ""))
  ;;FIXME: Order-by not implemented generally. Verify that the correct name
  ;; here. Should be in listerspec, but not sure
  (let* ((keys '(:thing :name :lister-param :lister-type :lister-orderby))
         (data
          (cl-utilities:collecting
              (dolist (k keys)
                (let ((newkey (format nil "~a-~(~a~)" extension k)))
                  (when-let ((val (getf listerspec k)))
                    (cl-utilities:collect (cons newkey (to-lowercase val)))))))))
    (apply #'url-reset-keys uri data)))




;;;;
;; Thing-lister as clack middleware
;;;;

(define-middleware thing-component ()
  (dolist (sym (thing-symbols))
    (register-link sym (things-link sym) :label (capitalize-first sym)))
  ;;FIXME: Search link to be added?
  (lambda ()
    (url-case
      (:things
       (with-output-to-string (*webhax-output*)
         ;;FIXME: binding webspecials here because no one else is using them now.
         (bind-webspecials *key-web-input*
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
                                   :name name :lister-param key))))))))
      (otherwise
       (call-endware)))))


