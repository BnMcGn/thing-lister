(in-package :json-thing-lister)



(defun thing-translate (string-thing)
  (or (first-match (thing-symbols) (curry string-thing #'eq-symb))
      string-thing))

(defun label-translate (string-label)
  (or (first-match (label-symbols) (curry string-label #'eq-symb))
      string-label))

(defun listerspec-from-keys (&rest keys)
  (bind-extracted-keywords (keys _ :thing :lister-type :lister-param)
    (list* thing lister-type (if lister-param (cons lister-param nil) nil))))

(defun auto-listerspec ()
  (bind-validated-input
      ((discard #'identity :rest t)
       (thing
        (mkparse-in-list (thing-symbols))
        :key t :required t)
       (lister-type
        (mkparse-in-list *thing-types*)
        :key t :required t)
       (lister-param
        #'identity
        :key t))
    (declare (ignore discard))
    (listerspec-from-keys
     :thing thing :lister-type lister-type :lister-param lister-param)))

(defun create-json-thing-service (app &key (url-base "/thing-lister/"))
  (macrolet ((crout (route bindings &body body)
               `(create-route (app
                               (strcat url-base ,route "/*")
                               :content-type "text/json")
                    ,bindings
                  ,@body)))
    (crout "thing-details"
           ((thing #'identity)
            (key #'identity))
           (encode-json-to-string
            (thing-call-keyfunc
             (thing-translate thing) (string-unless-number key))))
    (crout "get-list-of-things"
           ((params #'identity :rest t))
           (encode-json-to-string
            (apply #'get-list-of-things (auto-listerspec) params)))
    (crout "get-things-length"
           ((params #'identity :rest t))
           (encode-json-to-string
            (apply #'get-things-length (auto-listerspec) params)))))

(defun thing-details (thing key)
  (thing-call-keyfunc (thing-translate thing) key))

(defun %remove-listerspec-keys (alist)
  (nth-value 1 (extract-keywords '(:thing :lister-param :lister-type) alist)))

(defun things (&rest params)
  (apply #'get-list-of-things
         (auto-listerspec)
         (%remove-listerspec-keys params)))

(defun things-length (&rest params)
  (apply #'get-things-length
         (auto-listerspec)
         (%remove-listerspec-keys params)))

(defun things-thingtype (&rest params)
  (apply #'get-things-thingtype
         (auto-listerspec)
         (%remove-listerspec-keys params)))

(defun things-list (&rest params)
  (apply #'get-list-of-things
         (auto-listerspec)
         (%remove-listerspec-keys params)))

(register-json-call 'thing-symbols)
(register-json-call 'thing-details)
(register-json-call 'things)
(register-json-call 'things-length)
(register-json-call 'things-thingtype)
(register-json-call 'things-list)
(register-json-call 'thing-summary)

    '(
      get-thing
      get-connector-func

      thing-label
      thing-label-context
      thing-label-context-plural
      thing-label-plural)

