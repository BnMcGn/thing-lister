(in-package :json-thing-lister)

(defun thing-translate (string-thing)
  (or (first-match (thing-symbols) (curry  #'eq-symb string-thing))
      string-thing))

(defun label-translate (string-label)
  (or (first-match (label-symbols) (curry #'eq-symb string-label))
      string-label))

(defun listerspec-from-keys (&rest keys)
  (bind-extracted-keywords (keys _ :thing :lister-type :lister-param)
    (list* thing lister-type (if lister-param (cons lister-param nil) nil))))

(defun listerspec-from-keys (&rest keys)
  (bind-extracted-keywords (keys _ :thing :lister-type :lister-param
                                          :lister-name :other-thing)
    (strip-keywords
     (list :thing thing :lister-type lister-type :lister-param
           lister-param :name lister-name :other-thing other-thing))))

(defun auto-listerspec ()
  (print *regular-web-input*)
  (bind-validated-input
      ((discard (lambda (x) (values x t)) :rest t)
       (thing
        (mkparse-in-list (thing-symbols))
        :key t :required t)
       (lister-type
        (mkparse-in-list *thing-types*)
        :key t :required t)
       (lister-param
        (lambda (x) (values x t))
        :key t)
       (lister-name
        (mkparse-in-list (thing-connector-names))
        :key t)
       (other-thing
        (mkparse-in-list (thing-symbols))
        :key t))
    (declare (ignore discard))
    (listerspec-from-keys
     :thing thing :lister-type lister-type :lister-param lister-param
     :lister-name lister-name :other-thing other-thing)))

(defun thing-details (thing key)
  (thing-call-keyfunc (thing-translate thing) key))

(defun %remove-listerspec-keys (alist)
  (nth-value 1 (extract-keywords '(:thing :lister-param :lister-type) alist)))

(defclass json-thing (json-call)
  ((thingset :initform (list *thing-set* *thing-connection-set*)
             :initarg :thingset)))

(defmethod execute ((this json-thing))
  (with-thingset (slot-value this 'thingset)
   (call-next-method)))

(defmacro defun/listerspec (name function)
  `(defun ,name (&rest params)
     (apply (function ,function)
            (auto-listerspec)
            (%remove-listerspec-keys params))))

(defun/listerspec things get-list-of-things)
(defun/listerspec things-length get-things-length)
(defun/listerspec things-thingtype get-things-thingtype)
(defun/listerspec previous thing-previous)
(defun/listerspec next thing-next)
(defun/listerspec all-next thing-all-next)
(defun/listerspec all-previous thing-all-previous)

(register-json-call 'thing-symbols)
(register-json-call 'thing-details)
(register-json-call 'things)
(register-json-call 'things-length)
(register-json-call 'things-thingtype)
(register-json-call 'previous)
(register-json-call 'next)
(register-json-call 'all-next)
(register-json-call 'all-previous)
(register-json-call 'thing-summary)
(register-json-symbols '(:thing :lister-type :lister-param :lister-name
                         :other-thing))
(register-json-symbol-func #'thing-symbols)


