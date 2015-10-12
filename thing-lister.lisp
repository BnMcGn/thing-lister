;;;; thing-lister.lisp

(in-package #:thing-lister)

;;; "thing-lister" goes here. Hacks and glory await!

(defvar *thing-set* (make-hash-table :test #'eq))

(defun prep-lister-spec (lspec)
  (let ((data (if (listp lspec)
      (list*
       (cons :lister
       (car lspec))
       (keyword-splitter (cdr lspec)))
      (list (cons :lister lspec)))))
    (aif2 (assoc :limitable data)
    data
    (cons (cons :limitable t) data))))

(defun def-thing (thingname keyfunc summary &key label lister searcher)
  (setf (gethash thingname *thing-set*)
  `((:keyfunc . ,keyfunc)
    (:label . ,(or label #'thing-label))
    (:summary . ,summary)
    ,(when lister
     (cons :lister
           (prep-lister-spec lister)))
    ,(when searcher
     (cons :searcher
           (prep-lister-spec searcher))))))

(defun get-thing (thing)
  (gethash thing *thing-set*))

(defun thing-symbols ()
  "List of things currently defined"
  (hash-table-keys *thing-set*))

(defun thing-call-keyfunc (thing &rest params)
  (apply (assoc-cdr :keyfunc (get-thing thing)) params))

(defun thing-summary (thing key)
  (funcall (assoc-cdr :summary (get-thing thing))
     (thing-call-keyfunc thing key)))

(defvar *thing-connection-set* (make-hash-table :test #'eq))

(defun def-thing-connector (thing name &rest connspec)
  (push (list* name (prep-lister-spec connspec))
  (gethash thing *thing-connection-set*)))

(defun get-connector-func (thing1 thing2)
  (dolist (x (gethash thing1 *thing-connection-set*))
    (when (eq (car x) thing2)
      (return-from get-connector-func (assoc-cdr :lister (cdr x))))))

;The parameters of get-lister constitute a listerspec
(defun get-lister (thing ltype &rest params)
  (labels ((add-params (thing params)
             (if params
                 (cons (cons :parameters params) thing)
                 thing)))
    (case ltype
      (:connector
       (dolist (x (gethash thing *thing-connection-set*))
         (when (eq (car x) (car params))
           (return (add-params (cdr x) (cdr params))))))
      (:thing
       (assoc-cdr :lister (get-thing thing)))
      (:search
       (let ((res (assoc-cdr :searcher (get-thing thing))))
         (add-params res params)))
      (otherwise (error "No such lister type")))))

(defun get-list-of-things (listerspec &rest params)
  (let ((lister (apply #'get-lister listerspec)))
    (apply (assoc-cdr :lister lister)
     `(,@(assoc-cdr :parameters lister)
         ,@params))))

(defun get-things-length (listerspec &rest params)
  (let ((lister (apply #'get-lister listerspec)))
    (aif (assoc-cdr :length lister)
         (apply it
                `(,@(assoc-cdr :parameters lister)
                  ,@params))
         (length (apply #'get-list-of-things listerspec params)))))

(defparameter *thing-types* '(:thing :connector :search))

(defun get-things-thingtype (listerspec &optional (index 0))
  (declare (ignore index))
  (case (second listerspec)
    (:connector
     (third listerspec))
    (:thing
     (car listerspec))
    (:search 
     (car listerspec))))

(defmacro with-alternate-thingset (thingset &body body)
  `(let ((*thing-set* ,(car thingset))
        (*thing-connection-set* ,(second thingset)))
    ,@body))


;;;
; db-thing
;;;

;FIXME: think about me: should thing-lister depend on sql-stuff?

(defun def-db-thing (thingname table summary &key sortkeys search-cols keyfunc)
  (let ((table (or table thingname)))
    (def-thing
        thingname
        (or keyfunc
            (lambda (key)
              (sql-stuff:get-assoc-by-pkey table key)))
      summary
      :lister (list
               (lambda (&rest params)
                 (apply #'sql-stuff:get-column
                        table (sql-stuff:get-table-pkey table) params))
               :sortkeys sortkeys
               :length (lambda (&rest params)
                         (sql-stuff:get-count
                          (sql-stuff:unexecuted
                            (apply #'sql-stuff:get-column
                                   table
                                   (sql-stuff:get-table-pkey table) params)))))
      :searcher (when search-cols
                  (list
                   (lambda (text &rest params)
                     (apply #'sql-stuff:fulltext-search text
                            (mapcar (lambda (x)
                                      (sql-stuff:colm table x))
                                    search-cols)
                            params))
                   :sortkeys sortkeys
                   :length
                   (lambda (text &rest params)
                     (sql-stuff:get-count
                      (sql-stuff:unexecuted
                        (apply #'sql-stuff:fulltext-search text
                               (mapcar (lambda (x)
                                         (sql-stuff:colm table x))
                                       search-cols)
                               params)))))))))
