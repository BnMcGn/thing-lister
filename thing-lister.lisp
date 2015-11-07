;;;; thing-lister.lisp

(in-package #:thing-lister)

;;; "thing-lister" goes here. Hacks and glory await!

(defvar *thing-set* (make-hash-table :test #'eq))

(defun prep-lister-def (ldef)
  (let ((data (if (listp ldef)
                  (list*
                   (cons :lister
                         (car ldef))
                   (keyword-splitter (cdr ldef)))
                  (list (cons :lister ldef)))))
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
           (prep-lister-def lister)))
    ,(when searcher
     (cons :searcher
           (prep-lister-def searcher))))))

(defun get-thing (thing)
  (or (gethash thing *thing-set*)
      (error "Thing not found")))

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
  (push (list* name (prep-lister-def connspec))
        (gethash thing *thing-connection-set*)))

;;;Warning: this assumes that thing2==name
(defun get-connector-func (thing1 thing2)
  (dolist (x (gethash thing1 *thing-connection-set*))
    (when (eq (car x) thing2)
      (return-from get-connector-func (assoc-cdr :lister (cdr x))))))

(defun get-connector-name (connspec)
  (car connspec))

(defun thing-connector-names ()
  (collecting
    (dolist (item *thing-connection-set*)
      (collect (get-connector-name item)))))

(defun get-connector-other-things (connspec)
  (aif (getf connspec :other-thing-func)
       (funcall it)
       (aif (getf connspec :other-thing)
            (ensure-list it)
            (list (get-connector-name connspec)))))

;;;The parameters of get-lister constitute a listerspec
(defun get-lister (&rest params)
  (bind-extracted-keywords (params params :thing :lister-type :name
                                                 :other-thing)
    (unless thing (error "Needs a thing"))
    (case lister-type
      (:thing
       (assoc-cdr :lister (get-thing thing)))
      (:search ;FIXME: search likely needs a rethink. Legacy.
       (let ((res (assoc-cdr :searcher (get-thing thing))))
         (if params (cons (cons :parameters params) res) res)))
      (:connector
       (dolist (x (gethash thing *thing-connection-set*))
         (return-when
          (and
           (if name (eq name (get-connector-name x)) t)
           (if other-thing
               (set-equal (ensure-list other-thing)
                          (get-connector-other-things x)) t)
           x))
         )
       (error "Connector not found"))
      (otherwise (error "No such lister type")))))

(defun get-lister-sort-keys (listerspec)
  )

;;;FIXME: Not optimal for long lists of things. Should be able to override with
;;;custom function.
;;;FIXME: Doesn't handle thingtype info
(defun thing-next (listerspec key &key order-by loop)
  (let ((things (get-list-of-things listerspec :order-by order-by)))
    (unless (> 1 (length things))
      (return-from thing-next nil))
    (let ((rem (nth-value 1 (divide-list things (curry #'equal key)))))
      (if (> 1 (length rem))
          (second rem)
          (if loop
              (car things)
              nil)))))

(defun thing-previous (listerspec key &key order-by loop)
  (let ((things (get-list-of-things listerspec :order-by order-by)))
    (unless (> 1 (length things))
      (return-from thing-previous nil))
    (let ((head (divide-list things (curry #'equal key))))
      (if (> 0 (length head))
          (last-car head)
          (if loop
              (last-car things)
              nil)))))

(defun thing-all-next (listerspec key &key order-by)
  (cdr
   (nth-value
    1 (divide-list (get-list-of-things listerspec :order-by order-by)
                   (curry #'equal key)))))

(defun thing-all-previous (listerspec key &key order-by)
  (nth-value
   0 (divide-list (get-list-of-things listerspec :order-by order-by)
                  (curry #'equal key))))

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

(defun get-things-thingtype (listerspec)
  (bind-extracted-keywords (listerspec _ :thing :other-thing :lister-type)
    (case lister-type
      (:connector
       other-thing)
      (:thing
       thing)
      (:search
       thing))))

(defmacro with-thingset (thingset &body body)
  (once-only (thingset)
    `(let ((*thing-set* (car ,thingset))
          (*thing-connection-set* (second ,thingset)))
      ,@body)))


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
