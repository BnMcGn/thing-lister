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

;;FIXME: Should be way to indicate some other params on hints.
(defun def-thing (thingname keyfunc summary &rest key-params)
  (bind-extracted-keywords (key-params others :label :lister :searcher)
    (setf (gethash thingname *thing-set*)
          `((:keyfunc . ,keyfunc)
            (:label . ,(or label #'thing-label))
            (:summary . ,summary)
            ,(when lister
                   (cons :lister
                         (prep-lister-def lister)))
            ,(when searcher
                   (cons :searcher
                         (prep-lister-def searcher)))
            ,@(hu:plist->alist others)))))

(defun get-thing (thing)
  (or (gethash thing *thing-set*)
      (error "Thing not found")))

(defun thing-symbols ()
  "List of things currently defined"
  (hash-table-keys *thing-set*))

(defun thing-call-keyfunc (thing &rest params)
  (apply (assoc-cdr :keyfunc (get-thing thing)) params))

(defvar *thing-summary-width* nil
  "A suggested character width within which the summary should be kept. Can
be NIL or an integer. May vary depending on context of the summary. Gives
summary functions a chance to intelligently abbreviate")

(defun thing-summary (thing key)
  (let ((res (funcall (assoc-cdr :summary (get-thing thing))
                      (thing-call-keyfunc thing key))))
    (unless (and res (stringp res))
      (error
       (format nil
               "Summary function for thingtype ~a should return string or NIL"
               thing)))
    res))

(defvar *thing-connection-set* (make-hash-table :test #'eq))

(defun def-thing-connector (thing name &rest connspec)
  "thing: the source end of the connector. This thing-connector will appear when
this thing is viewed.
name: the other end of the connection. Can be the symbol of a thing.
 Title of the connector box will be taken from this second thing.
Connspec: first item in the connspec is a function that will be the Lister. It
takes a single index (pkey?), that of the current thing, and returns 0 or more
indices of connected things from the thing indicated by name.
The rest of the connspec consists of a plist of as yet undetermined parameters."
  (setf (hu:hash-get/extend *thing-connection-set* (list thing name))
        (prep-lister-def connspec)))

(defun get-connector-func (thing name)
  (assoc-cdr
   :lister
   (hu:hash-get *thing-connection-set* (list thing name))))

(defun thing-connector-names ()
  (collecting
      (dolist (v (alexandria:hash-table-values *thing-connection-set*))
        (mapc #'collect (alexandria:hash-table-keys v)))))

(defun get-connector-other-things (thing name)
  (let ((cspec (hu:hget *thing-connection-set* (list thing name))))
    (aif (assoc-cdr :other-thing-func cspec)
         (funcall it)
         (aif (assoc-cdr :other-thing cspec)
              (ensure-list it)
              (list name)))))

;;FIXME: Somewhere, order-by support needs to be implemented/thought out
;;;The parameters of get-lister constitute a listerspec
(defun get-lister (&rest params)
  (bind-extracted-keywords (params _ :thing :lister-type :name
                                   :lister-param) ; :other-thing
    (unless thing (error "Needs a thing"))
    ;;FIXME: Always should throw error on not found?
    (case lister-type
      (:thing
       (assoc-cdr :lister (get-thing thing)))
      (:search ;FIXME: search likely needs a rethink. Legacy.
       (let ((res (assoc-cdr :searcher (get-thing thing))))
         (if lister-param
             (cons (cons :parameters lister-param) res)
             res)))
      (:connector
       ;;FIXME: have dropped support for lister-param and other-thing
       ;; as determiners of the listerspec. Reconsider.
       (hu:hash-get *thing-connection-set* (list thing name)))
      (otherwise (error "No such lister type")))))

(defun get-lister-sort-keys (listerspec)
  (getf (apply #'get-lister listerspec) :sort-keys))

;;;FIXME: Not optimal for long lists of things. Should be able to override with
;;;custom function.
;;;FIXME: Doesn't handle thingtype info
(defun thing-next (listerspec key &key order-by loop)
  (let ((things (get-list-of-things listerspec :order-by order-by)))
    (unless (< 1 (length things))
      (return-from thing-next nil))
    (let ((rem (nth-value 1 (divide-list (curry #'equal key) things))))
      (if (< 1 (length rem))
          (second rem)
          (if loop
              (car things)
              nil)))))

(defun thing-previous (listerspec key &key order-by loop)
  (let ((things (get-list-of-things listerspec :order-by order-by)))
    (unless (< 1 (length things))
      (return-from thing-previous nil))
    (let ((head (divide-list (curry #'equal key) things)))
      (if (< 0 (length head))
          (last-car head)
          (if loop
              (last-car things)
              nil)))))

(defun thing-all-next (listerspec key &key order-by)
  "All the things after current thing in the listerspec by a certain order."
  (cdr
   (nth-value
    1 (divide-list (curry #'equal key)
                   (get-list-of-things listerspec :order-by order-by)))))

(defun thing-all-previous (listerspec key &key order-by)
  (nth-value
   0 (divide-list (curry #'equal key)
                  (get-list-of-things listerspec :order-by order-by))))

(defun get-list-of-things (listerspec &rest params)
  (let ((lister (apply #'get-lister listerspec)))
    (apply (assoc-cdr :lister lister)
           `(,@(strip-keywords (assoc-cdr :parameters lister))
             ,@(strip-keywords params)))))

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
                                   (sql-stuff:get-table-pkey table)
                                   params)))))
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

(defun wrap-with-paging-handler (func)
  "Func is a function that returns a list. Returns that function wrapped in a closure that strips the :offset and :limit keywords from its parameters, calls the function with the remainder, and limits the resulting list accordingly."
  (lambda (&rest params)
    (bind-extracted-keywords (params xparams :offset :limit)
      (let ((res (apply func xparams)))
        (when offset
          (setf res (nthcdr offset res)))
        (if (and limit (< limit (length res)))
            (subseq res 0 limit)
            res)))))
