;;;; thing-lister.lisp

(in-package #:thing-lister)

;;; "thing-lister" goes here. Hacks and glory await!

(defvar *thing-set* (make-hash-table :test #'eq))

(defun prep-lister-def (ldef)
  (let ((data (if (listp ldef)
                  (list*
                   (cons :lister
                         (car ldef))
                   (hu:plist->alist (cdr ldef)))
                  (list (cons :lister ldef)))))
    (if (assoc :limitable data)
        data
        (cons (cons :limitable nil) data))))

;;FIXME: Should be way to indicate some other params on hints.
(defun def-thing (thingname keyfunc summary &rest key-params)
  (bind-extracted-keywords (key-params others :label :lister :searcher)
    (setf (gethash thingname *thing-set*)
          `((:keyfunc . ,keyfunc)
            (:label . ,(or label #'capitalize-first))
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

(defvar *thing-connector-set* (make-hash-table :test #'eq))

(defun def-thing-connector (thing name &rest connspec)
  "thing: the source end of the connector. This thing-connector will appear when
this thing is viewed.
name: the other end of the connector. Can be the symbol of a thing.
 Title of the connector box will be taken from this second thing.
Connspec: first item in the connspec is a function that will be the Lister. It
takes a single index (pkey?), that of the current thing, and returns 0 or more
indices of connected things from the thing indicated by name.
The rest of the connspec consists of a plist of as yet undetermined parameters."
  (setf (hu:hash-get *thing-connector-set* (list thing name)
                     :fill-func #'make-hash-table)
        (prep-lister-def connspec)))

(defun get-connector (thing name)
  (when-let ((conn (hu:hash-get *thing-connector-set* (list thing name))))
    (list*
     (cons :lister-type :connector)
     (cons :thing thing)
     (cons :name name)
     conn)))

(defun get-connector-func (thing name)
  (assoc-cdr :lister (get-connector thing name)))

(defun thing-connector-names ()
  (collecting
      (dolist (v (alexandria:hash-table-values *thing-connector-set*))
        (mapc #'collect (alexandria:hash-table-keys v)))))

(defun get-connector-other-things (thing name)
  (let ((cspec (hu:hget *thing-connector-set* (list thing name))))
    (if-let ((ofunc (assoc-cdr :other-thing-func cspec)))
      (funcall ofunc)
      (if-let ((othing (assoc-cdr :other-thing cspec)))
        (ensure-list othing)
        (list name)))))

;;FIXME: Somewhere, order-by support needs to be implemented/thought out
;;;The parameters of get-lister constitute a listerspec
(defun get-lister (&rest params)
  (bind-extracted-keywords (params _ :thing :lister-type :name)
                                  ; :lister-param) ; :other-thing
    (unless thing (error "Needs a thing"))
    ;;FIXME: Always should throw error on not found?
    (case lister-type
      (:thing
       (assoc-cdr :lister (get-thing thing)))
      (:search ;FIXME: search likely needs a rethink. Legacy.
       (assoc-cdr :searcher (get-thing thing)))
      (:connector
       ;;FIXME: have dropped support for lister-param and other-thing
       ;; as determiners of the listerspec. Reconsider.
       (hu:hash-get *thing-connector-set* (list thing name)))
      (otherwise (error "No such lister type")))))

(defun get-lister-sort-keys (listerspec)
  (getf (apply #'get-lister listerspec) :sort-keys))

(defun add-lister-param (listerspec param)
  (if-let ((parm (assoc :lister-param listerspec)))
    (error (format nil "Parameter already set to ~a" (cdr parm)))
    (cons (cons :lister-param param) listerspec)))

;;;FIXME: Not optimal for long lists of things. Should be able to override with
;;;custom function.
;;;FIXME: Doesn't handle thingtype info
(defun thing-next (listerspec key &key order-by loop)
  (let ((things (get-list-of-things listerspec :order-by order-by)))
    (unless (< 1 (length things))
      (return-from thing-next nil))
    (let ((rem (nth-value 1 (part-on-true (curry #'equal key) things))))
      (if (< 1 (length rem))
          (second rem)
          (if loop
              (car things)
              nil)))))

(defun thing-previous (listerspec key &key order-by loop)
  (let ((things (get-list-of-things listerspec :order-by order-by)))
    (unless (< 1 (length things))
      (return-from thing-previous nil))
    (let ((head (part-on-true (curry #'equal key) things)))
      (if (< 0 (length head))
          (last-car head)
          (if loop
              (last-car things)
              nil)))))

(defun thing-all-next (listerspec key &key order-by)
  "All the things after current thing in the listerspec by a certain order."
  (cdr
   (nth-value
    1 (part-on-true (curry #'equal key)
                   (get-list-of-things listerspec :order-by order-by)))))

(defun thing-all-previous (listerspec key &key order-by)
  (nth-value
   0 (part-on-true (curry #'equal key)
                  (get-list-of-things listerspec :order-by order-by))))

(defun get-list-of-things (listerspec &rest params)
  (let* ((lister (apply #'get-lister listerspec))
         (thingtype (get-things-thingtype listerspec))
         (list-func (assoc-cdr :lister lister))
         (list-func
          (if (and (not (assoc-cdr :limitable lister))
                   (or (getf params :limit) (getf params :offset)))
              (wrap-with-paging-handler list-func)
              list-func))
         (res
          (apply list-func
                 `(,@(when (find :lister-param listerspec)
                           (list (getf listerspec :lister-param)))
                     ,@(proto:strip-keywords params)))))
    (if (eq thingtype :multiple)
        res
        (mapcar (lambda (x) (list x thingtype)) res))))

(defun get-things-length (listerspec &rest params)
  (let ((lister (apply #'get-lister listerspec)))
    (if-let ((ln (assoc-cdr :length lister)))
         (apply ln
                `(,@(when (find :lister-param listerspec)
                          (list (getf listerspec :lister-param)))
                  ,@params))
         (length (apply #'get-list-of-things listerspec params)))))

(defparameter *thing-types* '(:thing :connector :search))

(defun get-things-thingtype (listerspec)
  (bind-extracted-keywords (listerspec _ :thing :name :lister-type)
    (case lister-type
      (:connector
       (car (get-connector-other-things thing name)))
      (:thing
       thing)
      (:search
       thing))))

(defmacro with-thingset (thingset &body body)
  (once-only (thingset)
    `(let ((*thing-set* (car ,thingset))
          (*thing-connector-set* (second ,thingset)))
      ,@body)))

;;;
; db-thing
;;;

;FIXME: think about me: should thing-lister depend on sql-stuff?

(defun def-db-thing (thingname table summary &rest key-params)
  (bind-extracted-keywords (key-params others :sortkeys :search-cols :keyfunc)
    (let ((table (or table thingname)))
      (apply
       #'def-thing
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
                                    params))))
                :limitable t)
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
                                params))))
                    :limitable t))
       others))))

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
