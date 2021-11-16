;;;; thing-lister.lisp

(in-package #:thing-lister)

;;; "thing-lister" goes here. Hacks and glory await!


;;Number of items that a sidebox should display before adding a More... link
(defparameter *thing-sidebox-length* 10)

;; For sidebar lists
(defparameter *thing-sidebox-width* nil)

;; For full width lists
(defparameter *thing-summary-width* 40)

(defparameter *thing-page-length* 30)
(defparameter *thing-limit* nil)
(defparameter *thing-index* nil)

(defun thing-slice (src)
  (let ((index (or *thing-index* 0)))
    (subseq src index (min (length src) (+ 1 index *thing-limit*)))))

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

;;FIXME: page size is not user adjustable
(defun arrow-pager (position url total)
  "Create a google style pager with single numerals per page."
  (let* ((plength *thing-page-length*)
         (pages (1+ (floor (/ total plength))))
         (currpage (1+ (floor (/ position plength))))
         (lowpage (max 1 (- currpage 6))))
    (format t "arrow-pager ~a ~a ~a ~a" total pages currpage lowpage)
    (when (> pages 1)
      (html-out
        (:div
         :class "thing-pager"
         (if (eq 1 currpage)
             (htm (:span "&lt;"))
             (htm (:a :href
                      (url-reset-keys url (cons "index" (* (- currpage 2) plength)))
                      "&lt;")))
         (dolist (num (range lowpage (1+ pages)))
           (if (eq num currpage)
               (htm (:span (str num)))
               (htm (:a :href
                        (url-reset-keys url (cons "index" (* (1- num) plength)))
                        (str num)))))
         (if (eq pages currpage)
             (htm (:span "&gt;"))
             (htm (:a :href
                      (url-reset-keys url (cons "index" (* currpage plength)))
                      "&gt;"))))))))

(defun display-things-with-pagers (source params item-display-func base-url index)
  (display-thing-block-with-pagers
   source params
   (lambda (items)
     (dolist (itm items)
       (funcall item-display-func itm)))
   base-url index))

(defun display-thing-block-with-pagers (source params display-func base-url index)
  (let ((length (apply source (append params (list :getcount t))))
        (*thing-limit* (or *thing-limit* *thing-page-length*))
        (*thing-index* index))
    (html-out
      (:div
       :class "thing-lister"
       ;;(arrow-pager index base-url length)
       (funcall display-func (apply source params))
       (arrow-pager index base-url length)))))

(defun display-things-sidebar (source params item-display-func main-url
                               &key (class "featurebox_side") label)
  (let ((length (apply source (append params (list :getcount t))))
        (*thing-summary-width* *thing-sidebox-length*)
        (*thing-limit*  (or *thing-limit* *thing-sidebox-length*)))
    ;;FIXME: could display something when empty.
    (unless (zerop length)
      (html-out
        (:div :class class
              (:h3 (str label))
              (dolist (itm (apply source params))
                (funcall item-display-func itm))
              (when (and main-url (< *thing-limit* length))
                (htm
                 (:div :class "navigation"
                       (:a :href main-url "See more")))))))))

