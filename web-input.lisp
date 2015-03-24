(in-package :html-thing-lister)



(defun extract-webspecials-from-parameters (params)
  (with-collectors (norm< spec<)
    (dolist (p params)
      (if
       (ppcre:scan "^~(.*)~" (car p))
       (spec< p)
       (norm< p)))))

(defparameter *html-thing-webspecials* nil)

(defun bound-webspecials ()
  "Creates a list of symbols to be automatically bound as specials by bind-webspecials. Looks in *package* and *html-thing-webspecials*. Individual symbols and lists of symbols in *html-thign-webspecials* will be accepted as is. Only symbols beginning and ending with ~ will be taken from packages therein. All items must be available at macro expansion time."
  (let ((webspecials (if (member *package* *html-thing-webspecials*)
			 *html-thing-webspecials*
			 (cons *package* *html-thing-webspecials*))))
    (collecting
	(labels ((test-collect (x)
		   (let ((sname (symbol-name x)))
		     (and (eq #\~ (first-elt sname))
			  (eq #\~ (last-elt sname))
			  (collect x)))))
	  (dolist (itm webspecials)
	    (cond 
	      ((symbolp itm) (collect itm))
	      ((packagep itm)
	       (do-symbols (s itm)
		 (test-collect s)))
	      ((listp itm)
	       (mapc #'collect itm))))))))

(defmacro bind-webspecials (input &body body)
  `(let 
       ,(collecting
	 (dolist (var (bound-webspecials))
	   (collect `(,var (aif 
			    (assoc ,(symbol-name var) ,input 
				   :test #'eq-symb-upcase)
			    (cdr it)
			    ,var)))))
     ,@body))

(defun string-unless-number (x)
  (handler-case
      (parse-number:parse-number x)
    (sb-int:simple-parse-error () x)
    (org.mapcar.parse-number:invalid-number () x)))

(defun symbol-unless-number (x)
  (let ((val (string-unless-number x)))
    (if (numberp val) 
        val
        (symbolize val))))
