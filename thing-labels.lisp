

(in-package :thing-lister)


(defvar *thing-label-store* (make-hash-table :test #'eq))
(defvar *thing-context*)

(defun capitalize-first (sym)
  (format nil "~@(~A~)" (symbol-name sym)))

(defvar *thing-label-default* #'capitalize-first)

(defun thing-label (thingspec)
  (let ((thingname
	 (if (listp thingspec) (car thingspec) thingspec)))
    (multiple-value-bind (labl there-p) (gethash thingname *thing-label-store*)
      (if there-p
	  (funcall (car labl) thingname)
	  (funcall *thing-label-default* thingname)))))

(defun deflabel (thing labeller &rest args)
  (declare (ignore args))
  (assert (symbolp thing))
  (let ((labeller (if (stringp labeller)
		      (lambda (&rest x)
			(declare (ignore x))
			labeller)
		      labeller)))
    (setf (gethash thing *thing-label-store*)
	   (list labeller))))
  


;;;
; Context tree stuff
;;;

(defun context-lowest-branch (context tree)
  "Hunt down a tree of hashes for the path in context. Return the deepest matching node, the portion of context that matched, and a boolean to indicate if the whole context sequence was found."
  (if (null context)
      (values tree nil t)
      (aif2only (gethash (car context) tree)
		(multiple-value-bind (tree tags sig)
		    (context-lowest-branch (cdr context) it)
		  (values tree (cons (car context) tags) sig))
		(values tree (list (car context)) nil))))

(defun context-tree-find (thing context storage)
  "Look up the item that matches thing, with the most specifically matching context." 
  (let ((thetree (aif2only (gethash thing storage)
			   it
			   (gethash nil storage))))
    (if thetree
	(let ((short-context context))
	  (loop do
	       (when (= 0 (length short-context))
		 (return (values nil nil)))
	       (aif2only 
		(gethash nil (context-lowest-branch short-context thetree))
		(return (values it t))
		(setf short-context (butlast short-context)))))
	(values nil nil))))

(defun context-tree-ensure (thing context storage)
  "Ensure that the hash tree for thing in storage extends all the way down the path named by context. Returns the lowest hash table of the chain."
  (labels ((list-tail (l1 l2)
	     (if (null l1)
		 l2
		 (list-tail (cdr l1) (cdr l2)))))
    (let* ((xcontext context)
	   (node 
	    (aif2only 
	     (gethash thing storage)
	     (multiple-value-bind (val path sig)
		 (context-lowest-branch context it)
	       (if sig
		   (return-from context-tree-ensure  val)
		   (progn
		     (setf xcontext (list-tail path context))
		     val)))
	     (progn 
	       (setf (gethash thing storage) (make-hash-table :test #'eq))
	       (gethash thing storage)))))
      (loop with currnode = node
	 for itm in xcontext
	 do (let ((newnode (make-hash-table :test #'eq)))
	      (setf (gethash itm currnode) newnode)
	      (setf currnode newnode))
	 finally (return currnode)))))
