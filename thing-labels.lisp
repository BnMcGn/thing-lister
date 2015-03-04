

(in-package :thing-lister)


(defvar *thing-label-store* (make-hash-table :test #'eq))
(defvar *thing-context*)
(defvar *thing-label-flags*)

(defun capitalize-first (sym)
  (format nil "~@(~A~)" (symbol-name sym)))

(defvar *thing-label-default* #'capitalize-first)

(defun thing-label (thingspec &optional (context *thing-context*))
  (dolist (x (context-tree-find thingspec context *thing-label-store*))
    (awhen (most-qualified-item x)
      (funcall it thingspec)
      (return)))
  (funcall *thing-label-default* thingspec));Reached if dolist doesn't find one

(defun deflabel (thing labeller &rest args) 
  (assert (symbolp thing))
  (let ((labeller (if (stringp labeller)
		      (lambda (&rest x)
			(declare (ignore x))
			labeller)
		      labeller)))
    (autobind-specials ((context label-flags) args *thing-)
      (save-labeller-to-loc 
       (context-tree-ensure thing context *thing-label-store*)
       labeller label-flags))))
  
;;;
; Conditional label stuff
;;;

(defun condition-release-closure (item &rest condition-specs)
  "Creates a closure that will release (return) item if all of the condition-specs are met. Condition-specs are cons pairs: varname, predicate. Varname is presumed to be a special variable. Predicate will be applied to it."
  (lambda ()
    (loop for (var pred) in condition-specs
	 do (unless (funcall pred (symbol-value var))
	      (return nil))
	 finally (return item))))

(defun save-labeller-to-loc (treehash labeller condition-specs)
  (let ((lochash (gethash nil treehash (make-hash-table :test #'eq)))
	(condlen (length condition-specs)))
    (push (if (< 0 condlen)
	      (condition-release-closure labeller condition-specs)
	      (lambda ()
		labeller))
	  (gethash (length condition-specs) lochash nil))))

(defun most-qualified-item (endhash)
  "The end hash on the context tree contains numbered bins. Each bin is a list. The numbers indicate how many conditions each labeller wants met. Start with the most demanding, work down, returning the first one that is happy. The conditions are based on dynamic vars, so aren't handled here."
  (dolist (n (sort (alexandria:hash-table-keys endhash) #'>))
    (dolist (f (gethash n endhash))
      (anaphora:awhen (funcall f)
	(return it)))))

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

(defun context-tree-find (thing context storage &key first-only)
  "Look up the item that matches thing, with the most specifically matching context." 
  (let ((thetree (aif2only (gethash thing storage)
			   it
			   (gethash nil storage))))
    (if thetree
	(let ((short-context context)
	      (stor nil))
	  (block proc
	    (loop do
		 (when (= 0 (length short-context))
		   (return-from proc))
		 (aif2only 
		  (gethash nil (context-lowest-branch short-context thetree))
		  (progn
		    (push it stor)
		    (when first-only (return-from proc)))
		  (setf short-context (butlast short-context)))))
	  (if stor
	      (values (if first-only (car stor) (nreverse stor)) t)
	      (values nil nil)))		  
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
