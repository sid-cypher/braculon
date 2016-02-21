(in-package :braculon)
(use-package :annot.class)
(annot:enable-annot-syntax)

@export-class
(defclass brac-view ()
  ((appstate :reader appstate
	     :initarg :parent
	     :initform (error "View object needs a parent appstate.")
	     :documentation "")
   (name :reader name
	 :initarg :name
	 :initform (error "View object needs a name.")
         :type string
	 :documentation "")
   (fields :reader fields
	   :initarg :fields
	   :type list
	   :initform ()
	   :documentation "A vector of field name keywords.")
   (dependencies :reader dependencies
		 :initarg :dependencies
		 :type list
		 :initform ()
		 :documentation "A vector of direct dependencies of this view, name symbols.")
   (renderable :reader renderable
	       :initarg :renderable
	       :initform (error "View object needs it renderable part.")
	       :documentation "")
   (source-file :reader source-file
		:initarg :source-file
		:documentation "")
   (load-time :reader load-time
	      :initform (local-time:now)
	      :documentation "")))

(defmethod print-object ((view brac-view) stream)
  (print-unreadable-object (view stream :type t)
    (format stream "~A" (name view))))

(defgeneric get-view (appstate view-name)
  (:method ((appstate brac-appstate) view-name)
    ""
    (declare (type (or string symbol) view-name))
    (gethash (name-to-downcase-string view-name) (views appstate)))
  (:documentation ""))

;; TODO hooks, maybe log, no-overwrite option
(defgeneric add-view (appstate view)
  (:method ((appstate brac-appstate) (view brac-view))
    ""
    (setf (gethash (name view) (views appstate)) view))
  (:documentation ""))

;;TODO add hooks
(defgeneric del-view (appstate view-name)
  (:method ((appstate brac-appstate) view-name)
    ""
    (declare (type (or symbol string) view-name))
    (with-slots (views) appstate
      (remhash (name-to-downcase-string view-name) views)))
  (:documentation ""))

;;TODO walk view dependencies, get full length, add keys
;;IMPORTANT: check if view was already visited, avoid circular deps.
(defun make-field-collection (view)
  (let ((state (appstate view))
	(queue (make-instance 'jpl-queues:unbounded-fifo-queue))
	dep-fields-list
	all-dep-fields
	field-collection)
    (labels
	((bfs-walk (v) ;;make iterative maybe
	   (declare (type brac-view view))
	   (dolist (dv (dependencies v))
	     (jpl-queues:enqueue (get-view state dv) queue))
	   (push (fields v) dep-fields-list)
	   (let ((next (jpl-queues:dequeue queue)))
	     (if next
		 (bfs-walk next)
		 t))))
      (bfs-walk view))
    (format t "Dependencies of ~W need fields:~%~W~%" view dep-fields-list)
    (setf all-dep-fields (reduce #'nunion dep-fields-list))
    (format t "Union list: ~W~%" all-dep-fields)
    (setf field-collection
	  (make-hash-table :test 'eq
			   :size (length all-dep-fields)
			   :rehash-threshold 1))
    (dolist (field-key all-dep-fields)
      (setf (gethash field-key field-collection) nil))
    field-collection))

@export
(defun field (collection indicator)
  ""
  (declare (type hash-table collection)
	   (type keyword indicator))
  (gethash indicator collection))

(defun (setf field) (collection indicator value)
  (declare (type hash-table collection)
	   (type keyword indicator))
  (setf (gethash indicator collection) value))

(defmacro with-view-fields (fields env &body body)
  ""
  (let ((fcol-sym (gensym "field-collection"))
	field-bindings)
    (loop for f in fields do
      (push `(,f (field ,fcol-sym
			(find-symbol (symbol-name ,f) :keyword)))
	    field-bindings))
    `(let ((,fcol-sym (view-fields ,env)))
       (let ,field-bindings
	 ,@body))))

@export
(defgeneric pack-rendering-data (env view &optional field-collection)
  (:method ((env brac-reqstate) view-name &optional field-collection)
    (declare (type symbol view-name))
    (let ((view (get-view (appstate env) view-name)))
      (if view
	  (pack-rendering-data env view field-collection)
	  (error "view-not-found"))))
  (:method ((env brac-reqstate) (view brac-view) &optional field-collection)
    (setf (root-view env) view)
    (setf (view-fields env)
	  (or field-collection
	      (make-field-collection view))))
  (:documentation ""))

@export
(defun render (env)
  ""
  (let ((view (root-view env)))
    (when view
      (let ((state (appstate env))
	    result)
	(when (verbosep state)
	  (format t "Rendering view: ~W~%" view))
	(labels
	    ((dep-call-results (v)
	       (let ((deps (dependencies v))
		     (ren-fun (renderable v))
		     d-values)
		 (setf d-values
		       (mapcar (lambda (d)
				 (dep-call-results (get-view state d)))
			       deps))
		 (apply ren-fun env d-values))))
	  (setf result (dep-call-results view)))
	(setf (response-content env) result)))))

;;TODO faster typechecking, better env data structure
(defun valid-response-p (clack-response-form)
  (if (or (typep clack-response-form 'function)
	    (and
	     (consp clack-response-form)
	     (integerp (first clack-response-form))
	     (listp (second clack-response-form))
	     (typep (third clack-response-form)
		    '(or cons pathname (simple-array (unsigned-byte 8))))
	     (if (consp (third clack-response-form))
		 (stringp (first (third clack-response-form)))
		 t)))
	clack-response-form
	nil))

;;TODO
(defun load-builtin-views (state)
  nil)
