(in-package :braculon)
(use-package :annot.class)
(annot:enable-annot-syntax)

@export-class
(defclass view ()
  ((name :reader name
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

(defmethod print-object ((view view) stream)
  (print-unreadable-object (view stream :type t)
    (format stream "~A" (name view))))

@export
(defun get-view (view-name &optional app)
  (declare (type (or string symbol) view-name))
  (gethash (name-to-downcase-string view-name) (views (find-app app))))

(defun add-view (view &optional app)
  (declare (type view view))
  (setf (gethash (name view) (views (find-app app))) view))

@export
(defun del-view (view-name &optional app)
  (declare (type (or string symbol) view-name))
  (remhash (name-to-downcase-string view-name) (views (find-app app))))

;;TODO walk view dependencies, get full length, add keys
;;IMPORTANT: check if view was already visited, avoid circular deps.
(defun make-field-collection (view)
  (let ((app (app view))
	(queue (make-instance 'jpl-queues:unbounded-fifo-queue))
	dep-fields-list
	all-dep-fields
	field-collection)
    (labels
	((bfs-walk (v) ;;make iterative maybe
	   (declare (type view view))
	   (dolist (dv (dependencies v))
	     (jpl-queues:enqueue (get-view dv app) queue))
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

(defmacro with-view-fields (fields rps &body body)
  ""
  (let ((fcol-sym (gensym "field-collection"))
	field-bindings)
    (loop for f in fields do
      (push `(,f (field ,fcol-sym
			(find-symbol (symbol-name ,f) :keyword)))
	    field-bindings))
    `(let ((,fcol-sym (view-fields ,rps)))
       (let ,field-bindings
	 ,@body))))

@export
(defgeneric pack-rendering-data (rps view &optional field-collection)
  (:method ((rps request-processing-state) view-name &optional field-collection)
    (declare (type symbol view-name))
    (let ((view (get-view view-name (app rps))))
      (if view
	  (pack-rendering-data rps view field-collection)
	  (error "view-not-found"))))
  (:method ((rps request-processing-state) (view view) &optional field-collection)
    (setf (root-view rps) view)
    (setf (view-fields rps)
	  (or field-collection
	      (make-field-collection view))))
  (:documentation ""))

@export
(defun render (rps)
  ""
  (let ((view (root-view rps)))
    (when view
      (let ((app (app rps))
	    result)
	(when (verbosep app)
	  (format t "Rendering view: ~W~%" view))
	(labels
	    ((dep-call-results (v)
	       (let ((deps (dependencies v))
		     (ren-fun (renderable v))
		     d-values)
		 (setf d-values
		       (mapcar (lambda (d)
				 (dep-call-results (get-view d app)))
			       deps))
		 (apply ren-fun rps d-values))))
	  (setf result (dep-call-results view)))
	(setf (res-content rps) result)))))

;;TODO faster typechecking, better rps data structure
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
(defun load-builtin-views (app)
  nil)
