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
	 :initform (error "View object needs a name symbol.")
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

;; TODO write, with hooks later
(defgeneric add-view (state view)
  (:method ((state brac-appstate) (view brac-view))
    "" ;; TODO
    (with-slots (views) state
      (setf (gethash (name view) views) view)))
  (:documentation ""))

;; TODO hooks
(defgeneric del-view (state view-name)
  (:method ((state brac-appstate) view-name)
    (declare (type symbol view-name))
    (with-slots (views) state
      (remhash view-name views)))
  (:documentation ""))

(defgeneric get-view (state view-name)
  (:method ((state brac-appstate) view-name)
    (declare (type symbol view-name))
    (gethash view-name (slot-value state 'views)))
  (:method (env view-name)
    (etypecase env
      (cons (get-view (get-appstate env) view-name))))
  (:documentation ""))

(defun hash-table-test-eq-p (ht)
  (eq 'eq (hash-table-test ht)))

;;TODO use defstruct for better printing
(deftype view-field-collection ()
  '(and hash-table
    (satisfies hash-table-test-eq-p)))

;;TODO walk view dependencies, get full length, add keys
(defun make-field-collection (view &key no-deps)
  (make-hash-table :test 'eq
		   :size (length (fields view))
		   :rehash-threshold 1))

(defun field (collection indicator)
  ""
  (declare (type view-field-collection collection)
	   (type keyword indicator))
  (gethash indicator collection))

(defun (setf field) (collection indicator value)
  (declare (type view-field-collection collection)
	   (type keyword indicator))
  (setf (gethash indicator collection) value))

(defmacro with-view-fields (env))

;;TODO
(defun render (env)
  nil)

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

(defun set-response (value env)
  (setf (response env) value)
  env)

;;TODO
(defun load-builtin-views (state)
  nil)

;;TODO: remove special variables in favor of ENV keys
(defvar *view-src-file* nil)

;; TODO review file format, rewrite
(defgeneric load-view-files (state)
  (:method ((state brac-appstate))
    (let ((view-src-files (uiop:directory-files (views-path state))))
      (dolist (filepath view-src-files)
	(let ((src-form (read-single-form-file filepath)))
	  (let* ((fcall-symbol (when (and (consp src-form)
					  (symbolp (first src-form)))
				 (first src-form)))
		 (view-name (when (consp (rest src-form))
			      (second src-form)))) ;; TODO report errors
	    (when (and (string= (symbol-name fcall-symbol) "DEFVIEW")
		       (symbolp view-name))

	      (format t "View definition file found: ~A; name: ~W~%"
		      filepath view-name)

	      (let ((brac::*appstate* state)
		    (brac::*view-src-file* filepath)
		    (*package* (find-package :brac-conf)))
		(eval src-form))))))))
  (:documentation ""))
