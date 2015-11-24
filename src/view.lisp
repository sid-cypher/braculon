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
	   :documentation "A vector of field name keywords.")
   (dependencies :reader dependencies
		 :initarg :dependencies
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
    nil)
  (:documentation ""))

;; TODO hooks
(defgeneric del-view (state view-name)
  (:method ((state brac-appstate) view-name)
    (declare (type string view-name))
    (with-slots (views) state
      (remhash view-name views)))
  (:documentation ""))

;;TODO
(defun make-field-collection (view-name &key no-deps)
  nil)

;;TODO
(defun field (collection indicator)
  nil)

;;TODO
(defun (setf field) (collection indicator value)
  nil)

;;TODO
(defun render (env)
  nil)

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

	      (let ((brac:*appstate* state)
		    (brac::*view-src-file* filepath)
		    (*package* (find-package :brac-conf)))
		(eval src-form))))))))
  (:documentation ""))
