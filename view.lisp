;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10 -*-

(in-package :braculon)

(defclass brac-view ()
  ((project-state :reader project-state
		  :initarg :parent
		  :initform (error "Please specify the project that will use this object.")
		  :documentation "")
   (name :reader name
	 :initarg :name
	 :documentation "")
   (renderable :reader renderable
	       :initarg :renderable
	       :documentation "")
   (source-file :reader source-file
		:initarg :source-file
		:documentation "")
   (load-time :reader load-time
	      :initform (get-universal-time)
	      :documentation "")))

;; TODO: make and use this
(defclass brac-composed-view (brac-view)
  nil
  (:documentation "Assembled by recursively including other views,
 used to do the tree walk once and cache the results."))

(defclass brac-view-data () ;; create in a controller, pass to RENDER-WHO
  ((var-names :reader var-names
	      :initarg :var-names
	      :documentation "string list")
   (var-values :reader var-values
	       :initarg :var-values
	       :documentation "hashtable")))

(defmethod print-object ((view brac-view) stream)
  (print-unreadable-object (view stream :type t)
    (format stream "~A" (name view))))

(defgeneric add-view (state view)
  (:documentation ""))

(defgeneric del-view (state view-name)
  (:documentation ""))

(defgeneric load-view-files (state)
  (:documentation ""))

(defmethod add-view ((state project-state) (view brac-view))
  "" ;; TODO
  (with-slots (views view-names) state
    (let ((view-name (name view)))
      (push view-name view-names)
      (setf (gethash view-name views) view))))

(defmethod del-view ((state project-state) view-name)
  (with-slots (views view-names) state
    (remove view-name view-names :test #'string=)
    (remhash view-name views)))

(defmethod load-view-files ((state project-state))
  (let ((view-src-files (cl-fad:list-directory (views-path state))))
    (dolist (filename view-src-files)
      (let ((source-file-forms (read-multiple-forms-file filename)))
	(dolist (src-form source-file-forms)
	  (let* ((fcall-symbol (when (consp src-form)
				 (pop src-form)))
		 (view-name (when (consp src-form)
			      (pop src-form)))
		 (view-body (when (consp src-form)
			      src-form))
		 view-renderable) ;; TODO report errors
	    (when (and (symbolp fcall-symbol)
		       (string= (symbol-name fcall-symbol) "DEFVIEW")
		       (or (stringp view-name)
			   (symbolp view-name))
		       view-body)
	      (setf view-name (safe-name-symbol-to-string view-name))
	      (setf view-renderable view-body))
	    (when view-renderable ;; TODO log this addition
	      (add-view state (make-instance 'brac-view
					     :parent state
					     :name view-name
					     :renderable view-renderable
					     :source-file filename)))))))))

(defun render-who (view-name view-data &key if-var-unused if-var-missing cache depth-limit)
  (setf (html-mode) :html5)
  (let (view
	cacheablep)
    ;; TODO: load views recursively to build a full cl-who form, insert vars
    ;;
    ;; (eval `(let (,stuff)
    ;;   (with-html-output-to-string (*standard-output* nil :prologue nil :indent t)

    ))

(defmacro with-data-collected-into (bindings result-object &body body)
  "BINDINGS is a list of variables to be used in the view, and RESULT-OBJECT"
  nil) ;;TODO

#+nil (defun make-view-data-collection ()
  "a closure with a property list and a push-pull iface"
  ;; TODO enforce symbol type, keyword package
  (let (plist)
    (lambda (&optional symbol value)
      (if symbol
	  (if value
	      (setf (getf plist symbol) value)
	      (getf plist symbol))
	  plist))))
