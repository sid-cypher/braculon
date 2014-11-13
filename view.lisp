;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10 -*-

(in-package :braculon)

(defclass brac-view () ;; fill from a file with defview on first LOAD-VIEW
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
   (var-names :reader var-names
	     :initarg :var-names
	     :documentation "string array")
   (load-time :reader load-time
	      :initform (get-universal-time)
	      :documentation "")))

(defclass brac-view-data () ;; create in a controller, pass to RENDER-WHO
   ((var-names :reader var-names
	     :initarg :var-names
	     :documentation "string array")
   (var-values :reader var-values
	     :initarg :var-values
	     :documentation "hashtable"))

(defmethod print-object ((view brac-view) stream)
  (print-unreadable-object (view stream :type t)
    (format stream "~A" (name view))))

(defgeneric route-request (request)
  (:documentation "o hai, i send off reqs thru routing tubez"))

(defgeneric add-view (state view)
  (:documentation ""))

(defgeneric del-view (state view-name)
  (:documentation ""))

(defgeneric load-view-files (state)
  (:documentation ""))

(defun render-who (view-name view-data &key cache depth-limit)
  (let (view
	cacheablep)
    ;; TODO: load views recursively to build a full cl-who form, insert vars
    ))

(defmacro with-view-data (symbols result-object &body body)
  "put values of symbols variables into an object, and the object in RESULT-OBJECT"
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
