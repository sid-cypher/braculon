;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10 -*-

(in-package :braculon)

(defclass brac-controller ()
  ((project-state :reader project-state
		  :initarg :parent
		  :initform (error "Please specify the project that will use this object.")
		  :documentation "")
   (name :reader name
	 :initarg :name
	 :documentation "")
   (callable :reader callable
	     :initarg :callable
	     :documentation "")
   (source-file :reader source-file
		:initarg :source-file
		:documentation "")
   (load-time :reader load-time
	      :initform (get-universal-time)
	      :documentation "")))

(defgeneric load-builtin-controllers (state)
  (:documentation ""))

(defgeneric add-controller (state ctrl)
  (:documentation ""))

(defgeneric del-controller (state ctrl-name)
  (:documentation ""))

(defmethod load-builtin-controllers ((state project-state))
  (let ((messages-ctrl-callable
	 (lambda (req)
	   nil))
	(http-code-ctrl-callable
	 (lambda (req)
	   nil)))
    nil))

(defmethod add-controller ((state project-state) (ctrl brac-controller))
  "" ;; TODO
  (with-slots (routers router-names) state
    (let ((ctrl-name (name ctrl)))
      (push ctrl-name router-names)
      (setf (gethash ctrl-name routers) ctrl))))

(defmethod del-controller ((state project-state) ctrl-name)
  (with-slots (routers router-names) state
    (remove ctrl-name router-names :test #'string=)
    (remhash ctrl-name routers)))
