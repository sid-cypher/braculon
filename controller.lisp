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

(defmethod print-object ((ctrl brac-controller) stream)
  (print-unreadable-object (ctrl stream :type t)
    (format stream "~A" (name ctrl))))

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
	(hello-ctrl-callable
	 (lambda (req)
	   "Outputs a short greetings page. A tiny built-in controller for testing purposes."
	   (cl-who:with-html-output-to-string (s nil :prologue t)
	     (:html (:head (:title "braculon:hello"))
		    (:body (:p "Hello! Things seem to work here."))))))
	(http-code-ctrl-callable
	 (lambda (req)
	   nil)))
    (add-controller state (make-instance 'brac-controller
					 :parent state
					 :name "hello"
					 :callable hello-ctrl-callable
					 :source-file nil))
    t))

(defmethod add-controller ((state project-state) (ctrl brac-controller))
  "" ;; TODO
  (with-slots (controllers controller-names) state
    (let ((ctrl-name (name ctrl)))
      (push ctrl-name controller-names)
      (setf (gethash ctrl-name controllers) ctrl))))

(defmethod del-controller ((state project-state) ctrl-name)
  (with-slots (controllers controller-names) state
    (remove ctrl-name controller-names :test #'string=)
    (remhash ctrl-name controllers)))
