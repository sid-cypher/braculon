;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10 -*-

(in-package :braculon)

(defclass brac-ctrl ()
  ((appstate :reader appstate
		  :initarg :parent
		  :initform (error "Please specify the webapp that will use this object.")
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

(defmethod print-object ((ctrl brac-ctrl) stream)
  (print-unreadable-object (ctrl stream :type t)
    (format stream "~A" (name ctrl))))

(defgeneric add-controller (state ctrl)
  (:documentation ""))

(defgeneric del-controller (state ctrl-name)
  (:documentation ""))

;; TODO: replace generic functions with regular defun, declare type
(defgeneric load-builtin-controllers (state)
  (:documentation ""))

(defgeneric load-controller-files (state)
  (:documentation ""))

(defmethod add-controller ((state brac-appstate) (ctrl brac-ctrl))
  "" ;; TODO
  (with-slots (controllers controller-names) state
    (let ((ctrl-name (name ctrl)))
      (push ctrl-name controller-names)
      (setf (gethash ctrl-name controllers) ctrl))))

(defmethod del-controller ((state brac-appstate) ctrl-name)
  (with-slots (controllers controller-names) state
    (remove ctrl-name controller-names :test #'string=)
    (remhash ctrl-name controllers)))

(defmethod load-builtin-controllers ((state brac-appstate))
  (let ((messages-ctrl-callable
	 (lambda (req)
	   nil))
	(hello-ctrl-callable
	 (lambda (req)
	   "Outputs a short greetings page. A tiny built-in controller for testing purposes."
	   (cl-who:with-html-output-to-string (s nil :prologue t :indent t)
	     (:html (:head (:title "braculon:hello"))
		    (:body (:p "Hello! Things seem to work here."))))))
	(dir-index-ctrl-callable
	 (lambda (req)
	   nil))
	(file-contents-ctrl-callable
	 (lambda (req)
	   (lack.component:call
	    (lack.app.file:make-app :file (getf (router-data req) :file)
			            :root (static-content-path state)))))
	(http-code-ctrl-callable
	 (lambda (req)
	   nil)))
    (add-controller state (make-instance 'brac-ctrl
					 :parent state
					 :name "messages"
					 :callable messages-ctrl-callable
					 :source-file nil))
    (add-controller state (make-instance 'brac-ctrl
					 :parent state
					 :name "hello"
					 :callable hello-ctrl-callable
					 :source-file nil))
    (add-controller state (make-instance 'brac-ctrl
					 :parent state
					 :name "dir-index"
					 :callable dir-index-ctrl-callable
					 :source-file nil))
    (add-controller state (make-instance 'brac-ctrl
					 :parent state
					 :name "file-contents"
					 :callable file-contents-ctrl-callable
					 :source-file nil))
    (add-controller state (make-instance 'brac-ctrl
					 :parent state
					 :name "http code"
					 :callable http-code-ctrl-callable
					 :source-file nil))
    t))

(defmethod load-controller-files ((state brac-appstate))
  (let ((controller-src-files (uiop:directory-files (controllers-path state))))
    (dolist (filename controller-src-files)
      (let ((source-file-forms (read-multiple-forms-file filename)))
	(dolist (src-form source-file-forms)
	  (let* ((fcall-symbol (when (consp src-form)
				 (pop src-form)))
		 (ctrl-name (when (consp src-form)
			     (pop src-form)))
		 (ctrl-lambda-list (when (consp src-form)
				    (pop src-form)))
		 (req-sym (when (consp ctrl-lambda-list)
			    (pop ctrl-lambda-list)))
		 (ctrl-body (when (consp src-form)
			     src-form))
		 ctrl-callable) ;; TODO report errors
	    (when (and (symbolp fcall-symbol)
		       (string= (symbol-name fcall-symbol) "DEFCONTROLLER")
		       (or (stringp ctrl-name)
			   (symbolp ctrl-name))
		       (symbolp req-sym)
		       (null ctrl-lambda-list)
		       (not (constantp req-sym))
		       ctrl-body)
	      (setf ctrl-name (safe-name-symbol-to-string ctrl-name))
	      (setf ctrl-callable
		    (ignore-errors ;; TODO log the errors instead
		      (eval `(lambda (,req-sym)
			       ,@ctrl-body)))))
	    (when ctrl-callable ;; TODO log this addition
	      (add-controller state (make-instance 'brac-ctrl
					       :parent state
					       :name ctrl-name
					       :callable ctrl-callable
					       :source-file filename)))))))))
