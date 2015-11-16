;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10 -*-

(in-package :braculon)

(defclass brac-ctrl ()
  ((appstate :reader appstate
	     :initarg :parent
	     :initform (error "Controller object needs a parent appstate.")
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

;; TODO hooks, maybe log
(defgeneric add-controller (state ctrl)
  (:method ((state brac-appstate) (ctrl brac-ctrl))
    "" ;; TODO
    (with-slots (controllers) state
      (let ((ctrl-name (name ctrl)))
	(setf (gethash ctrl-name controllers) ctrl))))
  (:documentation ""))

;; TODO hooks
(defgeneric del-controller (state ctrl-name)
  (:method ((state brac-appstate) ctrl-name)
    (with-slots (controllers) state
      (remhash ctrl-name controllers)))
  (:documentation ""))

;; TODO check that callables receive correct arguments
(defgeneric load-builtin-controllers (state)
  (:method ((state brac-appstate))
    (let ((messages-ctrl-callable
	   (lambda (env)
	     nil))
	  (hello-ctrl-callable
	   (lambda (env)
	     "Outputs a short greetings page. A tiny built-in controller for testing purposes."
	     (cl-who:with-html-output-to-string (s nil :prologue t :indent t)
	       ;;TODO call renderer here
	       (:html (:head (:title "braculon:hello"))
		      (:body (:p "Hello! Things seem to work here."))))))
	  (dir-index-ctrl-callable
	   (lambda (env)
	     nil))
	  (file-contents-ctrl-callable
	   (lambda (env)
	     (lack.component:call
	      (lack.app.file:make-app :file (getf (router-data env) :file)
				      :root (static-content-path state)))))
	  (http-code-ctrl-callable
	   (lambda (env)
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
  (:documentation ""))

;; TODO check file format
(defgeneric load-controller-files (state)
  (:method ((state brac-appstate))
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
	      (when ctrl-callable
		(add-controller state (make-instance 'brac-ctrl
						     :parent state
						     :name ctrl-name
						     :callable ctrl-callable
						     :source-file filename)))))))))
  (:documentation ""))

;;(defmethod call ((ctrl brac-ctrl) env &rest args &key &allow-other-keys)
  ;;(funcall (callable ctrl) env :allow-other-keys t args))
