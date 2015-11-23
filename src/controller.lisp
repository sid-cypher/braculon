(in-package :braculon)
(use-package :annot.class)
(annot:enable-annot-syntax)

@export-class
(defclass brac-ctrl ()
  ((appstate :reader appstate
	     :initarg :parent
	     :initform (error "Controller object needs a parent appstate.")
	     :documentation "")
   (name :reader name
	 :initarg :name
	 :initform (error "Controller object needs a name symbol.")
	 :documentation "")
   (callable :reader callable
	     :initarg :callable
	     :initform (error "Controller object needs it callable part.")
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

;; TODO hooks, maybe log, no-overwrite option
(defgeneric add-controller (state ctrl)
  (:method ((state brac-appstate) (ctrl brac-ctrl))
    "" ;; TODO
    (with-slots (controllers) state
      (setf (gethash (name ctrl) controllers) ctrl)))
  (:documentation ""))

;; TODO hooks
(defgeneric del-controller (state ctrl-name)
  (:method ((state brac-appstate) ctrl-name)
    (with-slots (controllers) state
      (remhash ctrl-name controllers)))
  (:documentation ""))

(defgeneric call-controller (state ctrl-name env)
  (:method ((state brac-appstate) ctrl-name env)
    (funcall (callable (gethash ctrl-name
				(controllers state)))
	     env))
  (:documentation ""))

(defmacro defcontroller* (name env-var appstate &body body)
  (declare (type symbol name env-var))
  `(add-controller ,appstate
		   (make-instance 'brac-ctrl
				  :parent ,appstate
				  :name ',name
				  :callable (lambda (,env-var) ,@body)
				  :source-file nil)))

;; TODO check that callables receive correct arguments
(defgeneric load-builtin-controllers (state)
  (:method ((state brac-appstate))
    (let ((test-ctrl-callable
	   (lambda (env)
	     "Outputs a short greetings page. A tiny built-in controller for testing purposes."
	     `(200
	       (:content-type "text/plain; charset=UTF-8")
	       ;;TODO call renderer here
	       ,(list (format nil "Test controller reporting.~%state: ~W~%env: ~W~%" state env)))))
	  (hello-ctrl-callable
	   (lambda (env)
	     "Outputs a short greetings page. A tiny built-in controller for testing purposes."
	     (declare (ignorable env))
	     `(200
	       (:content-type "text/html; charset=utf-8")
	       ;;TODO call renderer here
	       ,(list (cl-who:with-html-output-to-string (s nil :prologue t :indent t)
			(:html (:head (:title "braculon:hello"))
			       (:body (:p "Hello! Things seem to work here."))))))))
	  (dir-index-ctrl-callable
	   (lambda (env)
	     nil))
	  (file-contents-ctrl-callable
	   (lambda (env)
	     (lack.component:call
	      (let ((st-path-ext (getf (extensions state) :static-content-path)))
		(lack.app.file:make-app :file (getf (getf env :router-data) :filename)
					:root (or st-path-ext
						  (uiop:merge-pathnames* #p"static/" ;;TODO no magic
									 (root-path state)))))
	      env)))
	  (http-code-ctrl-callable
	   (lambda (env)
	     nil)))
      (add-controller
       state (make-instance 'brac-ctrl
			    :parent state
			    :name 'brac-conf::test
			    :callable test-ctrl-callable
			    :source-file nil))
      (add-controller
       state (make-instance 'brac-ctrl
			    :parent state
			    :name 'brac-conf::hello
			    :callable hello-ctrl-callable
			    :source-file nil))
      (add-controller
       state (make-instance 'brac-ctrl
			    :parent state
			    :name 'brac-conf::dir-index
			    :callable dir-index-ctrl-callable
			    :source-file nil))
      (add-controller
       state (make-instance 'brac-ctrl
			    :parent state
			    :name 'brac-conf::file-contents
			    :callable file-contents-ctrl-callable
			    :source-file nil))
      (add-controller
       state (make-instance 'brac-ctrl
			    :parent state
			    :name 'brac-conf::http-code
			    :callable http-code-ctrl-callable
			    :source-file nil))
      t))
  (:documentation ""))

;;TODO: remove special variables in favor of ENV keys
(defvar *controller-src-file* nil)

;; TODO check file format
(defgeneric load-controller-files (state)
  (:method ((state brac-appstate))
    (let ((controller-src-files (uiop:directory-files (controllers-path state))))
      (dolist (filepath controller-src-files)
	(let ((src-form (read-single-form-file filepath)))
	  (let* ((fcall-symbol (when (and (consp src-form)
					  (symbolp (first src-form)))
				 (first src-form)))
		 (ctrl-name (when (consp (rest src-form))
			      (second src-form)))) ;; TODO report errors

	    (when (and (string= (symbol-name fcall-symbol) "DEFCONTROLLER")
		       (symbolp ctrl-name))
	      (format t "Controller definition found: ~A; Internal name: ~W~%"
		      filepath ctrl-name)

	      (let ((brac:*appstate* state)
		    (brac::*controller-src-file* filepath)
		    (*package* (find-package :brac-conf)))
		(eval src-form))))))))
  (:documentation ""))

;;(defmethod call ((ctrl brac-ctrl) env &rest args &key &allow-other-keys)
  ;;(funcall (callable ctrl) env :allow-other-keys t args))
