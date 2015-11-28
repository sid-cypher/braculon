(in-package :braculon)
(use-package :annot.class)
(annot:enable-annot-syntax)

@export-class
(defclass brac-router ()
  ((appstate :reader appstate
	     :initarg :parent
	     :initform (error "Router object needs a parent appstate.")
	     :documentation "")
   (name :reader name
	 :initarg :name
	 :initform (error "Router object needs a name.")
	 :documentation "")
   (callable :reader callable
	     :initarg :callable
	     :initform (error "Router object needs its callable part.")
	     :documentation "")
   (source-file :reader source-file
		:initarg :source-file
		:documentation "")
   (load-time :reader load-time
	      :initform (local-time:now)
	      :documentation "")))

(defmethod print-object ((rtr brac-router) stream)
  (print-unreadable-object (rtr stream :type t)
    (format stream "~A" (name rtr))))

@export
(defun pack-routing-data (env source-router target-controller data)
  (declare (type brac-reqstate env)
	   (type brac-router source-router)
	   (type brac-ctrl target-controller))
  (setf (router env) source-router)
  (setf (controller env) target-controller)
  (setf (routing-data env) data)
  env)

@export
(defgeneric get-router (state rtr-name)
  (:method ((state brac-appstate) rtr-name)
    ""
    (declare (type symbol rtr-name))
    (gethash rtr-name (slot-value state 'routers)))
  (:documentation ""))

;;TODO: rewrite so that only env is send to and received from a controller.
(defun chain-route-request (env)
  ;; TODO *all* the sanity checks
  (let ((state (appstate env)))
    (flet ((call-router (form)
	     (etypecase form
	       (cons
		(apply (callable (gethash (first form) (routers state)))
		       env (rest form)))
	       (symbol
		(funcall (callable (gethash form (routers state)))
			 env)))))

      (dolist (form (routing-chain state))
	(format t "Calling form: ~W~%" form)
	(let ((new-env (call-router form)))
	  (when new-env
	    (setf env new-env)
	    (return))))
      (setf (response env) ;default response to be overwritten
	    '(404 ;TODO: default error router with logging
	      (:content-type "text/html; charset=utf-8")
	      ("<html><head><title>Not found</title></head>
<body> Resource not found. </body></html>")))
      env)))

;;TODO add hooks into router slot writer
(defgeneric add-router (state rtr)
  (:method ((state brac-appstate) (rtr brac-router))
    ""
    (setf (gethash (name rtr) (slot-value state 'routers)) rtr))
  (:documentation ""))

;;TODO add hooks
(defgeneric del-router (state rtr-name)
  (:method ((state brac-appstate) rtr-name)
    ""
    (declare (type string rtr-name))
    (with-slots (routers) state
      (remhash rtr-name routers)))
  (:documentation ""))

(defmacro defrouter* (name lambda-list appstate &body body)
  (declare (type symbol name)
	   (type list lambda-list))
  `(add-router ,appstate
	       (make-instance 'brac-router
			      :parent ,appstate
			      :name ',name
			      :callable (lambda ,lambda-list ,@body)
			      :source-file nil)))

(defgeneric load-builtin-routers (state)
  (:method ((state brac-appstate))
    ;;TODO with regex option
    (defrouter* brac-conf::fixed (env path ctrl-name &key (trailing-slash-option t)) state
      (declare (type string path)
	       (type symbol ctrl-name))
      (when (if trailing-slash-option
		(string-and-slash= path (gethash :path-info (request env)))
		(string= path (gethash :path-info (request env))))
	(pack-routing-data env
			   (get-router state 'brac-conf::fixed)
			   (get-controller state ctrl-name)
			   nil)))

    (defrouter* brac-conf::test (env) state
      (format t "Test router reporting.~%state: ~W~%env: ~W~%"
	      state env)
      (pack-routing-data env
			 (get-router state 'brac-conf::test)
			 (get-controller state 'brac-conf::test)
			 nil))

    ;; TODO: build-folder-index, recursive, separator, controller
    (defrouter* brac-conf::static (env) state
      (let ((router-data '(:filename "wavy.png")))
	(setf (routing-data env) router-data)
	(when (string-and-slash= "/wavy" (gethash :path-info (request env)))
	  (pack-routing-data env
			     (get-router state 'brac-conf::static)
			     (get-controller state 'brac-conf::file-contents)
			     router-data))))
    ;; ===old===
    #+nil(destructuring-bind (&key folder url-prefix data) options
	   (unless (stringp url-prefix)
	     ;; TODO: log config error - url-prefix not a string
	     (setf url-prefix nil))
	   (unless (pathnamep folder) ;; TODO as well
	     (setf folder nil))
	   (let ((static-files (uiop:directory-files
				(if folder
				    (merge-pathnames folder (static-content-path state))
				    (static-content-path state))))
		 (prefix (or url-prefix "/"))
		 matchp)
	     (setf (router-data req) nil)
	     (loop for file in static-files
		while (not matchp) do
		;; trailing slash :deny \(later :allow, :require)
		  (when (string= (url-req-path-name req)
				 (cat prefix (file-namestring file)))
		    (setf (router-data req) (list :file file
						  :data data))
		    (setf matchp t)))
	     (when matchp
	       "file-contents")))

    #+nil(defrouter* brac-conf::redirect (env) state
      nil)
    #+nil(defrouter* brac-conf::masquerade (env) state
      nil)
    t)
  (:documentation ""))

;;TODO: remove special variables in favor of ENV keys
(defvar *router-src-file* nil)

;;TODO: DRY in load-some-files
(defgeneric load-router-files (state)
  (:method ((state brac-appstate))
    (let ((router-src-files (uiop:directory-files (routers-path state))))
      (dolist (filepath router-src-files)
	(let ((src-form (read-single-form-file filepath)))
	  (let* ((fcall-symbol (when (and (consp src-form)
					  (symbolp (first src-form)))
				 (first src-form)))
		 (rtr-name (when (consp (rest src-form))
			     (second src-form)))) ;; TODO report errors
	    (when (and (string= (symbol-name fcall-symbol) "DEFROUTER")
		       (symbolp rtr-name))
	      (format t "Router definition file found: ~A; name: ~W~%"
		      filepath rtr-name)
	      (let ((brac::*appstate* state)
		    (brac::*router-src-file* filepath)
		    (*package* (find-package :brac-conf)))
		(eval src-form))))))))
  (:documentation ""))
