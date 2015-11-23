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
	      :initform (get-universal-time)
	      :documentation "")))

(defmethod print-object ((rtr brac-router) stream)
  (print-unreadable-object (rtr stream :type t)
    (format stream "~A" (name rtr))))

(defgeneric chain-route-request (state env)
  (:method ((state brac-appstate) env)
    ;; TODO *all* the sanity checks
    (flet ((call-router (form)
	     (etypecase form
	       (cons
		(apply (callable (gethash (first form) (routers state)))
		       env (rest form)))
	       (symbol
		(funcall (callable (gethash form (routers state)))
			 env)))))
      (or
       (dolist (form (routing-chain state))
	 (format t "Calling form: ~W~%" form)
	 (multiple-value-bind (result new-env) (call-router form)
	   (when new-env
	     (setf env new-env))
	   (when result
	     (return (call-controller state result (or new-env env))))))
       '(404 ;TODO: default error router with logging
	 (:content-type "text/html; charset=utf-8")
	 ("<html><head><title>Not found</title></head>
<body> Resource not found. </body></html>")))))
  (:documentation "o hai, i send off reqs thru routing tubez"))

;;TODO add hooks
(defgeneric add-router (state rtr)
  (:method ((state brac-appstate) (rtr brac-router))
    ""
    (with-slots (routers) state
      (setf (gethash (name rtr) routers) rtr)))
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
		(string-and-slash= path (getf env :path-info))
		(string= path (getf env :path-info)))
	ctrl-name))

    (defrouter* brac-conf::test (env) state
      (format t "Test router reporting.~%state: ~W~%env: ~W~%" state env)
      'brac-conf::test)

    ;; TODO: build-folder-index, recursive, separator, controller
    (defrouter* brac-conf::static (env) state
      (let ((router-data '(:filename "wavy.png")))
	(setf (getf env :router-data) router-data)
	(when (string-and-slash= "/wavy" (getf env :path-info))
	  (values 'brac-conf::file-contents env))))
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

    (defrouter* brac-conf::code (env) state
      nil)
    (defrouter* brac-conf::redirect (env) state
      nil)
    t)
  (:documentation ""))

;;TODO: remove special variables in favor of ENV keys
(defvar *router-src-file* nil)

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
	      (format t "Router definition found: ~A; Internal name: ~W~%"
		      filepath rtr-name)
	      (let ((brac:*appstate* state)
		    (brac::*router-src-file* filepath)
		    (*package* (find-package :brac-conf)))
		(eval src-form))))))))
  (:documentation ""))
