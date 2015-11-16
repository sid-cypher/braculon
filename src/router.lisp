;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10 -*-

(in-package :braculon)

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
    (flet ((call-router (form)
	     (etypecase form
	       (cons
		(princ (gethash (first form) (routers state)))
		(terpri)
		(funcall (callable (gethash (first form) (routers state))) env (rest form)))
	       (symbol (funcall (callable (gethash form (routers state))) env)))))
      (format t "starting chainz~%")
      (or
       (find-if #'call-router (routing-chain state))
       '(404
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

(defgeneric load-builtin-routers (state)
  (:method ((state brac-appstate))
    (let ((fixed-router-callable ;; TODO: with :regex t option
	   (lambda (env &keys)
	     nil))
	  (test-router-callable
	   (lambda (env &keys)
	     `(200
	       (:content-type "text/plain; charset=UTF-8")
	       ,(list (format nil "state: ~A~%env: ~A~%" state env)))))
	  (static-file-router-callable
	   (lambda (env &keys) ;; TODO: build-folder-index, recursive, separator, controller


	     ;; ===old===
	     (destructuring-bind (&key folder url-prefix data) options
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

	     ))
	  (code-router-callable
	   (lambda (env &keys)
	     nil))
	  (redirect-router-callable
	   (lambda (env &keys)
	     nil)))
      (add-router state (make-instance 'brac-router
				      :parent state
				      :name 'brac-conf::fixed
				      :callable fixed-router-callable
				      :source-file nil))
      (add-router state (make-instance 'brac-router
				      :parent state
				      :name 'brac-conf::test
				      :callable test-router-callable
				      :source-file nil))
      (add-router state (make-instance 'brac-router
				      :parent state
				      :name 'brac-conf::static
				      :callable static-file-router-callable
				      :source-file nil))
      (add-router state (make-instance 'brac-router
				      :parent state
				      :name 'brac-conf::code
				      :callable code-router-callable
				      :source-file nil))
      (add-router state (make-instance 'brac-router
				      :parent state
				      :name 'brac-conf::redirect
				      :callable redirect-router-callable
				      :source-file nil))
      t))
  (:documentation ""))

;; TODO totally needs rewriting
(defgeneric load-router-files (state)
  (:method ((state brac-appstate))
    (let ((default-order '(braculon::static braculon::dynamic (braculon::fixed "/" "index")))
	  (order-file (merge-pathnames #p"order.conf" (routers-path state)))
	  order-form)
      (setf order-form (read-form-file order-file))
      (unless order-form
	;; TODO put this file write-out behind a macro
	(with-open-file (filestream order-file ;; TODO handle all exceptions
				    :direction :output
				    :if-does-not-exist :create
				    :if-exists :rename)
	  ;; TODO writeout default "index" controller elsewhere
	  (let ((*package* (find-package :braculon)))
	    (princ ";;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10 -*-" filestream)
	    (terpri filestream)
	    (write default-order
		   :case (config-print-case state)
		   :stream filestream)
	    (terpri filestream)))
	(setf order-form default-order))
      (setf (slot-value state 'routers-order) order-form))

    (let ((router-src-files (uiop:directory-files (routers-path state))))
      (dolist (filename router-src-files)
	(let ((source-file-forms (read-multiple-forms-file filename)))
	  (dolist (src-form source-file-forms)
	    (let* ((fcall-symbol (when (and (consp src-form)
					    (symbolp (first src-form)))
				   (pop src-form)))
		   (rtr-name (when (consp src-form)
			       (pop src-form)))
		   (rtr-lambda-list (when (consp src-form)
				      (pop src-form)))
		   (req-sym (when (consp rtr-lambda-list)
			      (pop rtr-lambda-list)))
		   (opts-sym (when (consp rtr-lambda-list)
			       (pop rtr-lambda-list)))
		   (rtr-body (when (consp src-form)
			       src-form))
		   rtr-callable) ;; TODO report errors
	      (when (and (string= (symbol-name fcall-symbol) "DEFROUTER")
			 (or (symbolp rtr-name)
			     (stringp rtr-name))
			 (symbolp req-sym)
			 (symbolp opts-sym)
			 (null rtr-lambda-list)
			 (not (constantp req-sym))
			 (not (constantp opts-sym))
			 rtr-body)
		(setf rtr-name (safe-name-symbol-to-string rtr-name))
		(setf rtr-callable
		      (ignore-errors ;; TODO log the errors instead
			(eval `(lambda (,req-sym ,opts-sym)
				 ,@rtr-body)))))
	      (when rtr-callable ;; TODO log this addition
		(add-router state (make-instance 'brac-router
						:parent state
						:name rtr-name
						:callable rtr-callable
						:source-file filename)))))))))
  (:documentation ""))

;;(defmethod call ((rtr brac-router) env &rest args &key &allow-other-keys)
;;  (funcall (callable rtr) env :allow-other-keys t args))
