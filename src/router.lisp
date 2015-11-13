;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10 -*-

(in-package :braculon)

(defclass brac-router ()
  ((appstate :reader appstate
	     :initarg :parent
	     :initform (error "Router object needs a parent appstate.")
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

(defmethod print-object ((rtr brac-router) stream)
  (print-unreadable-object (rtr stream :type t)
    (format stream "~A" (name rtr))))

(defgeneric route-request (request)
  (:documentation "o hai, i send off reqs thru routing tubez"))

;;TODO rewrite
(defgeneric add-router (state rtr)
  (:method ((state brac-appstate) (rtr brac-router))
    "" ;; TODO
    nil)
  (:documentation ""))

(defgeneric del-router (state rtr-name)
  (:method ((state brac-appstate) rtr-name)
    (declare (type string rtr-name))
    (with-slots (routers) state
      (remhash rtr-name routers)))
  (:documentation ""))

(defgeneric load-builtin-routers (state)
  (:method ((state brac-appstate))
    (let ((fixed-router-callable ;; TODO: with :regex t option
	   (lambda (clack-http-req)
	     nil))
	  (static-file-router-callable
	   (lambda (clack-http-req) ;; TODO: build-folder-index, recursive, separator, controller


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
	   (lambda (req options)
	     nil))
	  (redirect-router-callable
	   (lambda (req options)
	     nil)))
      (add-router state (make-instance 'brac-router
				      :parent state
				      :name "fixed"
				      :callable fixed-router-callable
				      :source-file nil))
      (add-router state (make-instance 'brac-router
				      :parent state
				      :name "static"
				      :callable static-file-router-callable
				      :source-file nil))
      (add-router state (make-instance 'brac-router
				      :parent state
				      :name "code"
				      :callable code-router-callable
				      :source-file nil))
      (add-router state (make-instance 'brac-router
				      :parent state
				      :name "redirect"
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
