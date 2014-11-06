;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10 -*-

(in-package :braculon)

(defclass brac-acceptor (hunchentoot:acceptor)
  ((project-state :reader project-state
		  :initarg :parent
		  :initform (error "Please specify the project that will use this object.")
		  :documentation ""))
  (:default-initargs
   :request-class 'brac-request))

(defclass brac-request (hunchentoot:request)
  ((router-data :reader router-data
	       :initform '()
	       :documentation "")))

(defclass brac-router ()
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

(defmethod print-object ((rtr brac-router) stream)
  (print-unreadable-object (rtr stream :type t)
    (format stream "~A" (name rtr))))

(defgeneric route-request (request)
  (:documentation "o hai, i send off reqs thru routing tubez"))

(defgeneric add-router (state rtr)
  (:documentation ""))

(defgeneric del-router (state rtr-name)
  (:documentation ""))

(defgeneric load-builtin-routers (state)
  (:documentation ""))

(defgeneric load-router-files (state)
  (:documentation ""))

;;; TODO: check interference between process requests
(defvar *request-interference* nil)

(defun start-output (return-code &optional (content nil content-provided-p))
  "Wrapper around Hunchentoot's"
  (if content-provided-p (hunchentoot::start-output return-code content)
      (hunchentoot::start-output return-code)))

(defmethod process-request ((req brac-request))
  "same as the Hunchentoot standard implementation, but with HANDLE-REQUEST
replaced by ROUTE-REQUEST."
  (catch 'hunchentoot::request-processed ; maybe thrown in START-OUTPUT to end request processing
    (let (hunchentoot::*tmp-files* ;; TODO remove all traces of this
	  hunchentoot::*headers-sent*
	  (hunchentoot::*request* req))
      (hunchentoot::with-mapped-conditions ()
	(labels
	    ;; TODO : Review logging mechanism
	    ((report-error-to-client (error &optional backtrace)
	       (when *log-lisp-errors-p*
		 (log-message* *lisp-errors-log-level* "~A~@[~%~A~]" error (when *log-lisp-backtraces-p*
									     backtrace)))
	       ;; TODO: remember the error messages and route them to a fitting controller,
	       ;; to be rendered in a more sophisticated way.
	       ;; TODO: BRAC-REQUEST should carry the parent acceptor and the child reply in slots
	       (start-output +http-internal-server-error+
			     (acceptor-status-message *acceptor* ;; TODO no specials plz
						      +http-internal-server-error+
						      :error (princ-to-string error)
						      :backtrace (princ-to-string backtrace)))))
	  (multiple-value-bind (contents error backtrace)
	      ;; TODO: why skip dispatch if bad request? handle it.
	      (catch 'hunchentoot::handler-done ; thrown on error within, otherwise bound values are nil.
		(route-request req))
	    ;; TODO better log handling
	    (when error
	      ;; error occurred in request handler
	      (report-error-to-client error backtrace))
	    (handler-case
		(hunchentoot::with-debugger
		    (start-output (return-code *reply*) ;; TODO: get rid of special vars
				  (or contents
				      (acceptor-status-message *acceptor*
							       (return-code *reply*)))))
	      (error (e)
		;; error occurred while writing to the client.  attempt to report.
		(report-error-to-client e)))))))))

;;; TODO: Make HANDLE-STATIC-FILE into a controller.
(defmethod route-request ((req brac-request))
  "Offers the request to registered routers until one of them accepts or all of
them refuse. Also sets up standard error handling which catches any errors
within the handler."
  ;;; TODO implement actual downlist dispatching through available routers.
  (handler-bind ((error
                  (lambda (cond)
                    ;; if the headers were already sent, the error
                    ;; happened within the body and we have to close
                    ;; the stream
                    (when hunchentoot::*headers-sent*
                      (setq hunchentoot::*close-hunchentoot-stream* t))
                    (throw 'hunchentoot::handler-done
                      (values nil cond (hunchentoot::get-backtrace)))))
		 ;; TODO: review logging mechanism
                 (warning
                  (lambda (cond)
                    (when *log-lisp-warnings-p*
                      (log-message* *lisp-warnings-log-level* "~A" cond)))))
    (hunchentoot::with-debugger
      ;; ACCEPTOR-DISPATCH-REQUEST was here.
      ;; routers return nil or controller name
      ;; controller return a string or an octet array
      ;; - return it here as well
      (let ((state (project-state (request-acceptor req)))
	    chosen-controller-name
	    reply-content)
	(flet ((find-and-call (state req name opts)
		 (let (working-router)
		   (setf working-router (gethash (if (symbolp name)
						     (string-downcase (symbol-name name))
						     (the string name))
						 (routers state)))
		   (when working-router ;; TODO: log message if not found
		     ;; TODO sanitize options
		     (funcall (callable working-router) req opts)))))
	  (loop for ordered-router-args in (routers-order state)
	     while (not chosen-controller-name) do
	       (cond ((or (symbolp ordered-router-args)
			  (stringp ordered-router-args))
		      (setf chosen-controller-name
			    (find-and-call state req
					   ordered-router-args nil)))
		     ((consp ordered-router-args)
		      (setf chosen-controller-name
			    (find-and-call state req
					   (first ordered-router-args)
					   (rest ordered-router-args)))) ;; TODO
		     ;; TODO fail more gracefully
		     (t (error "Only symbols, strings and lists are allowed in order.conf"))))

	  (when chosen-controller-name
	    (setf reply-content
		  (funcall (callable (gethash chosen-controller-name (controllers state)))
			   req)))

	  ;;TODO think of a sane fallback, push info through the log system
	  (or reply-content
	      (progn (setf (hunchentoot:return-code *reply*) +http-not-found+)
		     "o hai")))))))

(defmethod load-builtin-routers ((state project-state))
  (let ((fixed-router-callable ;; with :regex t option
	 (lambda (req options) ;; TODO
	   (values "hello")))
	(static-router-callable
	 (lambda (req options)
	   nil))
	(dynamic-router-callable
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
				     :callable static-router-callable
				     :source-file nil))
    (add-router state (make-instance 'brac-router
				     :parent state
				     :name "dynamic"
				     :callable dynamic-router-callable
				     :source-file nil))
    (add-router state (make-instance 'brac-router
				     :parent state
				     :name "redirect"
				     :callable redirect-router-callable
				     :source-file nil))
    t))

(defmethod add-router ((state project-state) (rtr brac-router))
  "" ;; TODO
  (with-slots (routers router-names) state
    (let ((rtr-name (name rtr)))
      (push rtr-name router-names)
      (setf (gethash rtr-name routers) rtr))))

(defmethod del-router ((state project-state) rtr-name)
  (with-slots (routers router-names) state
    (remove rtr-name router-names :test #'string=)
    (remhash rtr-name routers)))

(defmethod load-router-files ((state project-state))
  (let ((default-order '(braculon::static braculon::dynamic (braculon::fixed "/" "index")))
	(order-file (merge-pathnames #p"order.conf" (routers-path state)))
	order-form)
    (setf order-form (read-form-file order-file))
    (unless order-form
      (with-open-file (filestream order-file ;; TODO handle all exceptions
				  :direction :output
				  :if-does-not-exist :create
				  :if-exists :rename)
	;; TODO writeout default "index" controller elsewhere
	(let ((*package* (find-package :braculon)))
	  (write default-order
		 :case (config-print-case state)
		 :stream filestream)))
      (setf order-form default-order))
    (setf (slot-value state 'routers-order) order-form)
    ;; TODO load router objects from files into state
    ))
