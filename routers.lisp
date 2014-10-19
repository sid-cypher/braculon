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
  ())

(defclass router-state ()
  ((project-state :reader project-state
		  :initarg :parent
		  :initform (error "Please specify the project that will use this object.")
		  :documentation "")
   (callable :reader callable
	     :initarg :callable
	     :documentation "")
   (source-file :reader source-file
		:initarg :source-file
		:documentation "")
   (load-time :reader load-time
	      :initarg :load-time
	      :documentation "")))

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
	       (start-output +http-internal-server-error+
			     (acceptor-status-message *acceptor*
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
		  (start-output (return-code *reply*) ;; TODO: get rid of special var reply
				(or contents
				    (acceptor-status-message *acceptor*
							     (return-code *reply*)))))
	      (error (e)
		;; error occurred while writing to the client.  attempt to report.
		(report-error-to-client e)))))))))

(defgeneric route-request (request)
  (:documentation "o hai, i send off reqs thru routing tubez"))

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
      ;; TODO replace with file-based routing
      ;;   routers return nil or controller name
      ;;   call them in user-defined order, pass request to resulting controller.
      ;; need own fallback instead of CALL-NEXT-METHOD
      )))
