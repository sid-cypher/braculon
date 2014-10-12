;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10 -*-

(in-package :braculon)

(defclass bracceptor (hunchentoot:acceptor) ;; now else can i call it?
  ((project-parent :reader project-parent
		 :initarg :parent
		 :initform (error "Please specify the project that will use this object.")
		 :documentation ""))
  (:default-initargs
   :request-class 'brequest))

(defclass brequest (hunchentoot:request) ;; naming things is hard
  ())

;;; TODO: check interference between process requests
(defvar *request-interference* nil) ;; only used in PROCESS-REQUEST

(defun start-output (return-code &optional (content nil content-provided-p))
  "Wrapper around Hunchentoot's"
  (if content-provided-p (hunchentoot::start-output return-code content)
      (hunchentoot::start-output return-code)))

(defmethod process-request ((req brequest))
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
		  (start-output (return-code *reply*)
				(or contents
				    (acceptor-status-message *acceptor*
							     (return-code *reply*)))))
	      (error (e)
		;; error occurred while writing to the client.  attempt to report.
		(report-error-to-client e)))))))))


(defgeneric route-request (request)
  (:documentation "o hai, i send off reqs thru routing tubez"))

(defmethod route-request ((req brequest))
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
      ;; TODO replace with proper routing
      ;;   router returns nil or controller name
      (let ((path (and (static-content-path (project-parent (request-acceptor req)))
		       (request-pathname req))))
	(cond
	  (path
	   (hunchentoot::handle-static-file ; peek there
	    (merge-pathnames (if (equal "/" (script-name req)) #p"index.html" path)
			     path)))
	  (t
	   (setf (return-code *reply*) +http-not-found+)
	   (hunchentoot::abort-request-handler)))))))
