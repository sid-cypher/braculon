;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10 -*-

(in-package :braculon)

(defconstant version-major 0)
(defconstant version-minor 1)
(defconstant version-revision 1)

(define-constant conf-file-error
  "While a web project must be associated with a config file at all times, neither filename nor a
filename-specifying form was found in the provided :config argument." :test #'string=)
(define-constant form-read-error
  "Failed to extract data from file." :test #'string=)
(define-constant need-conf-file-arg
  "A filename is required to write the config file." :test #'string=)
(define-constant file-exists
  "File with that name already exists and the overwrite flag is not set." :test #'string=)
(define-constant config-list-wrong-head
  "Config file must begin with a \"braculon-settings\" as the title or a parenthesised list." :test #'string=)

(defvar *project-instances* '() "launched web projects with separate configs")
(defvar *hooks-running* '() "used to avoid accidental endless recursions when handling state changes")

(defmacro cat (&body bod)
  "because 'concatenate strings' is too much to type"
  `(concatenate 'string ,@bod))
(defmacro silence (&body bod)
  "mutes the stdout unless overridden"
  `(let ((*standard-output* (make-broadcast-stream)))
     ;;TODO: scan for *std-out* redefs in body.
    ,@bod))

(defclass state ()
  ())
(defclass braculon-state (state)
  ((name :reader name
	 :initform "[unnamed]"
	 :documentation "")
   (config-file :reader config-file
		:initform nil
		:documentation "")
   (ports :reader ports
	  :initform '()
	  :documentation "")
   (acceptors :reader acceptors
	      :documentation "")
   (project-root :reader project-root
		 :initform nil
		 :documentation "")
   (static-content-path :reader static-content-path
			:initform nil
			:documentation "")
   (dynamic-content-path :reader dynamic-content-path
			 :initform nil
			 :documentation "")
   (controllers-path :reader controllers-path
		     :initform nil
		     :documentation "")
   (routes-path :reader routes-path
		:initform nil
		:documentation "")
   (views-path :reader views-path
	       :initform nil
	       :documentation "")
   (use-src :reader use-src
	    :initform nil
	    :documentation "")
   (src-path :reader src-path
	     :initform nil
	     :documentation "")
   (allow-read-eval :reader allow-read-eval
		    :initform nil
		    :documentation "")
   (config-print-case :reader config-print-case
		      :initform :downcase
		      :documentation "")
   (render-who-include-symbol :reader render-who-include-symbol
			      :initform 'include
			      :documentation "")
   (launch-time :reader launch-time
		:documentation ""))
  (:documentation ""))

;; TODO macroexpand writers that call registered hooks
(defun (setf name) (value object)
  (declare (type string value))
  (setf (slot-value object 'name) value))
(defun (setf ports) (value object)
  (setf (slot-value object 'ports) value))

(defun read-form-file (filepath)
  (declare (type pathname filepath))
  (let ((truepath (cl-fad:file-exists-p filepath)))
    (if (not truepath) nil
	(with-open-file (stream truepath)
	  (read stream)))))
(defun find-instance-by-conf-file (conf-filepath)
  (find-if (lambda (tested-inst)
	     (equal conf-filepath (config-file tested-inst))) *project-instances*))
(defun find-instance-by-name (name)
  (declare (type string name))
    (find-if (lambda (tested-inst)
	       (string= name (name tested-inst))) *project-instances*))
(defun wizard (&key config-file name ports) ;;TODO more keys
  "Interactively create a new web project."
  nil)
(defun launch (config &key overwrite)
  (let ((obj (make-instance 'braculon-state :config config :overwrite overwrite)))
    ;; TODO somehow warn if already launched
    (unless (or (not obj)
		 (find-instance-by-conf-file (config-file obj))
		 (find-instance-by-name (name obj)))
      (push obj *project-instances*)
      (name obj))))
(defun finish (project-id)
  (declare (type (or string pathname) project-id))
  "Uses a name or a config file path of a launched project to finish it. Return T if a project was found, NIL otherwise."
  (let (found-by-name)
    (cond ((fad:file-exists-p project-id)
	   (when (find-instance-by-conf-file project-id)
	     (setq *project-instances*
		   (delete-if (lambda (tested-inst)
				(equal project-id (config-file tested-inst))) *project-instances*))
	     t))
	  ((setq found-by-name (find-instance-by-name project-id))
	   (setq *project-instances*
		 (delete-if (lambda (tested-inst)
			      (eq found-by-name tested-inst)) *project-instances*))
	   (when found-by-name
	     t)))))
(defun show-running ()
  "Prints a list of running web projects."
  (let ((namelist (mapcar #'name *project-instances*)))
    (format t "~{~A~%~}" namelist)
    ;; TODO: uptime
    namelist))

(defmethod print-object ((state braculon-state) stream)
  (print-unreadable-object (state stream :type t)
    (format stream "~A" (name state))))
(defmethod initialize-instance :after ((state braculon-state) &key config overwrite)
  (let (config-form config-path rootpath)
    ;;make sure we have both config data and a file to keep it there.
    (cond ((pathnamep config)
	   (setf config-path config)
	   ;; TODO restarts for all the reader conditions
	   (setf config-form (read-form-file config))
	   (unless config-form
	     (error "~A~%~A" form-read-error conf-file-error))
	   (unless (string= (symbol-name (first config-form))
			    (string-upcase "braculon-settings"))
	     (error config-list-wrong-head))
	   (setf config-form (rest config-form)))
	  ((consp config)
	   (setf config-form config)
	   (setf config-path (getf config-form :config-file))
	   (unless config-path
	     ;;TODO restart with filename input from debugger
	     (error need-conf-file-arg))
	   (when (or (not overwrite)
		     (fad:file-exists-p config-path))
					;TODO restart with overwrite
	     (error file-exists)))
	  (t (error conf-file-error)))
    ;;fill our object with config data
    (with-slots (name config-file ports project-root static-content-path
		      dynamic-content-path routes-path controllers-path
		      views-path use-src src-path allow-read-eval config-print-case
		      render-who-include-symbol) state
      (setf rootpath (getf config-form :project-root))
      (setf name (let ((raw-name (getf config-form :name)))
		   (if (symbolp raw-name)
		       (string-downcase (symbol-name raw-name))
		       (format nil "~A" raw-name)))
	    config-file config-path
	    ports (getf config-form :ports)
	    project-root (ensure-directories-exist rootpath)
	    static-content-path (ensure-directories-exist (merge-pathnames
							   (getf config-form :static-content-path) rootpath))
	    dynamic-content-path (ensure-directories-exist (merge-pathnames
							    (getf config-form :dynamic-content-path) rootpath))
	    routes-path (ensure-directories-exist (merge-pathnames
						   (getf config-form :routes-path) rootpath))
	    controllers-path (ensure-directories-exist (merge-pathnames
							(getf config-form :controllers-path) rootpath))
	    views-path (ensure-directories-exist (merge-pathnames
						  (getf config-form :views-path) rootpath))
	    use-src (getf config-form :use-src)
	    src-path (ensure-directories-exist (merge-pathnames
						(getf config-form :src-path) rootpath))
	    allow-read-eval (getf config-form :allow-read-eval)
	    config-print-case (getf config-form :config-print-case)
	    render-who-include-symbol (getf config-form :render-who-include-symbol)))))

(defgeneric write-config (braculon-state)
  (:documentation ""))

(defmethod state-report ((state braculon-state))
  ;;TODO
  (format t "A report for ~A goes here." state))

