;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10 -*-

(in-package :braculon)

(defconstant version-major 0)
(defconstant version-minor 1)
(defconstant version-revision 1)

(defconstant conf-file-error
  "While a web project must be associated with a config file at all times, neither filename nor a filename-specifying form was found in the provided :config argument.")
(defconstant form-read-error
  "Failed to extract data from file.")
(defconstant need-conf-file-arg
  "A filename is required to write the config file.")
(defconstant file-exists
  "File with that name already exists and the overwrite flag is not set.")
(defconstant config-list-wrong-head
  "Config file must begin with a \"braculon-settings\" as the title or a parenthesised list.")

(defvar *braculon-instances* '())

(defmacro cat (&body bod)
  `(concatenate 'string ,@bod))
(defmacro silence (&body bod)
  `(let ((*standard-output* (make-broadcast-stream)))
     ;;TODO: scan for *std-out* redefs in body.
    ,@bod))

(defun read-form-file (filepath)
  (declare (type pathname filepath))
  (let ((truepath (cl-fad:file-exists-p filepath)))
    (if (not truepath) nil
	(with-open-file (stream truepath)
	  (read stream)))))

(defclass state ()
  ())
(defclass braculon-state (state)
  ((name :reader name
	 :initform 'give-me-a-name
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
			      :documentation ""))
  (:documentation ""))

(defun (setf name) (value object)
  (setf (slot-value object 'name) value))
(defun (setf ports) (value object)
  (setf (slot-value object 'ports) value))

(defmethod print-object ((state braculon-state) stream)
  (print-unreadable-object (state stream :type t)
    (format stream "~A" (name state))))
(defmethod initialize-instance :after ((state braculon-state) &key config overwrite)
  (let (config-form config-path rootpath)
    ;;make sure we have both config data and a file to keep it there.
    (cond ((pathnamep config)
	   (setf config-path config)
	   (setf config-form (read-form-file config))
	   (unless config-form
	     (error "~A~%~A" form-read-error conf-file-error))
	   (unless (eq (first config-form) 'braculon-settings)
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
      (setf name (getf config-form :name)
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
	    render-who-include-symbol (getf config-form :render-who-include-symbol))
      )
))

(defgeneric report (state)
  (:documentation ""))

(defmethod report ((state braculon-state))
  ;;TODO
  (format t "A report for ~A goes here." state))

