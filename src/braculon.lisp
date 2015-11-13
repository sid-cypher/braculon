(in-package :braculon)

;; get current version from the system definition file
(macrolet ((define-version-constants ()
	     (let* ((asd-form (uiop:with-safe-io-syntax (:package :braculon)
				(with-open-file (s (asdf:system-source-file :braculon)
						   :if-does-not-exist :error)
				  (read s))))
		    (version-string (getf (cddr asd-form) :version))
		    (version-list (map 'list
				       #'parse-integer
				       (cl-ppcre:split "\\." version-string
						       :end (min 8 (length version-string))
						       :limit 3 :sharedp t))))
	       (when (> 3 (length version-list))
		 (error "Could not parse :VERSION as major.minor.revision in braculon.asd"))
	       (progn
		 (defconstant version-major (first version-list))
		 (defconstant version-minor (second version-list))
		 (defconstant version-revision (third version-list))))))
  (define-version-constants))


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

(defvar *brac-apps* '() "launched web projects with separate configs")
;; TODO: move inside brac-appstate maybe?
(defvar *hooks-running* '() "used to avoid accidental endless recursions when handling state changes")

(defclass brac-corestate ()
  ())

(defclass brac-appstate (brac-corestate)
  ((name :reader name
	 :type 'string
	 :initform "[unnamed]"
	 :documentation "")
   (is-running-p :reader is-running-p
		 :type 'boolean
		 :initform nil
		 :documentation "")
   (root-path :reader project-root
	      :documentation "")
   (config-file :reader config-file
		:documentation "")
   (routers :reader routers
	   :initform (make-hash-table :test 'equal)
	   :documentation "")
   (routing-chain :reader routing-chain
		  :initform '()
		  :documentation "")
   (controllers :reader controllers
		:initform (make-hash-table :test 'equal)
		:documentation "")
   (view-compilers :reader view-compilers
		   :initform (make-hash-table :test 'equal)
		   :documentation "")
   (views :reader views
	  :initform (make-hash-table :test 'equal)
	  :documentation "")
   (static-content-path :reader static-content-path
			:documentation "")
   (routers-path :reader routers-path
		:documentation "")
   (controllers-path :reader controllers-path
		     :documentation "")
   (views-path :reader views-path
	       :documentation "")
   (view-compilers-path :reader view-compilers-path
			:documentation "")
   (extensions :initform (make-hash-table :test 'eq)
	       :documentation "Additional parameters can be injected here by loadable modules at runtime.")
   (launch-time :reader launch-time
		:documentation ""))
  (:documentation "This object represents a web app and holds its settings.
You can pass an instance of this object to clack:clackup, as the necessary call method has already been defined for it."))

(defmethod print-object ((state brac-appstate) stream)
  (print-unreadable-object (state stream :type t :identity t)
    (format stream "\"~A\" ~A"
	    (name state)
	    (if (is-running-p state) "running"
		"stopped"))))

;; TODO macroexpand writers that call registered hooks
(defun (setf name) (value object)
  (declare (type string value))
  (setf (slot-value object 'name) value))

;; TODO change to settings.conf.lisp specs
(defun load-config-file-settings (config overwrite)
  (let (config-form config-path)
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
		     (uiop:file-exists-p config-path))
					;TODO restart with overwrite
	     (error file-exists)))
	  (t (error conf-file-error)))
    (values config-form config-path)))


;; TODO yeah all the slots have changed, need to fix this one, too
(defun fill-slots-with-config-file-settings (config-form config-path brac-appstate)
  (with-slots (name config-file ports project-root static-content-path
		    dynamic-content-path routers-path controllers-path
		    views-path use-src src-path allow-read-eval config-print-case
		    render-who-include-symbol) brac-appstate
    (let (rootpath)
      (setf rootpath (getf config-form :project-root))
      ;; TODO: thoroughly check user inputs from config file
      (setf
       name (let ((raw-name (getf config-form :name)))
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
       routers-path (ensure-directories-exist (merge-pathnames
					       (getf config-form :routers-path) rootpath))
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

;; TODO init from new conf
(defmethod initialize-instance :after ((state brac-appstate) &key config overwrite)
  (let (config-form config-path)
    ;;make sure we have both config data and a file to keep it there.
    (multiple-value-setq (config-form config-path)
      (load-config-file-settings config overwrite))
    (fill-slots-with-config-file-settings config-form config-path state)
    ;; TODO: maybe load views
    (load-builtin-routers state)
    (load-builtin-controllers state)
    (load-router-files state)
    (load-controller-files state)
    (load-view-files state)))

;; TODO summoning skeletons
(defun wizard (path-to-app)
  "Answer the questions of the Wizard of Braculon and behold his wondrous magic."
  nil)

;; TODO optional config, wizard on *query-io*
(defmethod start ((state brac-appstate) &key if-running)
  nil)

(defmethod stop ((state brac-appstate))
  nil)

(defun show-running ()
  "Prints a list of running web projects."
  (let ((namelist (mapcar #'name *project-instances*)))
    (format t "~{~A~%~}" namelist)
    ;; TODO: uptime
    namelist))

(defmethod state-report ((state brac-appstate))
  ;;TODO
  nil)

