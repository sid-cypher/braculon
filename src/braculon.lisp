(in-package :braculon)
(use-package :annot.class)
(annot:enable-annot-syntax)

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
	       `(progn
		  (defconstant version-major ,(first version-list))
		  (defconstant version-minor ,(second version-list))
		  (defconstant version-revision ,(third version-list))
		  t))))
  (define-version-constants))


(define-constant +form-read-error+
  "Failed to extract data from file." :test #'string=)
(define-constant +need-conf-file-arg+
  "A filename is required to write the config file." :test #'string=)
(define-constant +config-list-wrong-head+
  "Config file must begin with an \"app-config\" as the first element of a list." :test #'string=)
(define-constant +root-path-doesnt-exist+
  "Directory ~A does not exist." :test #'string=)
(define-constant +connecting-clack+
  "Connecting app (~A) to server via Clack.~%" :test #'string=)
(define-constant +conf-dirs-subpaths+
   "One or more paths in app-config do not exist or are not subpaths of root." :test #'string=)
(define-constant +init-appstate-with-rootpath+
   "Please specify the path to your app dir with the :ROOT-PATH key." :test #'string=)

(defvar *loaded-apps* '() "List of web apps that have been already loaded.")
(defvar *running-apps* '() "List of web apps that are running.")

@export
(defvar *appstate* nil "Default argument in macros and functions")
@export
(defvar *reqstate* nil "Default argument in macros and functions")

;; TODO: move inside brac-appstate maybe?
(defvar *hooks-running* '() "used to avoid accidental endless recursions when handling state changes")

@export-class
(defclass brac-appstate ()
  ((name :reader name
	 :type 'string
	 :initform "[unnamed]"
	 :documentation "")
   (port :reader port
	 :type fixnum
	 :initform 5000
	 :documentation "")
   (is-running-p :reader is-running-p
		 :type 'boolean
		 :initform nil
		 :documentation "")
   (root-path :reader root-path
	      :documentation "")
   (config-file :reader config-file
		:documentation "")
   (chains :reader chains
           :initform (make-hash-table :test 'equal)
           :documentation "")
   (conditions :reader conditions
               :initform (make-hash-table :test 'equal)
               :documentation "")
   (actions :reader actions
            :initform (make-hash-table :test 'equal)
            :documentation "")
   (starting-chain :accessor starting-chain ;;TODO: custom setter
                   :initform "brac-init"
                   :documentation "")
   (max-chain-hops :accessor max-chain-hops
                   :initform 200
                   :type fixnum
                   :documentation "")
   (view-compilers :reader view-compilers
		   :initform (make-hash-table :test 'equal)
		   :documentation "")
   (views :reader views
	  :initform (make-hash-table :test 'equal)
	  :documentation "")
   (verbose :type boolean
	    :reader verbosep
	    :initform t
	    :documentation "")
   (reader-package :accessor reader-package ;;TODO: improve this
                   :initform :braculon)
   (launch-time :reader launch-time
		:documentation "")
   (clack-handler :initform nil))
  (:documentation "This object represents a web app and holds its settings.
You can pass an instance of this object to clack:clackup, as the necessary call method has already been defined for it."))

(defmethod print-object ((state brac-appstate) stream)
  (print-unreadable-object (state stream :type t :identity t)
    (format stream "\"~A\" ~A"
	    (name state)
	    (if (is-running-p state) "running"
		"stopped"))))

(defun wrap-request (clack-env appstate)
  (make-instance 'brac-reqstate
		 :appstate appstate
		 :original-request clack-env
		 :request (hash-original-request clack-env)))

;;TODO: documentation for RESPONSE-CONTENT types.
(defun to-clack-response (rs)
  (let ((content (response-content rs)))
    (if (functionp content)
	(lack.component:call content (original-request rs))
	(list
	 (response-status-code rs)
	 (alexandria:hash-table-plist (response-headers rs))
	 (if (stringp content)
	     (list content)
	     content)))))
(defun clack-drop-request ()
  ;;TODO: make clack close connection without returning anything
  (error "Dropping requests not implemented yet, need to figure out Clack support for that."))

(defmethod lack.component:to-app ((state brac-appstate))
  "This method is called by Clack to get a callback function that will be used for each incoming HTTP request to your app."
  (when (verbosep state)
    (format t +connecting-clack+ (name state)))
  (lambda (clack-env)
    (process-request
     (wrap-request clack-env state))))

(defgeneric process-request (rs)
  (:method ((rs brac-reqstate))
    (if (chain-pass rs)
        (to-clack-response rs)
        (clack-drop-request)))
  (:documentation ""))

;; TODO macroexpand writers that call registered hooks
(defun (setf name) (value object)
  (declare (type string value))
  (setf (slot-value object 'name) value))

(defun load-config-file-settings (root-path)
  (declare (type pathname root-path))
  (unless (uiop:directory-exists-p root-path)
    (error +root-path-doesnt-exist+ root-path))
  (let (config-form config-path)
    (setf config-path (uiop:merge-pathnames* #p"settings.conf.lisp" root-path))
    (setf config-form (read-first-form-file config-path))
    ;; TODO better read-first-form-file error handling
    (unless config-form
      (error "~A~%" +form-read-error+))
    (unless (string= (symbol-name (first config-form))
		     (string-upcase "app-config"))
      (error +config-list-wrong-head+))
    (setf config-form (rest config-form))
    (values config-form config-path)))

(defun fill-slots-with-config-file-settings (config-form config-path given-root-path appstate)
  (with-slots (name port root-path config-file reader-package verbose) appstate
    ;; TODO: thoroughly check user inputs from config file
    (setf
     name (name-to-downcase-string
           (getf config-form :name))
     port (getf config-form :port)
     root-path given-root-path
     config-file config-path
     reader-package (getf config-form :reader-package)
     verbose (getf config-form :verbose t))))

;; TODO finish this
(defmethod initialize-instance :after ((appstate brac-appstate) &key root-path)
  (unless root-path
    (error +init-appstate-with-rootpath+))
  (let (config-form config-path)
    (multiple-value-setq (config-form config-path)
      (load-config-file-settings root-path))
    (fill-slots-with-config-file-settings config-form config-path root-path appstate)
    (load-builtin-actions appstate)
    (load-builtin-conditions appstate)
    (load-builtin-chains appstate)
    ;;TODO: check views for circular dependencies.
    ))

;; TODO summoning skeletons, error handling
@export
(defun wizard (path-to-app)
  "Answer the questions of the Wizard of Braculon and behold his wondrous magic."
  path-to-app)

@export
(defun find-app (name)
  (declare (type (or string symbol brac-appstate) name))
  (if (typep name 'brac-appstate) name
      (find name *loaded-apps*
            :test (lambda (namearg an-app)
                    (string= (name an-app)
                             (name-to-downcase-string namearg))))))

;;TODO: use local-time
@export
(defun start (&key (app *appstate*) (if-running :restart) (server :woo))
  "This function starts (or, if applicable, restarts) your application.
 It accepts an app object (returned by LOAD-APP) or its name (string or symbol)
 as an argument. Providing a name results in calling FIND-APP internally.
 Starting an app includes adding it to *RUNNING-APPS* list, marking it as running
 and making it begin accepting web requests.

Use :SERVER key to pick a backend HTTP server from those supported by Clack.
With :PORT key you can specify which port the server should listen to.
Key :IF-RUNNING takes one of following values:
- :RESTART to restart the app if it was already running (default),
- :SKIP to do nothing if the app was already running,
- :ERROR to signal an error if, you guessed it, the app was running.
Unsurprisingly, if that app was not running, :IF-RUNNING has no effect."
  (declare (type (or brac-appstate string symbol) app)
	   (type (member :error :skip :restart) if-running)
	   (type symbol server))
  (setf app (find-app app))
  (flet ((actually-start ()
	   (let ((clack-result (clack:clackup app :server server :port (port app)
						       :use-default-middlewares nil)))
	     (when clack-result
	       (with-slots (clack-handler launch-time is-running-p) app
		 (setf clack-handler clack-result)
		 (setf launch-time (get-universal-time))
		 (setf is-running-p t)
		 (push app *running-apps*)
		 clack-result)))))
    (if (is-running-p app)
	(ecase if-running
	  (:skip nil)
	  (:restart (stop :app app)
	   (actually-start))
	  ;;TODO: define errmsg string constant elsewhere
	  (:error (error "The app you have tried to start is already running.")))
	(actually-start))))

@export
(defun stop (&key (app *appstate*))
  (let ((appstate (find-app app)))
    (when appstate
      (with-slots (clack-handler launch-time is-running-p name) appstate
	(when clack-handler
	  (clack:stop clack-handler)
	  (setf clack-handler nil)
	  (setf is-running-p nil)
	  (setf *running-apps* (delete-if (lambda (an-app)
					    (string= name (name an-app)))
					  *running-apps*))
	  t)))))

@export
(defun unload-app (app)
  (stop :app app)
  (let ((appname (if (typep app 'brac-appstate)
		     (name app)
		     (name-to-downcase-string app))))
    (setf *loaded-apps*
          (delete-if (lambda (an-app)
                       (string= (name an-app) appname))
                     *loaded-apps*))))

@export
(defun load-app (path)
  (let ((app (find path *loaded-apps*
		   :test (lambda (pn an-app)
			   (uiop:pathname-equal pn (root-path an-app))))))
    (when app
      (unload-app app)))
  (let ((new-app (make-instance 'brac-appstate :root-path path)))
    (push new-app *loaded-apps*)
    new-app))

@export
(defun reload-app (app)
  (let* ((appstate (find-app app))
	 (was-running (is-running-p appstate)))
    (unload-app appstate)
    (let ((new-appstate (load-app (root-path appstate))))
      (when was-running
	(start :app new-appstate))
      new-appstate)))

(defmacro list-those-apps (applist)
  `(if (not print)
       (mapcar #'name ,applist)
       (loop for app in ,applist
	  with name
	  do (progn
	       (setf name (name app))
	       (if (and detailed
			(is-running-p app))
		   (multiple-value-bind (d m h s)
		       (uptime-seconds-to-dhms (launch-time app))
		     (format t (mcat "~A (uptime: ~[~*~:;~D days, ~]"
				     "~[~*~:;~D hours, ~]"
				     "~[~*~:;~D minutes, ~]"
				     "~D seconds.)~%") name d d m m h h s))
		   (format t "~A~%" name)))
	  collect name into names
	  return names)))

@export
(defun list-loaded-apps (&key (print t) (detailed t))
  "Returns and optionally prints a list of loaded web projects."
  (list-those-apps *loaded-apps*))

@export
(defun list-running-apps (&key (print t) (detailed t))
  "Returns and optionally prints a list of running web projects."
  (list-those-apps *running-apps*))

@export
(defgeneric state-report (appstate)
  (:method ((appstate brac-appstate))
    ;;TODO
    nil))

;; TODO: when creating a new app skeleton, make sure there's a src/ folder
;; where all kinds of random code can be put, whether it's extentions/redefinitions
;; or something that doesn't fit in the framework, stuff like that.
