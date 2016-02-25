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
(define-constant +init-app-with-rootpath+
  "Please specify the path to your app dir with the :ROOT-PATH key." :test #'string=)

(defvar *registered-apps* '() "List of web apps that have been already registered.")
(defvar *running-apps* '() "List of web apps that are running.")

;; TODO: move inside brac-app maybe?
;;(defvar *hooks-running* '() "used to avoid accidental endless recursions when handling app changes")

@export-class
(defclass brac-app ()
  ((name :reader name
	 :type 'string
	 :initarg :name
	 :initform (error "An app needs a name.")
	 :documentation "")
   (port :reader port
	 :type fixnum
	 :initform (error "A web app needs a port to listen on.")
	 :initarg :port
	 :documentation "")
   (is-running-p :reader is-running-p
		 :type 'boolean
		 :initform nil
		 :documentation "")
   (root-path :reader root-path
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
                   :initform "init"
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
	    :initarg :verbose
	    :documentation "")
   (app-package :accessor app-package ;;TODO: improve this
                :type keyword
                :initarg :app-package
                :initform :braculon)
   (launch-time :reader launch-time
		:documentation "")
   (clack-handler :initform nil))
  (:documentation "This object represents a web app and holds its settings.
You can pass an instance of this object to clack:clackup, as the necessary call method has already been defined for it."))

(defmethod print-object ((app brac-app) stream)
  (print-unreadable-object (app stream :type t :identity t)
    (format stream "\"~A\" ~A"
	    (name app)
	    (if (is-running-p app) "running"
		"stopped"))))

(defun wrap-request (clack-env app)
  (make-instance 'request-processing-state
		 :app app
		 :original-request clack-env
		 :request (hash-original-request clack-env)))

;;TODO: documentation for RESPONSE-CONTENT types.
(defun to-clack-response (rps)
  (let ((content (response-content rps)))
    (if (functionp content)
	(lack.component:call content (original-request rps))
	(list
	 (response-status-code rps)
	 (alexandria:hash-table-plist (response-headers rps))
	 (if (stringp content)
	     (list content)
	     content)))))
(defun clack-drop-request ()
  ;;TODO: make clack close connection without returning anything
  (error "Dropping requests not implemented yet, need to figure out Clack support for that."))

(defmethod lack.component:to-app ((app brac-app))
  "This method is called by Clack to get a callback function that will be used for each incoming HTTP request to your app."
  (when (verbosep app)
    (format t +connecting-clack+ (name app)))
  (lambda (clack-env)
    (process-request
     (wrap-request clack-env app))))

(defgeneric process-request (rps)
  (:method ((rps request-processing-state))
    (if (chain-pass rps)
        (to-clack-response rps)
        (clack-drop-request)))
  (:documentation ""))

;; TODO macroexpand writers that call registered hooks
(defun (setf name) (value object)
  (declare (type string value))
  (setf (slot-value object 'name) value))

;; TODO finish this
(defmethod initialize-instance :after ((app brac-app) &key root-path)
  (unless (uiop:directory-exists-p root-path)
    (error +root-path-doesnt-exist+ root-path))
  (load-builtin-actions app)
  (load-builtin-conditions app)
  (load-builtin-chains app)
  ;;TODO: check views for circular dependencies.
  )

;; TODO summoning skeletons, error handling
@export
(defun wizard (path-to-app)
  "Answer the questions of the Wizard of Braculon and behold his wondrous magic."
  path-to-app)

@export
(defun find-app (&optional spec)
  "Returns the app object with a name SPEC.
 When called with no arguments, finds an app with the same
 name as current package or the first app registered with
 that package's designator keyword in APP-PACKAGE slot."
  (declare (type (or string symbol brac-app null) spec))
  (if (typep spec 'brac-app)
      spec
      (let ((npspec (or spec (package-name *package*))))
        (or
         (find-if (lambda (an-app)
                    (string= (name an-app)
                             (name-to-downcase-string npspec)))
                  *registered-apps*)
         (find-if (lambda (an-app)
                    (eq (app-package an-app)
                        (name-to-keyword npspec)))
                  *registered-apps*)))))

;;TODO: use local-time
@export
(defun start (&key app (if-running :restart) (server :woo))
  "This function starts (or, if applicable, restarts) your application.
 You can provide the name of the app you are starting (string or symbol)
 as a key or leave it out, in which case the last app registered from
 the current package will be started.
 Starting an app includes adding it to the list of running apps, marking it
 as running and making it begin accepting web requests.

Use :SERVER key to pick a backend HTTP server from those supported by Clack.
Key :IF-RUNNING takes one of following values:
- :RESTART to restart the app if it was already running (default),
- :SKIP to do nothing if the app was already running,
- :ERROR to signal an error if, you guessed it, the app was running.
Unsurprisingly, if that app was not running, :IF-RUNNING has no effect."
  (declare (type (or brac-app string symbol) app)
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
(defun stop (&key app)
  (let ((app (find-app app)))
    (when app
      (with-slots (clack-handler launch-time is-running-p name) app
        (when clack-handler
          (clack:stop clack-handler)
          (setf clack-handler nil)
          (setf is-running-p nil)
          (setf *running-apps* (delete-if (lambda (an-app)
                                            (string= name (name an-app)))
                                          *running-apps*))
          t)))))

@export
(defun unregister-app (app-spec)
  (stop :app app-spec)
  (let ((appname (if (typep app-spec 'brac-app)
		     (name app-spec)
		     (name-to-downcase-string app-spec))))
    (setf *registered-apps*
          (delete-if (lambda (an-app)
                       (string= (name an-app) appname))
                     *registered-apps*))))

;; TODO: a function to change the listener port of a running app.
@export
(defun register-new-app (name &key port (package-spec (package-name *package*)) overwrite (verbose-app t))
  (declare (type (or string symbol) name package-spec)
           (type fixnum port)
           (type boolean overwrite verbose-app))
  (let ((app (find name *registered-apps*
                   :test (lambda (appname an-app)
                           (string= appname (name an-app))))))
    (when app
      (if overwrite
          (unregister-app app)
          (error "An app with the same name already exists.")))) ;;TODO l10n
  (let* ((package (name-to-keyword package-spec))
         (new-app (make-instance 'brac-app
                                 :name (name-to-downcase-string name)
                                 :port (or port 8500)
                                 :app-package package-spec
                                 :root-path (asdf:system-source-directory package)
                                 :verbose verbose-app)))
    (push new-app *registered-apps*)
    new-app))

(defmacro list-those-apps (applist)
  ;;internal macro, make it a function
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
(defun list-registered-apps (&key (print t) (detailed t))
  "Returns and optionally prints a list of registered web projects."
  (list-those-apps *registered-apps*))

@export
(defun list-running-apps (&key (print t) (detailed t))
  "Returns and optionally prints a list of running web projects."
  (list-those-apps *running-apps*))

@export
(defgeneric app-report (app)
  (:method ((app brac-app))
    ;;TODO
    nil))

;; TODO: when creating a new app skeleton, make sure there's a src/ folder
;; where all kinds of random code can be put, whether it's extentions/redefinitions
;; or something that doesn't fit in the framework, stuff like that.
