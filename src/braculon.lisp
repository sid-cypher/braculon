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
(defvar *appstate* nil "Used in macros when loading files")

;; TODO: move inside brac-appstate maybe?
(defvar *hooks-running* '() "used to avoid accidental endless recursions when handling state changes")

@export-class
(defclass brac-appstate ()
  ((name :reader name
	 :type 'string
	 :initform "[unnamed]"
	 :documentation "")
   (is-running-p :reader is-running-p
		 :type 'boolean
		 :initform nil
		 :documentation "")
   (root-path :reader root-path
	      :documentation "")
   (config-file :reader config-file
		:documentation "")
   (routers :reader routers
	    :initform (make-hash-table :test 'eq)
	    :documentation "")
   (routing-chain :reader routing-chain
		  :initform '()
		  :documentation "")
   (controllers :reader controllers
		:initform (make-hash-table :test 'eq)
		:documentation "")
   (view-compilers :reader view-compilers
		   :initform (make-hash-table :test 'eq)
		   :documentation "")
   (views :reader views
	  :initform (make-hash-table :test 'eq)
	  :documentation "")
   (routers-path :reader routers-path
		 :documentation "")
   (controllers-path :reader controllers-path
		     :documentation "")
   (views-path :reader views-path
	       :documentation "")
   (view-compilers-path :reader view-compilers-path
			:documentation "")
   (verbose :type boolean
	    :reader verbosep
	    :initform t
	    :documentation "")
   (extensions :reader extensions
	       :initform (make-hash-table :test 'eq)
	       :documentation "Additional parameters can be injected here by loadable modules at runtime.")
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

(defgeneric process-request (env)
  (:method ((env brac-reqstate))
    (chain-route-request env)
    (call-controller env)
    (render env)
    (response env))
  (:documentation ""))

(defmethod lack.component:to-app ((state brac-appstate))
  "This method is called by Clack to get a callback function that will be used for each incoming HTTP request to your app."
  (when (verbosep state)
    (format t +connecting-clack+ (name state)))
  (lambda (clack-env)
    (process-request
     (wrap-request clack-env state))))

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
  (with-slots (name root-path config-file routing-chain routers-path
		    controllers-path views-path view-compilers-path extensions verbose) appstate
    ;; TODO: thoroughly check user inputs from config file
    (let ((r-path (uiop:merge-pathnames*
		   (getf config-form :routers-path #p"routers/") given-root-path))
	  (c-path (uiop:merge-pathnames*
		   (getf config-form :controllers-path #p"controllers/") given-root-path))
	  (v-path (uiop:merge-pathnames*
		   (getf config-form :views-path #p"views/") given-root-path))
	  (vc-path (uiop:merge-pathnames*
		    (getf config-form :view-compilers-path #p"viewcc/") given-root-path)))
      (macrolet ((test-paths (symlist)
		   (let (testcode)
		     ;; repeat testing code snippet for all paths.
		     (dolist (x-path symlist)
		       (let ((subpath (gensym)))
			 (push `(let ((,subpath (uiop:subpathp ,x-path given-root-path)))
				  (unless (and (pathname-directory ,subpath)
					       (ensure-directories-exist ,x-path :verbose t))
				    (error +conf-dirs-subpaths+)))
			       testcode)))
		     (push 'progn testcode))))
	(test-paths
	 (r-path c-path v-path vc-path)))
      (setf
       name (let ((raw-name (getf config-form :name)))
	      (if (symbolp raw-name)
		  (string-downcase (symbol-name raw-name))
		  (format nil "~A" raw-name)))
       root-path given-root-path
       config-file config-path
       routing-chain (let ((rc (getf config-form :routing-chain)))
		       (if (consp rc)
			   (if (and (symbolp (car rc))
				    (string= "QUOTE"
					     (symbol-name (car rc))))
			       (cadr rc)
			       rc)
			   (error "Routing chain not a list or empty.")))
       routers-path r-path
       controllers-path c-path
       views-path v-path
       view-compilers-path vc-path
       extensions (getf config-form :extensions)
       verbose (getf config-form :verbose t)))))

;; TODO finish this
(defmethod initialize-instance :after ((state brac-appstate) &key root-path)
  (unless root-path
    (error +init-appstate-with-rootpath+))
  (let (config-form config-path)
    (multiple-value-setq (config-form config-path)
      (load-config-file-settings root-path))
    (fill-slots-with-config-file-settings config-form config-path root-path state)
    (load-builtin-routers state)
    (load-builtin-controllers state)
    (load-router-files state)
    (load-controller-files state)
    (load-view-files state)
    ))

;; TODO summoning skeletons
@export
(defun wizard (path-to-app)
  "Answer the questions of the Wizard of Braculon and behold his wondrous magic."
  nil)

@export
(defun find-app (name)
  (declare (type (or string symbol brac-appstate) name))
  (if (typep name 'brac-appstate) name
      (find name *loaded-apps*
	:test (lambda (namearg an-app)
		(string= (name an-app)
			 (symbol-to-downcase-string namearg))))))

;;TODO: use local-time
@export
(defgeneric start (appstate &key if-running server port)
  (:method (app &key (if-running :restart) (server :woo) (port 5000))
    (let ((state (find-app app)))
      (when state
	(start state :if-running if-running :server server :port port))))
  (:method ((appstate brac-appstate) &key (if-running :restart) (server :woo) (port 5000))
    (declare (type (member :error :skip :restart) if-running))
    (flet ((actually-start ()
	     (let ((clack-result (clack:clackup appstate :server server :port port
						:use-default-middlewares nil)))
	       (when clack-result
		 (with-slots (clack-handler launch-time is-running-p) appstate
		   (setf clack-handler clack-result)
		   (setf launch-time (get-universal-time))
		   (setf is-running-p t)
		   (push appstate *running-apps*)
		   clack-result)))))
      (if (is-running-p appstate)
	  (ecase if-running
	    (:skip nil)
	    (:restart (stop appstate)
		      (actually-start))
	    ;;TODO: define errmsg string constant elsewhere
	    (:error (error "The app you have tried to start is already running.")))
	  (actually-start))))
  (:documentation
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
Unsurprisingly, if that app was not running, :IF-RUNNING has no effect."))

@export
(defgeneric stop (appstate)
  (:method (name)
    (let ((state (find-app name)))
      (when state
	(stop state))))
  (:method ((state brac-appstate))
    (with-slots (clack-handler launch-time is-running-p name) state
	(when clack-handler
	  (clack:stop clack-handler)
	  (setf clack-handler nil)
	  (setf is-running-p nil)
	  (setf *running-apps* (delete-if (lambda (an-app)
					    (string= name (name an-app)))
					  *running-apps*))
	  t))))

@export
(defun unload-app (app)
  (stop app)
  (let ((appname (if (typep app 'brac-appstate)
		     (name app)
		     (symbol-to-downcase-string app))))
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
  (let* ((state (find-app app))
	 (was-running (is-running-p state)))
    (unload-app state)
    (let ((new-state (load-app (root-path state))))
      (when was-running
	(start new-state))
      new-state)))

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
(defgeneric state-report (state)
  (:method ((state brac-appstate))
    ;;TODO
    nil))

;; TODO: when creating a new app skeleton, make sure there's a src/ folder
;; where all kinds of random code can be put, whether it's extentions/redefinitions
;; or something that doesn't fit in the framework, stuff like that.
