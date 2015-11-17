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
   (extensions :initform (make-hash-table :test 'eq)
	       :documentation "Additional parameters can be injected here by loadable modules at runtime.")
   (launch-time :reader launch-time
		:documentation "")
   (clack-handler))
  (:documentation "This object represents a web app and holds its settings.
You can pass an instance of this object to clack:clackup, as the necessary call method has already been defined for it."))

(defmethod print-object ((state brac-appstate) stream)
  (print-unreadable-object (state stream :type t :identity t)
    (format stream "\"~A\" ~A"
	    (name state)
	    (if (is-running-p state) "running"
		"stopped"))))

(defmethod lack.component:to-app ((state brac-appstate))
  "This method is called by Clack to get a callback function that will be used for each incoming HTTP request to your app."
  (when (verbosep state)
    (format t +connecting-clack+ (name state)))
  (lambda (env)
    (chain-route-request state env)))

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
    (setf config-form (read-form-file config-path))
    ;; TODO better read-form-file error handling
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
       ;;TODO: check that routing-chain isn't quoted
       routing-chain (getf config-form :routing-chain)
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
    ;;(load-builtin-controllers state)
    ;;(load-router-files state)
    ;;(load-controller-files state)
    ;; TODO: maybe load views
    ;;(load-view-files state)
    ))

;; TODO summoning skeletons
(defun wizard (path-to-app)
  "Answer the questions of the Wizard of Braculon and behold his wondrous magic."
  nil)

;; TODO optional config, wizard on *query-io*
(defgeneric start (state &key if-running server port)
  (:method ((state brac-appstate) &key if-running (server :woo) (port 5000))
    (let ((clack-result (clack:clackup state :server server :port port
					:use-default-middlewares nil)))
      (when clack-result
	(with-slots (clack-handler launch-time is-running-p) state
	  (setf clack-handler clack-result)
	  (setf launch-time (get-universal-time))
	  (setf is-running-p t)
	  clack-result)))))

(defgeneric stop (state)
  (:method ((state brac-appstate))
    (with-slots (clack-handler launch-time is-running-p) state
	(when clack-handler
	  (clack:stop clack-handler)
	  (setf clack-handler nil)
	  (setf is-running-p nil)
	  t))))

(defun load-app (path)
  (make-instance 'brac-appstate :root-path path))

(defun show-running ()
  "Prints a list of running web projects."
  (let ((namelist (mapcar #'name *project-instances*)))
    (format t "~{~A~%~}" namelist)
    ;; TODO: uptime
    namelist))

(defmethod state-report ((state brac-appstate))
  ;;TODO
  nil)

