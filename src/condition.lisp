(in-package :braculon)
(use-package :annot.class)
(annot:enable-annot-syntax)

@export-class
(defclass rule-condition ()
  ((name :reader name
	 :initarg :name
	 :initform (error "Condition object needs a name.")
         :type string
         :documentation "")
   (callable :reader callable
	     :initarg :callable
	     :initform (error "Condition object needs its callable part.")
	     :documentation "")
   (source-file :reader source-file
		:initarg :source-file
		:documentation "")
   (load-time :reader load-time
	      :initform (local-time:now)
	      :documentation "")))

(defmethod print-object ((condition rule-condition) stream)
  (print-unreadable-object (condition stream :type t)
    (format stream "~A" (name condition))))

;; TODO hooks, log failures and warnings, no-overwrite option
@export
(defun get-condition (condition-name &optional app)
  (declare (type (or string symbol) condition-name))
  (gethash (name-to-downcase-string condition-name) (conditions (find-app app))))

(defun add-condition (condition &optional app)
  (declare (type condition rule-condition))
  (setf (gethash (name condition) (conditions (find-app app))) condition))

@export
(defun del-condition (condition-name &optional app)
  (declare (type (or string symbol) condition-name))
  (remhash (name-to-downcase-string condition-name) (conditions (find-app app))))

@export
(defmacro defcondition (name rps-sym lambda-list &body body)
  (declare (type (or symbol string) name)
           (type symbol rps-sym)
	   (type list lambda-list))
  `(add-condition (make-rule-condition ,name ,rps-sym ,lambda-list
                    ,@body)))

@export
(defmacro defcondition* (name rps-sym app lambda-list &body body)
  (declare (type (or symbol string) name)
           (type symbol rps-sym)
	   (type list lambda-list))
  `(add-condition (make-rule-condition ,name ,rps-sym ,lambda-list
                    ,@body)
                  ,app))

@export
(defmacro make-rule-condition (name rps-sym lambda-list &body body)
  `(make-instance 'rule-condition
                  :name ,(name-to-downcase-string name)
                  :callable (lambda ,(cons rps-sym lambda-list)
                              (let ((brac::*current-rs* ,rps-sym))
                                ,@body))
                  :source-file (load-time-value (or #.*compile-file-pathname* *load-pathname*))))

(defgeneric load-builtin-conditions (app)
  (:method ((app brac-app))
    ;;TODO with regex option
    (defcondition* path rqs app (path &key (trailing-slash-option t))
      (declare (type string path))
      (if trailing-slash-option
          (string-and-slash= path (gethash :path-info (request rqs)))
          (string= path (gethash :path-info (request rqs)))))

    ;; ===old===
    #+nil(destructuring-bind (&key folder url-prefix data) options
	   (unless (stringp url-prefix)
	     ;; TODO: log config error - url-prefix not a string
	     (setf url-prefix nil))
	   (unless (pathnamep folder) ;; TODO as well
	     (setf folder nil))
	   (let ((static-files (uiop:directory-files
				(if folder
				    (merge-pathnames folder (static-content-path app))
				    (static-content-path app))))
		 (prefix (or url-prefix "/"))
		 matchp)
	     (setf (condition-data req) nil)
	     (loop for file in static-files
                   while (not matchp) do
                     ;; trailing slash :deny \(later :allow, :require)
                     (when (string= (url-req-path-name req)
                                    (cat prefix (file-namestring file)))
                       (setf (condition-data req) (list :file file
                                                        :data data))
                       (setf matchp t)))
	     (when matchp
	       "file-contents")))

    #+nil(defcondition braculon::redirect (rqs) app
           nil)
    #+nil(defcondition braculon::masquerade (rqs) app
           nil)
    t)
  (:documentation ""))

(defun condition-check (rs condition-spec)
  (flet ((call-if-found ()
           (let ((cnd (get-condition (app rs) condition-spec)))
             (if cnd
                 (funcall (callable cnd) rs)
                 (error "Condition ~W not found." condition-spec)))))
    (etypecase condition-spec
      (string
       (call-if-found))
      (symbol
       (call-if-found))
      (cons
       (let ((cnd (get-condition (app rs) (first condition-spec)))
             (args (last condition-spec)))
         (if cnd
             (apply (callable cnd) rs args)
             (error "Condition ~W not found." (first condition-spec))))))))
