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

    (defcondition* file rqs app (root-folder-path)
      ;;TODO explode path into folders and file
      ;; traverse fs with a goal instead of building a list
      (let* ((rfp (merge-pathnames root-folder-path
                                   (asdf:system-source-directory (app-package app))))
             ;;TODO would absolutely move this outside defscope
             ;; provide means to define subclassed conditions.
             (static-files (uiop:directory-files
                            (merge-pathnames uiop:*wild-inferiors* rfp)))
             (url-pathname (merge-pathnames
                            (strip-leading-slashes (rq :path-info)) rfp))
             matchp)
        (loop for file in static-files
              while (not matchp) do
                (setf matchp (pathname-match-p url-pathname file))
                (when matchp
                  (setf (rq-data :file-match rqs) file)))
        matchp))

    #+nil(defcondition redirect rqs ()
           nil)
    #+nil(defcondition masquerade rqs ()
           nil)
    t)
  (:documentation ""))

(defun condition-check (rs condition-spec)
  (flet ((call-if-found ()
           (let ((cnd (get-condition condition-spec (app rs))))
             (if cnd
                 (funcall (callable cnd) rs)
                 (error "Condition ~W not found." condition-spec)))))
    (etypecase condition-spec
      (string
       (call-if-found))
      (symbol
       (call-if-found))
      (cons
       (let ((cnd (get-condition (first condition-spec) (app rs)))
             (args (rest condition-spec)))
         (if cnd
             (apply (callable cnd) rs args)
             (error "Condition ~W not found." (first condition-spec))))))))
