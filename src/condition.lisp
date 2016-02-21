(in-package :braculon)
(use-package :annot.class)
(annot:enable-annot-syntax)

@export-class
(defclass brac-condition ()
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

(defmethod print-object ((condition brac-condition) stream)
  (print-unreadable-object (condition stream :type t)
    (format stream "~A" (name condition))))

@export
(defgeneric get-condition (appstate condition-name)
  (:method ((appstate brac-appstate) condition-name)
    ""
    (declare (type (or string symbol) condition-name))
    (gethash (name-to-downcase-string condition-name) (conditions appstate)))
  (:documentation ""))

;;TODO add hooks into condition slot writer
(defgeneric add-condition (appstate condition)
  (:method ((appstate brac-appstate) (condition brac-condition))
    ""
    (setf (gethash (name condition) (conditions appstate)) condition))
  (:documentation ""))

;;TODO add hooks
(defgeneric del-condition (appstate condition-name)
  (:method ((appstate brac-appstate) condition-name)
    ""
    (declare (type (or symbol string) condition-name))
    (with-slots (conditions) appstate
      (remhash (name-to-downcase-string condition-name) conditions)))
  (:documentation ""))

(defmacro defcondition (name reqstate-symbol appstate lambda-list &body body)
  (declare (type (or symbol string) name)
           (type symbol reqstate-symbol)
	   (type list lambda-list))
  `(add-condition ,appstate
                  (make-instance 'brac-condition
                                 :name ',(name-to-downcase-string name)
                                 :callable (lambda ,(cons reqstate-symbol lambda-list)
                                             (let ((brac::*current-rs* ,reqstate-symbol))
                                               ,@body))
                                 :source-file (load-time-value (or #.*compile-file-pathname* *load-pathname*)))))

(defgeneric load-builtin-conditions (appstate)
  (:method ((appstate brac-appstate))
    ;;TODO with regex option
    (defcondition path reqstate appstate (path ctrl-name &key (trailing-slash-option t))
      (declare (type string path)
               (type symbol ctrl-name))
      (when (if trailing-slash-option
		(string-and-slash= path (gethash :path-info (request reqstate)))
		(string= path (gethash :path-info (request reqstate))))
	(pack-routing-data reqstate
                           (get-condition appstate 'braculon::fixed)
                           (or (get-controller appstate ctrl-name)
                               (error "No controller named ~A was found" ctrl-name))
                           nil)))

    (defcondition test reqstate appstate ()
      (format t "Test condition reporting.~%appstate: ~W~%reqstate: ~W~%" appstate reqstate)
      reqstate)
    ;; ===old===
    #+nil(destructuring-bind (&key folder url-prefix data) options
	   (unless (stringp url-prefix)
	     ;; TODO: log config error - url-prefix not a string
	     (setf url-prefix nil))
	   (unless (pathnamep folder) ;; TODO as well
	     (setf folder nil))
	   (let ((static-files (uiop:directory-files
				(if folder
				    (merge-pathnames folder (static-content-path appstate))
				    (static-content-path appstate))))
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

    #+nil(defcondition braculon::redirect (reqstate) appstate
           nil)
    #+nil(defcondition braculon::masquerade (reqstate) appstate
           nil)
    t)
  (:documentation ""))
