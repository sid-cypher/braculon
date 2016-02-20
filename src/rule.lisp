(in-package :braculon)
(use-package :annot.class)
(annot:enable-annot-syntax)

@export-class
(defclass brac-rule ()
  ((name :reader name
	 :initarg :name
	 :initform (error "Rule object needs a name.")
         :type string
         :documentation "")
   (callable :reader callable
	     :initarg :callable
	     :initform (error "Rule object needs its callable part.")
	     :documentation "")
   (source-file :reader source-file
		:initarg :source-file
		:documentation "")
   (load-time :reader load-time
	      :initform (local-time:now)
	      :documentation "")))

(defmethod print-object ((rule brac-rule) stream)
  (print-unreadable-object (rule stream :type t)
    (format stream "~A" (name rule))))

@export
(defgeneric get-rule (appstate rule-name)
  (:method ((appstate brac-appstate) rule-name)
    ""
    (declare (type (or string symbol) rule-name))
    (gethash (name-to-downcase-string rule-name) (rules appstate)))
  (:documentation ""))

;;TODO add hooks into rule slot writer
(defgeneric add-rule (appstate rule)
  (:method ((appstate brac-appstate) (rule brac-rule))
    ""
    (setf (gethash (name rule) (rules appstate)) rule))
  (:documentation ""))

;;TODO add hooks
(defgeneric del-rule (appstate rule-name)
  (:method ((appstate brac-appstate) rule-name)
    ""
    (declare (type (or symbol string) rule-name))
    (with-slots (rules) appstate
      (remhash (name-to-downcase-string rule-name) rules)))
  (:documentation ""))

(defmacro defrule (name reqstate-symbol appstate lambda-list &body body)
  (declare (type (or symbol string) name)
           (type symbol reqstate-symbol)
	   (type list lambda-list))
  `(add-rule ,appstate
             (make-instance 'brac-rule
                            :name ',(name-to-downcase-string name)
                            :callable (lambda ,(cons reqstate-symbol lambda-list)
                                        (let ((*current-rs* ,reqstate-symbol))
                                          ,@body))
                            :source-file (load-time-value (or #.*compile-file-pathname* *load-pathname*)))))

(defgeneric load-builtin-rules (appstate)
  (:method ((appstate brac-appstate))
    ;;TODO with regex option
    (defrule path reqstate appstate (path ctrl-name &key (trailing-slash-option t))
      (declare (type string path)
               (type symbol ctrl-name))
      (when (if trailing-slash-option
		(string-and-slash= path (gethash :path-info (request reqstate)))
		(string= path (gethash :path-info (request reqstate))))
	(pack-routing-data reqstate
                           (get-rule appstate 'braculon::fixed)
                           (or (get-controller appstate ctrl-name)
                               (error "No controller named ~A was found" ctrl-name))
                           nil)))

    (defrule test reqstate appstate ()
      (format t "Test rule reporting.~%appstate: ~W~%reqstate: ~W~%" appstate reqstate)
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
	     (setf (rule-data req) nil)
	     (loop for file in static-files
                   while (not matchp) do
                     ;; trailing slash :deny \(later :allow, :require)
                     (when (string= (url-req-path-name req)
                                    (cat prefix (file-namestring file)))
                       (setf (rule-data req) (list :file file
                                                   :data data))
                       (setf matchp t)))
	     (when matchp
	       "file-contents")))

    #+nil(defrule braculon::redirect (reqstate) appstate
             nil)
    #+nil(defrule braculon::masquerade (reqstate) appstate
             nil)
    t)
  (:documentation ""))
