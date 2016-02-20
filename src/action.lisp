(in-package :braculon)
(use-package :annot.class)
(annot:enable-annot-syntax)

@export-class
(defclass brac-action ()
  ((name :reader name
	 :initarg :name
	 :initform (error "Action object needs a name symbol.")
         :type string
         :documentation "")
   (callable :reader callable
	     :initarg :callable
	     :initform (error "Action object needs it callable part.")
	     :documentation "")
   (source-file :reader source-file
		:initarg :source-file
		:documentation "")
   (load-time :reader load-time
	      :initform (local-time:now)
	      :documentation "")))

(defmethod print-object ((action brac-action) stream)
  (print-unreadable-object (action stream :type t)
    (format stream "~A" (name action))))

;; TODO log failures
@export
(defgeneric get-action (appstate action-name)
  (:method ((appstate brac-appstate) action-name)
    ""
    (declare (type (or string symbol) action-name))
    (gethash (name-to-downcase-string action-name) (actions appstate)))
  (:documentation ""))

;; TODO hooks, maybe log, no-overwrite option
(defgeneric add-action (appstate action)
  (:method ((appstate brac-appstate) (action brac-action))
    ""
    (setf (gethash (name action) (actions appstate)) action))
  (:documentation ""))

;;TODO add hooks
(defgeneric del-action (appstate action-name)
  (:method ((appstate brac-appstate) action-name)
    ""
    (declare (type (or symbol string) action-name))
    (with-slots (actions) appstate
      (remhash (name-to-downcase-string action-name) actions)))
  (:documentation ""))

(defmacro defaction (name reqstate-symbol appstate lambda-list &body body)
  (declare (type (or symbol string) name)
           (type symbol reqstate-symbol)
	   (type list lambda-list))
  `(add-action ,appstate
               (make-instance 'brac-action
                              :name ',(name-to-downcase-string name)
                              :callable (lambda ,(cons reqstate-symbol lambda-list)
                                          (let ((*current-rs* ,reqstate-symbol))
                                            ,@body
                                            ,reqstate-symbol))
                              :source-file (load-time-value (or #.*compile-file-pathname* *load-pathname*)))))

;; TODO return new rs only.
(defgeneric load-builtin-actions (appstate)
  (:method ((appstate brac-appstate))
    (defaction test rs appstate ()
      ;;A tiny built-in action for testing purposes.
      (setf (status-code rs) 200)
      (setf (res-hdr :content-type) "text/plain; charset=UTF-8")
      (setf (response-content rs)
	    (format nil "Test action reporting.~%appstate: ~W~%rs: ~W~%~A~%"
		    appstate rs (format-request rs))))

    (defaction hello rs appstate ()
      ;;Outputs a short greetings page.
      (setf (status-code rs) 200)
      (setf (res-hdr :content-type) "text/html; charset=UTF-8")
      (setf (response-content rs)
	    (list (cl-who:with-html-output-to-string (s nil :prologue t :indent t)
		    (:html (:head (:title "braculon:hello"))
			   (:body (:p "Hello! Things seem to work here.")))))))

    (defaction file rs appstate (pathname)
      (setf (response-content rs)
            (lack.app.file:make-app
             :file (getf (routing-data rs) :filename)
             :root (or pathname
                       (uiop:merge-pathnames* #p"static/" ;;TODO no magic
                                              (root-path appstate))))))
    t)
  (:documentation ""))
