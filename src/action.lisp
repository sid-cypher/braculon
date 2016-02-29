(in-package :braculon)
(use-package :annot.class)
(annot:enable-annot-syntax)

@export-class
(defclass action ()
  ((name :reader name
	 :initarg :name
	 :initform (error "Action object needs a name.")
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

(defmethod print-object ((action action) stream)
  (print-unreadable-object (action stream :type t)
    (format stream "~A" (name action))))

;; TODO hooks, log failures and warnings, no-overwrite option
@export
(defun get-action (action-name &optional app)
  (declare (type (or string symbol) action-name))
  (gethash (name-to-downcase-string action-name) (actions (find-app app))))

(defun add-action (action &optional app)
  (declare (type action action))
  (setf (gethash (name action) (actions (find-app app))) action))

@export
(defun del-action (action-name &optional app)
  (declare (type (or string symbol) action-name))
  (remhash (name-to-downcase-string action-name) (actions (find-app app))))

(defvar *action-finish* nil)
(defvar *jump-chain* nil)

@export
(defmacro defaction (name rps lambda-list &body body)
  `(add-action (make-action ,name ,rps ,lambda-list
                 ,@body)))
@export
(defmacro defaction* (name rps app lambda-list &body body)
  `(add-action (make-action ,name ,rps ,lambda-list
                 ,@body)
               ,app))

@export
(defun finish (act-fin-type &optional chain-spec)
  (declare (type (member :send :pass :jump :skip :drop) act-fin-type)
           (ignore act-fin-type chain-spec))
  (error "The function FINISH should be used in action definition scope only."))

@export
(defmacro make-action (name rps-sym lambda-list &body body)
  (declare (type (or symbol string) name)
           (type symbol rps-sym)
	   (type list lambda-list))
  (let ((srcfile (load-time-value
                  (or #.*compile-file-pathname* *load-pathname*)))
        (action-block-name (gensym "ACTIONBLOCK")))
    `(make-instance
      'action
      :name ',(name-to-downcase-string name)
      :source-file ,srcfile
      :callable
      (lambda ,(cons rps-sym lambda-list)
        (let ((*current-rs* ,rps-sym)
              (*action-finish* :pass)
              *jump-chain*)
          (block ,action-block-name
            (flet ((finish (act-fin-type &optional chain-spec)
                     (declare (type (member :send :pass :jump :skip :drop)
                                    act-fin-type))
                     (when chain-spec
                       (setf *jump-chain* chain-spec))
                     (setf *action-finish* act-fin-type))
                   (send-now ()
                     (setf *action-finish* :send)
                     (return-from ,action-block-name))
                   (pass-now ()
                     (setf *action-finish* :pass)
                     (return-from ,action-block-name))
                   (jump-now (chain-spec)
                     (setf *action-finish* :jump)
                     (setf *jump-chain* chain-spec)
                     (return-from ,action-block-name))
                   (skip-now ()
                     (setf *action-finish* :skip)
                     (return-from ,action-block-name))
                   (drop-now ()
                     (setf *action-finish* :drop)
                     (return-from ,action-block-name)))
              (declare (ignorable #'send-now #'pass-now #'jump-now
                                  #'skip-now #'drop-now #'finish))
              ,@body)
            (values *action-finish* *jump-chain*)))))))

(defgeneric load-builtin-actions (app)
  (:method ((app brac-app))
    (defaction* nop rs app ()
      nil)

    (defaction* send-test rs app ()
      ;;A tiny built-in action for testing purposes.
      (finish :send)
      (setf (res-status-code rs) 200)
      (setf (res-hdr :content-type) "text/plain; charset=UTF-8")
      (setf (res-content rs)
	    (format nil "Test action reporting.~%app: ~W~%request processing state: ~W~%~A~%"
                    app rs (format-request rs))))

    (defaction* send-hello rs app ()
      ;;Outputs a short greetings page.
      (finish :send)
      (setf (res-status-code rs) 200)
      (setf (res-hdr :content-type) "text/html; charset=UTF-8")
      (setf (res-content rs)
	    (list (cl-who:with-html-output-to-string (s nil :prologue t :indent t)
		    (:html (:head (:title "braculon:hello"))
			   (:body (:p "Hello! Things seem to work here.")))))))

    (defaction* send-404 rs app ()
      ;;Outputs a short greetings page.
      (finish :send)
      (setf (res-status-code rs) 400)
      (setf (res-hdr :content-type) "text/html; charset=UTF-8")
      (setf (res-content rs)
	    (list (cl-who:with-html-output-to-string (s nil :prologue t :indent t)
		    (:html (:head (:title "404 Not Found"))
			   (:body (:h2 "404 Not Found")
                                  (:p "Unknown URL.")))))))

    (defaction* send-file rs app (&optional pathname root)
      (finish :send)
      (setf (res-content rs)
            (lack.app.file:make-app
             :file (or pathname (rq-data :file-match rs))
             :root (or root #p"/tmp"))))

    (defaction* drop rs app ()
      (finish :drop))

    t)
  (:documentation ""))

(defun perform (rs action-spec)
  (flet ((call-if-found ()
           (let ((cnd (get-action action-spec (app rs))))
             (if cnd
                 (funcall (callable cnd) rs)
                 (error "Action ~W not found." action-spec)))))
    (etypecase action-spec
      (string
       (call-if-found))
      (symbol
       (call-if-found))
      (cons
       (let ((cnd (get-action (first action-spec) (app rs)))
             (args (rest action-spec)))
         (if cnd
             (apply (callable cnd) rs args)
             (error "Action ~W not found." (first action-spec))))))))
