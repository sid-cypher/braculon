(in-package :braculon)
(use-package :annot.class)
(annot:enable-annot-syntax)

@export-class
(defclass chain ()
  ((name :reader name
	 :initarg :name
	 :initform (error "Chain object needs a name.")
         :type string
         :documentation "")
   (rules :accessor rules
          :initarg :rules
          :documentation "")
   (chain-length :accessor chain-length
                 :initform 0
                 :initarg :length
                 :type number)
   (final-action :accessor final-action
                 :initarg :final-action
                 :type action
                 :documentation "")
   (source-file :reader source-file
		:initarg :source-file
		:documentation "")
   (load-time :reader load-time
	      :initform (local-time:now)
	      :documentation "")))

(defmethod print-object ((chain chain) stream)
  (print-unreadable-object (chain stream :type t)
    (format stream "~A" (name chain))))

;; TODO hooks, log failures and warnings, no-overwrite option
@export
(defun get-chain (&optional chain-name app)
  (declare (type (or string symbol null) chain-name))
  (setf app (or app (find-app)))
  (setf chain-name (or chain-name (starting-chain app)))
  (gethash (name-to-downcase-string chain-name) (chains (find-app app))))

(defun add-chain (chain &optional app)
  (declare (type chain chain))
  (setf (gethash (name chain) (chains (find-app app))) chain))

@export
(defun del-chain (chain-name &optional app)
  (declare (type (or string symbol) chain-name))
  (remhash (name-to-downcase-string chain-name) (chains (find-app app))))

(defun chain-pass (rs &optional chain-name)
  ;; TODO: store at least url path, maybe Rete alg eventually?
  (unless chain-name
    (setf chain-name (starting-chain (app rs))))
  (let ((max-hops (max-chain-hops (app rs)))
        (chain (get-chain chain-name (app rs)))
        skip-rule permission-to-send)
    (unless chain
      (error "Chain ~W not found." chain-name))
    (block chainloop
      (dolist (rule (rules chain))
        ;;(format t "---~%rule:~W~%car: ~W~%cdr: ~W~%---~%" rule (car rule) (cdr rule))
        (when (and (not skip-rule)
                   (condition-check rs (car rule)))
          (multiple-value-bind (action-finish jump-chain) (perform rs (cdr rule))
            (ecase action-finish
              (:send
               (setf permission-to-send t)
               (return-from chainloop))
              (:pass nil)
              (:jump
               (if (< (chain-hops rs) max-hops)
                   (progn
                     (incf (chain-hops rs))
                     (setf permission-to-send
                           (chain-pass rs jump-chain)))
                   (error "too many chain hops")))
              (:skip (setf skip-rule t))
              (:drop (return-from chainloop)))))
        (when skip-rule
          (setf skip-rule nil)))
      (multiple-value-bind
            (action-finish jump-chain) (perform rs (final-action chain))
        (ecase action-finish
          (:send (setf permission-to-send t))
          (:jump
           (setf permission-to-send
                 (chain-pass rs jump-chain)))
          (:drop (setf permission-to-send nil)))))
    permission-to-send))

@export
(defmacro defchain (name final-action &body body)
  (declare (type (or symbol string) name final-action))
  ;;TODO: walk body of rules and ensure it is well-defined
  `(add-chain (make-chain ,name ,final-action
                          ,@body)))

@export
(defmacro defchain* (name app final-action &body body)
  (declare (type (or symbol string) name final-action))
  ;;TODO: walk body of rules and ensure it is well-defined
  `(add-chain (make-chain ,name ,final-action
                ,@body)
              ,app))

@export
(defmacro make-chain (name final-action &body body)
  `(let ((rules ',body))
     (make-instance 'chain
                    :name ,(name-to-downcase-string name)
                    :final-action ,(name-to-downcase-string final-action)
                    :source-file (load-time-value (or #.*compile-file-pathname* *load-pathname*))
                    :rules rules
                    :length (length rules))))

(defgeneric load-builtin-chains (app)
  (:method ((app brac-app))
    (defchain* init app send-404
      ((path "/") send-hello)
      ((path "/drop") drop)
      ((path "/test") send-test))
    t)
  (:documentation ""))

@export
(defun list-chains (&optional app)
  (alexandria:hash-table-keys (chains (find-app app))))

@export
(defun chain-flush (&optional chain-name app)
  (let ((chain (get-chain (or chain-name
                              (starting-chain (find-app app)))
                          app)))
    (setf (rules chain) nil)
    (setf (chain-length chain) 0)))

@export
(defun chain-append-rule (rule &optional chain-name app)
  (let ((chain (get-chain (or chain-name
                              (starting-chain (find-app app)))
                          app)))
    (setf (rules chain) (append (rules chain)
                                (list rule)))
    (incf (chain-length chain))))

@export
(defun chain-ensure-rule (rule &key ins-index chain-name app)
  ;;TODO: append/insert rule to chain if not present
  (let ((chain (get-chain (or chain-name
                              (starting-chain (find-app app)))
                          app)))
    nil))
