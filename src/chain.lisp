(in-package :braculon)
(use-package :annot.class)
(annot:enable-annot-syntax)

@export-class
(defclass brac-chain ()
  ((name :reader name
	 :initarg :name
	 :initform (error "Chain object needs a name.")
         :type string
         :documentation "")
   (rules :accessor rules
          :initarg :rules
          :documentation "")
   (final-action :accessor final-action
                 :initarg :final-action
                 :type brac-action
                 :documentation "")
   (source-file :reader source-file
		:initarg :source-file
		:documentation "")
   (load-time :reader load-time
	      :initform (local-time:now)
	      :documentation "")))

(defmethod print-object ((chain brac-chain) stream)
  (print-unreadable-object (chain stream :type t)
    (format stream "~A" (name chain))))

;; TODO log failures
@export
(defgeneric get-chain (appstate chain-name)
  (:method ((appstate brac-appstate) chain-name)
    ""
    (declare (type (or string symbol) chain-name))
    (gethash (name-to-downcase-string chain-name) (chains appstate)))
  (:documentation ""))

;; TODO hooks, maybe log, no-overwrite option
(defgeneric add-chain (appstate chain)
  (:method ((appstate brac-appstate) (chain brac-chain))
    ""
    (setf (gethash (name chain) (chains appstate)) chain))
  (:documentation ""))

;;TODO add hooks
(defgeneric del-chain (appstate chain-name)
  (:method ((appstate brac-appstate) chain-name)
    ""
    (declare (type (or symbol string) chain-name))
    (with-slots (chains) appstate
      (remhash (name-to-downcase-string chain-name) chains)))
  (:documentation ""))

(defun chain-pass (rs &optional chain-name)
  (unless chain-name
    (setf chain-name (starting-chain (appstate rs))))
  (let ((max-hops (max-chain-hops (appstate rs)))
        (chain (get-chain (appstate rs) chain-name))
        skip-rule permission-to-send)
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

(defmacro defchain (name appstate final-action &body body)
  (declare (type (or symbol string) name final-action))
  ;;TODO: walk body of rules and ensure it is well-defined
  `(add-chain ,appstate
              (make-instance 'brac-chain
                             :name ,(name-to-downcase-string name)
                             :final-action ,(name-to-downcase-string final-action)
                             :source-file (load-time-value (or #.*compile-file-pathname* *load-pathname*))
                             :rules ',body)))

(defgeneric load-builtin-chains (appstate)
  (:method ((appstate brac-appstate))
    (defchain brac-init appstate send-404
      ((path "/") send-hello)
      ((path "/drop") drop)
      ((path "/test") send-test))
    t)
  (:documentation ""))
