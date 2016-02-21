(in-package :braculon)
(use-package :annot.class)
(annot:enable-annot-syntax)

@export-class
(defclass brac-chain ()
  ((name :reader name
	 :initarg :name
	 :initform (error "Chain object needs a name symbol.")
         :type string
         :documentation "")
   (configuration :accessor configuration
                  :initarg :configuration
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
