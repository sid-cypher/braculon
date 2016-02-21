(in-package :braculon)
(use-package :annot.class)
(annot:enable-annot-syntax)

@export-class
(defclass brac-reqstate ()
  ((appstate :reader appstate
	     :initarg :appstate)
   (datastore :accessor datastore
              :initform (make-hash-table :test 'eq)
              :type hash-table)
   (original-request :reader original-request
		     :initarg :original-request)
   (request :accessor request
	    :initarg :request
            :type hash-table)
   (response-status-code :accessor response-status-code
			 :initarg :status-code
			 :initform 200
			 :type fixnum)
   (response-headers :accessor response-headers
		     :initform (make-hash-table :test 'eq)
		     :type hash-table)
   (response-content :accessor response-content
		     :initarg :response)))

(defmethod print-object ((rs brac-reqstate) stream)
  (print-unreadable-object (rs stream :type t)
    (with-slots (router controller root-view) rs
      (format stream "reqstate:~A" (request rs)))))

(defvar *current-rs* nil)

(defun rq (key &optional (rs brac::*current-rs*))
  (gethash (name-to-keyword key) (datastore rs)))

;;TODO: check if this is needed
(defun (setf rq) (key value &optional (rs brac::*current-rs*))
  (setf (gethash (name-to-keyword key) (datastore rs)) value))

(defun rq-clear (key &optional (rs brac::*current-rs*))
  (remhash (name-to-keyword key) (datastore rs)))

(defun rq-data (key &optional (rs brac::*current-rs*))
  (gethash (name-to-keyword key) (datastore rs)))

;;(defun (setf rq-data) (key value &optional (rs brac::*current-rs*))
;;(setf (gethash (name-to-keyword key) (datastore rs)) value))

(defun rq-data-clear (key &optional (rs brac::*current-rs*))
  (remhash (name-to-keyword key) (datastore rs)))

(defun res-hdr (key &optional (rs brac::*current-rs*))
  (princ (format-request rs))
  (gethash (name-to-downcase-string key) (response-headers rs)))

(defun format-request (rs &key (format-control "~A: ~W~%") no-headers)
  (with-output-to-string (stream)
    (labels
	((loop-ht (ht)
	   (loop for k being the hash-keys
		   in ht using (hash-value v)
		 do (if (or no-headers
			    (not (eq :headers k)))
			(format stream
				format-control k v)
			(progn
			  (write-line "-- BEGIN HEADERS --"
				      stream)
			  (loop-ht v)
			  (write-line "--- END HEADERS ---"
				      stream))))))
      (write-line "---- REQUEST ----" stream)
      (loop-ht (request rs))
      (write-line "--- DATASTORE ---" stream)
      (loop-ht (datastore rs))
      (write-line "-----------------" stream))))

(defun hash-original-request (rs)
  (alexandria:plist-hash-table rs :test 'eq :size 22))
