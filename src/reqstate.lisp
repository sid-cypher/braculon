(in-package :braculon)
(use-package :annot.class)
(annot:enable-annot-syntax)

@export-class
(defclass request-processing-state ()
  ((app :reader app
        :initarg :app)
   (datastore :accessor datastore
              :initform (make-hash-table :test 'eq)
              :type hash-table)
   (original-request :reader original-request
		     :initarg :original-request)
   (request :accessor request
	    :initarg :request
            :type hash-table)
   (chain-hops :accessor chain-hops
               :initform 0
               :type fixnum)
   (response-status-code :accessor res-status-code
			 :initarg :status-code
			 :initform 200
			 :type fixnum)
   (response-headers :accessor res-headers
		     :initform (make-hash-table :test 'eq)
		     :type hash-table)
   (response-content :accessor res-content
		     :initarg :response)))

(defmethod print-object ((rs request-processing-state) stream)
  (print-unreadable-object (rs stream :type t)
    (format stream "status: ~A, hops: ~A" (res-status-code rs) (chain-hops rs))))

(defvar *current-rs* nil)

@export
(defun rq (key &optional (rs brac::*current-rs*))
  (gethash (name-to-keyword key) (request rs)))

@export
(defun (setf rq) (value key &optional (rs brac::*current-rs*))
  (setf (gethash (name-to-keyword key) (request rs)) value))

@export
(defun rq-data (key &optional (rs brac::*current-rs*))
  (gethash (name-to-keyword key) (datastore rs)))

@export
(defun (setf rq-data) (value key &optional (rs brac::*current-rs*))
  (setf (gethash (name-to-keyword key) (datastore rs)) value))

@export
(defun rq-data-clear (key &optional (rs brac::*current-rs*))
  (remhash (name-to-keyword key) (datastore rs)))

@export
(defun res-hdr (key &optional (rs brac::*current-rs*))
  (gethash (name-to-downcase-string key) (res-headers rs)))

@export
(defun (setf res-hdr) (value key &optional (rs brac::*current-rs*))
  (setf (gethash (name-to-downcase-string key) (res-headers rs)) value))

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
