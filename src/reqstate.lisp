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
	    :initarg :request)
   (response-status-code :accessor response-status-code
			 :initarg :status-code
			 :initform 200
			 :type fixnum)
   (response-headers :accessor response-headers
		     :initform (make-hash-table :test 'eq)
		     :type hash-table)
   (response-content :accessor response-content
		     :initarg :response)))

(defmethod print-object ((req brac-reqstate) stream)
  (print-unreadable-object (req stream :type t)
    (with-slots (router controller root-view) req
      (format stream "req:~A" (request req)))))

(defun format-request (env &key (format-control "~A: ~W~%") no-headers)
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
      (loop-ht (request env)))))

(defun hash-original-request (env)
  (alexandria:plist-hash-table env :test 'eq :size 22))
