(in-package :braculon)
(use-package :annot.class)
(annot:enable-annot-syntax)

@export-class
(defclass brac-reqstate ()
  ((appstate :reader appstate
	     :initarg :appstate)
   (router :accessor router
	   :initform nil)
   (controller :accessor controller
	       :initform nil)
   (root-view :accessor root-view
	      :initform nil)
   (routing-data :accessor routing-data
		 :initform nil)
   (controller-data :accessor controller-data
		    :initform nil)
   (view-fields :accessor view-fields)
   (original-request :reader original-request
		     :initarg :original-request)
   (request :accessor request
	    :initarg :request)
   (response-status-code :accessor status-code
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
      (format stream "router:~A, ctrl:~A, view:~A"
	      (or (unless router "nil")
		  (symbol-name (name router)))
	      (or (unless controller "nil")
		  (symbol-name (name controller)))
	      (or (unless root-view "nil")
		  (symbol-name (name root-view)))))))

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
