(in-package :braculon)
(use-package :annot.class)
(annot:enable-annot-syntax)

@export-class
(defclass brac-reqstate ()
  ((appstate :reader appstate
	     :initarg appstate)
   (router :accessor router
	   :initform nil)
   (controller :accessor controller
	       :initform nil)
   (root-view :accessor root-view
	      :initform nil)
   (routing-data :accessor routing-data
		 :initform nil)
   (view-fields :accessor view-fields)
   (original-request :reader original-request
		     :initarg original-request)
   (request :accessor request
	    :initarg request)
   (response :accessor response
	     :initarg response)))

(defun hash-original-request (env)
  (alexandria:plist-hash-table env :test 'eq :size 22))
