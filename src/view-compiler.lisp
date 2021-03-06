(in-package :braculon)
(use-package :annot.class)
(annot:enable-annot-syntax)

@export-class
(defclass view-compiler ()
  ((name :reader name
	 :initarg :name
	 :initform (error "View compiler object needs a name.")
         :type string
	 :documentation "")
   (callable :reader callable
	     :initarg :callable
	     :initform (error "View compiler object needs it callable part.")
	     :documentation "")
   (source-file :reader source-file
		:initarg :source-file
		:documentation "")
   (load-time :reader load-time
	      :initform (local-time:now)
	      :documentation "")))

;; TODO: lack.util::find-package-or-load might be useful for deps later

;; TODO: load views recursively to build a full cl-who form, insert vars
