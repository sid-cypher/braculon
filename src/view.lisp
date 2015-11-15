;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10 -*-

(in-package :braculon)

(defclass brac-view ()
  ((appstate :reader appstate
	     :initarg :parent
	     :initform (error "View object needs a parent appstate.")
	     :documentation "")
   (name :reader name
	 :initarg :name
	 :documentation "")
   (fields :reader fields
	   :initarg :fields
	   :initform '()
	   :documentation "A list of field name keywords.")
   (dependencies :reader dependencies
		 :initarg :dependencies
		 :initform '()
		 :documentation "A list of direct dependencies of this view, name strings.")
   (renderable :reader renderable
	       :initarg :renderable
	       :documentation "")
   (source-file :reader source-file
		:initarg :source-file
		:documentation "")
   (load-time :reader load-time
	      :initform (get-universal-time)
	      :documentation "")))

(defmethod print-object ((view brac-view) stream)
  (print-unreadable-object (view stream :type t)
    (format stream "~A" (name view))))

;; TODO write, with hooks later
(defgeneric add-view (state view)
  (:method ((state brac-appstate) (view brac-view))
    "" ;; TODO
    nil)
  (:documentation ""))

;; TODO hooks
(defgeneric del-view (state view-name)
  (:method ((state brac-appstate) view-name)
    (declare (type string view-name))
    (with-slots (views) state
      (remhash view-name views)))
  (:documentation ""))

;; TODO review file format, rewrite
(defgeneric load-view-files (state)
  (:method ((state brac-appstate))
    (let ((view-src-files (uiop:directory-files (views-path state))))
      (dolist (filename view-src-files)
	(let ((source-file-forms (read-multiple-forms-file filename)))
	  (dolist (src-form source-file-forms)
	    (let* ((fcall-symbol (when (consp src-form)
				   (pop src-form)))
		   (view-name (when (consp src-form)
				(pop src-form)))
		   (view-body (when (consp src-form)
				src-form))
		   view-renderable) ;; TODO report errors
	      (when (and (symbolp fcall-symbol)
			 (string= (symbol-name fcall-symbol) "DEFVIEW")
			 (or (stringp view-name)
			     (symbolp view-name))
			 view-body)
		(setf view-name (safe-name-symbol-to-string view-name))
		(setf view-renderable view-body))
	      (when view-renderable ;; TODO log this addition
		(add-view state (make-instance 'brac-view
					       :parent state
					       :name view-name
					       :renderable view-renderable
					       :source-file filename)))))))))
  (:documentation ""))

;; TODO: move to view-compilers, lack.util::find-package-or-load might be useful for deps.
(defun render-who (view-name view-data &key if-var-unused if-var-missing cache depth-limit)
  (setf (html-mode) :html5)
  (let (view
	cacheablep)
    ;; TODO: load views recursively to build a full cl-who form, insert vars
    ;;
    ;; (eval `(let (,stuff)
    ;;   (with-html-output-to-string (*standard-output* nil :prologue nil :indent t)

    ))
