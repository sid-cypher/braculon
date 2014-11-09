;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10 -*-

(in-package :braculon)

(defmacro cat (&body bod)
  "because \(concatenate 'strings ...) is too much to type"
  `(concatenate 'string ,@bod))

(defmacro silence (&body bod)
  "mutes the stdout unless overridden"
  `(let ((*standard-output* (make-broadcast-stream)))
     ;;TODO: scan for *std-out* redefs in body.
    ,@bod))

(defun read-form-file (filepath)
  (declare (type pathname filepath))
  (let ((truepath (cl-fad:file-exists-p filepath)))
    (if (not truepath) nil
	;;TODO push a message through event logging system
	(ignore-errors
	  (with-open-file (stream truepath)
	    (read stream :eof-value nil))))))

(defun read-multiple-forms-file (filepath)
  (declare (type pathname filepath))
  (let ((truepath (cl-fad:file-exists-p filepath)))
    (ignore-errors ;;TODO push a message through event logging system
	(with-open-file (stream truepath)
	  (handler-bind ((end-of-file (lambda (err)
					(declare (ignorable err))
					(invoke-restart 'finish-read-loop))))
	    (loop named :multiform-read-loop
	       as read-form = (restart-case (read stream)
				(finish-read-loop () (return-from :multiform-read-loop read-forms)))
	       collect read-form into read-forms))))))

(defun filter-single-pathname-type (filelist typestr)
  "Accepts a list of pathnames and filters it to return a list with pathnames
of specified pathname-type only (compared with STRING=)."
  (declare (type list filelist)
	   (type string typestr))
  (remove-if-not (lambda (filename)
		   (and (string= typestr (or (pathname-type filename) "."))
			(not (eql (aref (or (pathname-name filename) ".") 0)
				  #\.))))
		 filelist))
