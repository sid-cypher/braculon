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
	(with-open-file (stream truepath) ;;TODO handle exceptions
	  (handler-case (read stream :eof-value nil)
	    (error () nil))))))

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
