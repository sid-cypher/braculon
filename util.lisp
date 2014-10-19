;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10 -*-

(in-package :braculon)

(defmacro cat (&body bod)
  "because 'concatenate strings' is too much to type"
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
	(with-open-file (stream truepath)
	  (read stream)))))
