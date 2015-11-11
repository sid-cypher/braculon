;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10 -*-

(in-package :braculon)

(define-constant +month-names+
    '("January" "February" "March" "April" "May" "June"
      "July" "August" "September" "October" "November" "December")
  :test #'equal)

(defmacro cat (&body bod)
  "because \(concatenate 'string ...) is too much to type"
  `(concatenate 'string ,@bod))

(defmacro silence (&body bod)
  "mutes the stdout unless overridden"
  `(let ((*standard-output* (make-broadcast-stream)))
     ;;TODO: scan for *std-out* redefs in body.
    ,@bod))

(defun safe-name-symbol-to-string (namesym)
  (declare (type (or string symbol) namesym))
  (cond ((symbolp namesym)
	 (when (or (keywordp namesym)
		   (not (constantp namesym)))
	   (string-downcase (symbol-name namesym))))
	((stringp namesym)
	 namesym)
	(t nil)))

(defun date-from-timestamp (timestamp)
  (multiple-value-bind (s m h d mo y dw) (decode-universal-time timestamp)
    (declare (ignore s m h dw))
    (format nil "~A ~A, ~A" (nth (1- mo) +month-names+) d y)))

(defun count-en-num (n)
  (declare (type fixnum n))
  (format nil "~A~[th~;st~;nd~;rd~:;th~]"
	  n (if (= 1 (mod (floor n 10) 10)) ;; second digit, teens
		0
		(mod n 10))))

(defun filesize (path)
  (with-open-file (f path)
    (file-length f)))

(defun pathname-modify (source-name arglist)
  (declare (type pathname source-name)
	   (type cons arglist))
  (destructuring-bind (&key directory name type) arglist
    (make-pathname :directory (or directory (pathname-directory source-name))
		   :name (or name (pathname-name source-name))
		   :type (or type (pathname-type source-name)))))

(defun read-form-file (filepath)
  (declare (type pathname filepath))
  (let ((truepath (uiop:file-exists-p filepath)))
    (if (not truepath) nil
	;;TODO push a message through event logging system
	(ignore-errors
	  (with-open-file (stream truepath)
	    (read stream :eof-value nil))))))

(defun read-multiple-forms-file (filepath)
  (declare (type pathname filepath))
  (let ((truepath (uiop:file-exists-p filepath)))
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

(defun read-file-to-string (filedesc &key max-len)
  (with-open-file (f filedesc)
    (let ((str (make-array (min (file-length f)
				max-len)
			   :element-type 'character
			   :fill-pointer t)))
      (setf (fill-pointer str) (read-sequence str f :end (when (> (file-length f) max-len) max-len)))
      str)))

;;TODO: search for DEFUNs and DEFMACROs in a functions folder and eval them
;;  in a package \(maybe with the same name as the project)
