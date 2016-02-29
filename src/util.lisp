(in-package :braculon)

(defmacro cat (&body bod)
  "because \(concatenate 'string ...) is too much to type"
  `(concatenate 'string ,@bod))

(defmacro mcat (&body bod)
  "macroexpand-time cat for breaking up large texts into manageable pieces"
  (apply #'concatenate 'string bod))

(defmacro silence (&body bod)
  "mutes the stdout unless overridden"
  `(let ((*standard-output* (make-broadcast-stream)))
     ;;TODO: scan for *std-out* redefs in body.
     ,@bod))

(defun name-to-downcase-string (namesym)
  (declare (type (or string symbol) namesym))
  (etypecase namesym
    (string namesym)
    (symbol (string-downcase (symbol-name namesym)))))

(defun name-to-keyword (namesym)
  (declare (type (or string symbol) namesym))
  (intern (etypecase namesym
            (string (string-upcase namesym))
            (symbol (symbol-name namesym)))
          :keyword))

(defun string-and-slash= (argpath matchpath)
  "TODO"
  (declare (type string argpath matchpath))
  (let (aps mps (aplen (length argpath)) (mplen (length matchpath)))
    (when (and (< 1 aplen)
	       (string= "/" (subseq argpath (1- aplen) aplen)))
      (setf aps t))
    (when (and (< 1 mplen)
	       (string= "/" (subseq matchpath (1- mplen) mplen)))
      (setf mps t))
    (string= (if aps
		 (subseq argpath 0 (1- aplen))
		 argpath)
	     (if mps
		 (subseq matchpath 0 (1- mplen))
		 matchpath))))

(defun strip-leading-slashes (pathstr)
  (declare (type string pathstr))
  (let (char)
    (dotimes (i (length pathstr))
      (setf char (aref pathstr i))
      (when (not (or (char= #\/ char)
                     (char= #\\ char)))
        (return (subseq pathstr i))))))

;;TODO: use local-time
(defmacro with-decoded-timestamp
    (timestamp (&key sec min h day mon year dweek dst-p tz) &body body)
  (let ((time-unit-symbols
          (list
           (or sec 'sec) (or min 'min)
           (or h 'h) (or day 'day)
           (or mon 'mon) (or year 'year)
           (or dweek 'dweek) (or dst-p 'dst-p)
           (or tz 'tz))))
    `(multiple-value-bind ,time-unit-symbols (decode-universal-time ,timestamp)
       (declare (ignorable ,@time-unit-symbols))
       ,@body)))

;;TODO: use local-time
(defun date-from-timestamp (timestamp)
  (with-decoded-timestamp timestamp ()
    (format nil "~A ~A, ~A" (aref local-time:+month-names+ mon) (count-en-num day) year)))

(defun count-en-num (n)
  (declare (type fixnum n))
  (format nil "~A~[th~;st~;nd~;rd~:;th~]"
	  n (if (= 1 (mod (floor n 10) 10)) ;; second digit, teens
		0
		(mod n 10))))

(defun seconds-to-dhms (timestamp)
  (let ((seconds (local-time:timestamp-difference
                  (local-time:now) timestamp))
	minutes hours days)
    (multiple-value-bind (d s) (truncate seconds local-time:+seconds-per-day+)
      (setf days d
	    seconds s))
    (multiple-value-bind (h s) (truncate seconds local-time:+seconds-per-hour+)
      (setf hours h
	    seconds s))
    (multiple-value-bind (m s) (truncate seconds local-time:+seconds-per-minute+)
      (setf minutes m
	    seconds s))
    (values days hours minutes seconds)))

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

(defun read-first-form-file (filepath &optional package)
  (declare (type pathname filepath))
  (let ((truepath (uiop:file-exists-p filepath)))
    (unless truepath
      (error "Tried reading ~W, but it doesn't exist" filepath))
    ;;TODO push a message through event logging system, log errors
    (uiop:with-safe-io-syntax (:package (or package :brac))
      (with-open-file (stream truepath)
	(read stream :eof-value nil)))))

(defun read-single-form-file (filepath &optional package)
  (declare (type pathname filepath))
  (let ((truepath (uiop:file-exists-p filepath)))
    ;;TODO push a message through event logging system, log errors
    (uiop:with-safe-io-syntax (:package (or package :brac))
      (with-open-file (stream truepath)
	(let ((first-form (ignore-errors (read stream))))
	  (unless (ignore-errors (read stream))
	    first-form))))))

(defun read-multiple-forms-file (filepath &optional package)
  (declare (type pathname filepath))
  (let ((truepath (uiop:file-exists-p filepath)))
    (ignore-errors ;;TODO push a message through event logging system
     (uiop:with-safe-io-syntax (:package (or package :brac))
       (with-open-file (stream truepath)
         (handler-bind ((end-of-file (lambda (err)
                                       (declare (ignorable err))
                                       (invoke-restart 'finish-read-loop))))
           (loop named :multiform-read-loop
                 as read-form = (restart-case (read stream)
                                  (finish-read-loop () (return-from :multiform-read-loop read-forms)))
                 collect read-form into read-forms)))))))

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

;;TODO: a lightweight cons-based queue (save tail element), breadth-first traversal.
