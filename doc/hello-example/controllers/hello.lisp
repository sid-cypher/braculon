;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10 -*-

(defcontroller hello-world (request)
  (declare (ignorable request))
  (values "Hello, world!"
	  '(:content-type "text/plain; charset=UTF-8"))) ;; TODO: need a view rendering function
