;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10 -*-

(in-package :braculon)

(defclass bracceptor (hunchentoot:acceptor)
  ())

(defmethod process-request (request)
  ;; TODO
)

(defmethod handle-request ((*acceptor* bracceptor) (*request* request))
  ;; TODO
)

(defmethod acceptor-dispatch-request ((acceptor bracceptor) request)
  ;; TODO
)
