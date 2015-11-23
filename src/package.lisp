(defpackage :braculon
  (:nicknames :brac)
  (:use :cl
	:annot
	:annot.class
	:alexandria
	:cl-who)
  (:export
   #:*appstate*))

(defpackage :brac-conf
  (:use :cl :braculon)
  (:documentation "Symbols read by braculon from config files are put into this package."))
