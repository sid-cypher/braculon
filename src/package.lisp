(defpackage :braculon
  (:nicknames :brac)
  (:use :cl :alexandria :cl-who)
  (:export
   #:start
   #:stop
   #:wizard))

(defpackage :brac-conf
  (:documentation "Symbols read by braculon from config files are put into this package."))
