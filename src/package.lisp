(defpackage :braculon
  (:nicknames :brac)
  (:use :cl :alexandria :cl-who)
  (:export
   #:brac-appstate
   #:brac-router
   #:brac-ctrl
   #:*appstate*
   #:root-path ;;TODO: auto-export class accessor symbols
   #:extensions
   #:defrouter))

(defpackage :brac-conf
  (:use :cl :braculon)
  (:documentation "Symbols read by braculon from config files are put into this package."))
