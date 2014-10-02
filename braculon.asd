(defsystem braculon
  :version "0.1.1"
  :author "Evgeny Ilyushkin"
  :license "MIT"
  :depends-on (:alexandria
	       :cl-fad
	       :cl-ppcre
	       :cl-who
	       :trivial-timers
               :hunchentoot)
  :components ((:file "package")
               (:static-file "LICENSE")
	       (:file "braculon" :depends-on ("package"))
	       (:file "states" :depends-on ("package"))
	       (:file "routers" :depends-on ("package")))
  :description ""
  :long-description "")
