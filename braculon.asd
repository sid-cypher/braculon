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
	       (:file "util" :depends-on ("package"))
	       (:file "braculon" :depends-on ("package" "util"))
	       (:file "router" :depends-on ("package" "util"))
	       (:file "controller" :depends-on ("package")))
  :description ""
  :long-description "")
