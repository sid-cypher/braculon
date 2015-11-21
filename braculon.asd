(defsystem braculon
  :version "0.1.2"
  :author "Evgeny Ilyushkin"
  :license "MIT"
  :depends-on (:alexandria
	       :cl-ppcre
	       :cl-who
	       :clack
	       :lack-middleware-static
	       :trivial-timers)
  :components ((:module "src"
                :components
		((:file "package")
                 (:static-file "LICENSE")
	         (:file "util" :depends-on ("package"))
	         (:file "braculon" :depends-on ("util"))
		 (:file "conf-macros" :depends-on ("braculon"))
	         (:file "router" :depends-on ("braculon"))
	         (:file "controller" :depends-on ("braculon"))
	         (:file "view" :depends-on ("braculon")))))
  :description "Modular and customizable Clack-based web framework"
  :long-description "")
