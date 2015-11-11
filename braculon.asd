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
	         (:file "braculon" :depends-on ("package" "util"))
	         (:file "router" :depends-on ("package" "util"))
	         (:file "controller" :depends-on ("package" "util"))
	         (:file "view" :depends-on ("package" "util")))))
  :description "Modular and customizable Clack-based web framework"
  :long-description "")
