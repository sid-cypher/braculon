(defsystem braculon
  :version "0.1.2"
  :author "Evgeny Ilyushkin"
  :license "MIT"
  :depends-on (:alexandria
	       :cl-syntax
	       :cl-syntax-annot
	       :local-time
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
	         (:file "router" :depends-on ("braculon"))
	         (:file "controller" :depends-on ("braculon"))
	         (:file "view" :depends-on ("braculon"))
	         (:file "view-compiler" :depends-on ("braculon"))
		 (:file "conf-macros"
			:depends-on ("router" "controller" "view" "view-compiler")))))
  :description "Modular and customizable Clack-based web framework"
  :long-description "")
