(defsystem braculon
  :version "0.1.2"
  :author "Evgeny Ilyushkin"
  :license "MIT"
  :depends-on (:alexandria
	       :jpl-queues
	       :cl-syntax
	       :cl-syntax-annot
	       :local-time
	       :cl-ppcre
	       :cl-who
	       :clack
	       :lack-middleware-static
	       :trivial-timers)
  :components ((:module
		"src"
		:components
		((:file "package")
		 (:file "util" :depends-on ("package"))
		 (:file "reqstate" :depends-on ("util"))
		 (:file "braculon" :depends-on ("reqstate"))
		 (:file "rule" :depends-on ("braculon"))
		 (:file "action" :depends-on ("braculon"))
		 (:file "chain" :depends-on ("rule" "action"))
		 (:file "view" :depends-on ("braculon"))
		 (:file "view-compiler" :depends-on ("braculon"))))
	       (:static-file "LICENSE.md"))
  :description "Modular and customizable Clack-based web framework"
  :long-description "")
