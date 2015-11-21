(in-package :brac-conf)

(defmacro defrouter (name lambda-list &body body)
  (let ((callvar (gensym)))
    `(progn
       (let ((using-packages (getf (brac:extensions brac:*appstate*)
				   :read-using-packages)))
	 (when using-packages
	   (use-package using-packages)))
       (let ((,callvar (lambda ,lambda-list ,@body)))
	 (brac::add-router brac:*appstate*
		     (make-instance 'brac:brac-router
				    :parent brac:*appstate*
				    :name ',name
				    :callable ,callvar
				    :source-file brac::*router-src-file*))))))
