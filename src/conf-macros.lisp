(in-package :brac-conf)

;;TODO: DRY - defthing macro to define routers, ctrls, views, vccs
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

(defmacro defcontroller (name env-var &body body)
  (let ((callvar (gensym)))
    `(progn
       (let ((using-packages (getf (brac:extensions brac:*appstate*)
				   :read-using-packages)))
	 (when using-packages
	   (use-package using-packages)))
       (let ((,callvar (lambda (,env-var) ,@body)))
	 (brac::add-controller brac:*appstate*
		     (make-instance 'brac:brac-ctrl
				    :parent brac:*appstate*
				    :name ',name
				    :callable ,callvar
				    :source-file brac::*controller-src-file*))))))

(defmacro defview (name env-var field-list dep-list &body body)
  (let ((callvar (gensym)))
    `(progn
       (let ((using-packages (getf (brac:extensions brac:*appstate*)
				   :read-using-packages)))
	 (when using-packages
	   (use-package using-packages)))
       (let ((,callvar (lambda (,env-var) ,@body)))
	 (brac::add-view brac:*appstate*
		     (make-instance 'brac:brac-view
				    :parent brac:*appstate*
				    :name ',name
				    :fields (coerce ',field-list 'simple-vector)
				    :dependencies (coerce ',dep-list 'simple-vector)
				    :renderable ,callvar
				    :source-file brac::*view-src-file*))))))
