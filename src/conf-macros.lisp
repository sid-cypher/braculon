(in-package :brac-conf)

#+nil
(let ((using-packages (getf (brac:extensions brac:*appstate*)
			    :read-using-packages)))
  (when using-packages
    (use-package using-packages)))
;; actually, just put app-package and reader-package fields into state

;;TODO: DRY - defthing macro to define routers, ctrls, views, vccs
(defmacro defrouter (name lambda-list &body body)
  (let ((callvar (gensym)))
    `(let ((,callvar (lambda ,lambda-list ,@body)))
       (brac::add-router brac::*appstate*
			 (make-instance 'brac:brac-router
					:parent brac::*appstate*
					:name ',name
					:callable ,callvar
					:source-file brac::*router-src-file*)))))

(defmacro defcontroller (name env-var &body body)
  (let ((callvar (gensym)))
    `(let ((,callvar (lambda (,env-var) ,@body)))
       (brac::add-controller brac::*appstate*
			     (make-instance 'brac:brac-ctrl
					    :parent brac::*appstate*
					    :name ',name
					    :callable ,callvar
					    :source-file brac::*controller-src-file*)))))

(defmacro defview (name env-var field-list dep-list &body body)
  (let ((callvar (gensym)))
    `(let ((,callvar (lambda (,env-var) ,@body)))
       (brac::add-view brac::*appstate*
		       (make-instance 'brac:brac-view
				      :parent brac::*appstate*
				      :name ',name
				      :fields ',field-list
				      :dependencies ',dep-list
				      :renderable ,callvar
				      :source-file brac::*view-src-file*)))))
