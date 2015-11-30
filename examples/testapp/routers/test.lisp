(defrouter test (env)
  (let ((state (appstate env)))
    (pack-routing-data env
		       (get-router state 'brac-conf::test)
		       (get-controller state 'brac-conf::test)
		       nil)
    (format t "Redefined test router reporting.~%state: ~W~%env: ~A~%"
	    state env)
    env))
