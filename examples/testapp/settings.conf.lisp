(app-config
  :name "hgoc"
  :routing-chain ((fixed "/" index)
		  (fixed "/hello" hello)
		  (fixed "/test/" test)
		  (fixed "/e405/" e405)
		  static
		  test)
  :routers-path #p"routers/"
  :controllers-path #p"controllers/"
  :views-path #p"views/"
  :view-compilers-path #p"viewcc/")
