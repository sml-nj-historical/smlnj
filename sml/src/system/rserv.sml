CM.autoload "full-cm.cm";
CM.autoload "host-cmb.cm";
fun serv p n =
    CM.Server.start { name = n, pref = p, pathtrans = NONE,
	 	      cmd = ("/usr/bin/rsh",
			     [n, "/home/blume/ML/current/bin/sml",
			      "@CMslave"]) };

