structure InteractiveSystem =
    IntShare (structure VC = Compiler
	      val setRetargetPervStatEnv = CMB.setRetargetPervStatEnv
	      val cmbmake = fn bindir => (CMB.make' (SOME bindir);
					  CMB.wipeOut ()))
