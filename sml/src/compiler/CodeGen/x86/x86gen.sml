structure X86MC = 
  FLINTComp(
    structure Gen=X86CG
    fun collect epthunk = (Gen.finish ();
			   CodeString.getCodeString (epthunk ())))
