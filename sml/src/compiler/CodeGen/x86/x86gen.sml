structure X86MC = 
  FLINTComp(
    structure Gen=X86CG
    fun collect() = (Gen.finish(); CodeString.getCodeString()))
