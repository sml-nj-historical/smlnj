signature MLTREE_HASH = sig
  structure T  : MLTREE
  val hash     : T.labexp -> word

  val hashStm   : T.stm -> word
  val hashRexp  : T.rexp -> word
  val hashFexp  : T.fexp -> word
  val hashCCexp : T.ccexp -> word

end