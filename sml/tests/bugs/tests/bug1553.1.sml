(* bug1553.1.sml *)

signature S =
sig
  type flags
  val toWord : flags -> Word32.word
  val fromWord : Word32.word -> flags
  val flags : flags list -> flags
  val allSet : flags * flags -> bool
  val anySet : flags * flags -> bool
  type mode = flags
  val irwxu : mode
  val irusr : mode
  val iwusr : mode
  val ixusr : mode
  val irwxg : mode
  val irgrp : mode
  val iwgrp : mode
  val ixgrp : mode
  val irwxo : mode
  val iroth : mode
  val iwoth : mode
  val ixoth : mode
  val isuid : mode
  val isgid : mode
  (**** Comment out the following line to obtain a compilation.  bug BUG ****)
  sharing type flags = mode
end;
