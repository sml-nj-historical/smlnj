(* bug1041.sml *)

signature XTERN =
sig
  type T
  val t: T
end;

structure W =
struct
  type word =  Word32.word
  val zero : word = 0w0
end;

functor Ip_Flag_Extern (structure X: XTERN where type T = W.word) =
struct
  val result = X.t = W.zero
end;
