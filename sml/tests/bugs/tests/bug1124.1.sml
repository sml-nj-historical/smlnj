(* bug1124.1.sml *)

signature F =
sig
  type word
  val u: word
end;

functor L (structure Base: sig type word end
	   val u: Base.word) =
struct
  datatype word = W of Base.word
  val u = W u
end; (* struct *)

structure W': F = L (structure Base = Word32
		     val u = 0w0:Word32.word);

structure s = W';
