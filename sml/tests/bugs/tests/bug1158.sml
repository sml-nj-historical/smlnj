(* bug1158.sml *)

signature PROTOBUG =
sig
  type address
  datatype session = S of address -> int
  val session: session
end

structure Arg =
struct
  structure Lower =
    struct
      type address = Word32.word
      datatype session = S of address -> int
      val session = S (fn a => Word32.toInt a)
    end
   datatype address = A of Word32.word
   fun resolve (A host) = host
end

functor Connectbug (structure Lower: PROTOBUG
		    type address
		    val resolve: address -> Lower.address) =
struct
  type address = address
  datatype session = S of address -> int
  val session = S (case Lower.session of (Lower.S lower_connect) =>
		     (fn address => lower_connect (resolve address)))
end

structure AA = Connectbug (Arg)

val should_be_one = case AA.session of AA.S connect => connect (Arg.A 0w1)
