(* bug1323.1.sml *)

functor F (
      type code 
      val boo : bool option option
      val funcs : bool list
    ) : sig end =
struct
  fun reset() = ()   
  val _ = reset() 

  val pairId = fn (a,b) => (a,b)

  val boolref = ref false
  fun h (a,b) = if !boolref then (a,b) else (a,b)

  fun lookup () =
    case boo of
       SOME(SOME b) => b
      | _ => false

  fun f1 C = if lookup () then pairId C else C

  fun f2 C = C

  fun f (x, C) = if x then f1 C else f2 C

  fun g (x::rest) = h(f(x, g rest))

  val uc: unit * code = g funcs

end;
