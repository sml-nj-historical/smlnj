(* bug696.sml *)
(* Type system error in "includes" of signatures *)

structure A =
struct

type (''1a, '1b) t = ((''1a * '1b) list)

fun new() = nil
fun toList l = l
fun dom (l:(''a, 'b) t) = List.map #1 l

end

signature ASIG =
  sig
    type (''1a, '1b) t
    val new : unit -> (''1a, '1b) t
  end

signature BSIG =
  sig
    type (''1a, '1b) t
    val dom : (''1a, '1b) t -> ''1a list
  end
;
signature AB_SIG =
  sig
    include ASIG BSIG
  end
;
structure goo : AB_SIG = A;

let val _ = goo.dom (SOME "foo") in 23 end;
