(* 257.sml *)

signature T =
sig
  type parse_context
  structure Tacticals :
    sig
      type tactic_tree
    end
  val parse : unit -> Tacticals.tactic_tree list
  (* option type error not even discovered here *)
end;

functor PD(type result_type):
  sig
    val parse : (unit -> result_type) -> result_type
  end = 
struct
  fun parse f = f()
end;

functor FUN(structure P : T) : 
  sig
    type resulttype = (P.Tacticals.tactic_tree list)
    val mkTactic : P.parse_context -> resulttype option
  end = 
struct
  type resulttype = P.Tacticals.tactic_tree list
  structure TacticParser = PD(type result_type = resulttype)
  fun mkTactic(root,context) = TacticParser.parse P.parse 
end;
