(* bug679.sml *)
(* "Compiler bug: addObject" while compiling the Edinburgh library *)

functor F (X: sig end) = (* has to be a functor, not a structure *)
struct
  abstype s = S
  with
    type t = unit  (* type definition necessary *)
  end
end
