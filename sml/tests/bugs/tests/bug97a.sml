(* bug 97, Tofte -- type error in last line *)

signature VAL = sig type value end;

functor F(structure X: VAL) : sig val f: X.value -> unit end =
struct
  fun f(v) = ()
end;

structure M = F(structure X = struct type value = unit end);

M.f();
    

