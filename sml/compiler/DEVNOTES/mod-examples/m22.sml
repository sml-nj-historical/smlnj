(* bug 587.sml *)
(* Compiler bug: ModuleUtil: Instantiate:getSigPos.2<Argument> *)

signature S1 = sig end;

signature S2 = 
  sig
    type t1
    type t2
    val f : t2 -> t2 -> unit
  end;

funsig SF (X : S1) = S2;

functor F (functor XF : SF) = struct end;
