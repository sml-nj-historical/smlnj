(* bug737.sml *)
(* 737. Compiler bug: Instantiate:explore_tclass.5 *)

signature S0 =
sig
  type t
end;

signature S1 =
sig
  structure A : S0
end;
    
functor F (X: S0) : S0 =   (* result signature necessary *)
struct
  open X (* necessary, "type t = X.t" won't do *)  
end;

structure B = F(struct type t = unit end);
    (* must be defined by functor application *)

functor G(structure Y: S1 sharing Y.A = B) = struct end;

