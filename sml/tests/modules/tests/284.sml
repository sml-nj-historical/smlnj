(* 284.sml *)
(* where types and include *)

signature S =
sig
  type t
end;

signature S1 = S where type t = int;

(* is this legal? *)
signature S2 =
sig
  include S1
  type u
  sharing type t = u
end;


(*
Should this be equivalent to:

  signature S2 =
  sig
    type t
    type u
    sharing type t = u
  end
  where type t = s

which is legal, or should it be equivalent to:

  signature S2 = 
  sig
    type t = int
    type u
    sharing type t = u
  end;

which is not? 

Looks like it will be equivalent to the second.
*)
