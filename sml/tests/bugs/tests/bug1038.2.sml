(* from bug1038.sml *)

signature S =
sig
  type u
  type v
  type t = v
  sharing type t = u
end;

functor f(structure s : S) = struct end;
