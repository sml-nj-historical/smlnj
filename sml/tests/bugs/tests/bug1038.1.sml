(* bug1038.sml *)

signature S =
sig
  type U
  type a
  datatype 'a internal_address = Address of 'a
  type T = a internal_address
    sharing type T = U
end;

functor f(structure s : S) = struct end;
