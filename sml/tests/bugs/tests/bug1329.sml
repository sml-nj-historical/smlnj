(* bug1329.sml *)

signature S = sig
  datatype 'a t = A of 'a
end where type 'a t = int;
