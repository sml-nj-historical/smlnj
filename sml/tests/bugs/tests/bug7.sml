(* bug7.sml *)

signature S = sig
    type 'a t
    datatype 'a List = NIL | CONS of 'a t
end;
structure A : S = struct
    datatype 'a List = NIL | CONS of 'a * 'a List
    withtype 'a t = 'a * 'a List
end;
