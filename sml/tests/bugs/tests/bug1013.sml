(* bug1013.sml *)

signature A =
sig
  type x
  datatype d = D of {extension: x}
end;

signature B =
sig
  include A
  type t
  type b_x = {a: t, b: int}
  sharing type b_x = x
end;

signature C =
sig
  include B
end;

functor F (structure In: C) =
struct
  fun f arg =
       let val In.D {extension} = arg
           val {a, ...}: In.b_x = extension
       in a
       end
end;

