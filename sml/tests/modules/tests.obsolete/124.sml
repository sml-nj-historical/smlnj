signature S1 =
sig
   type t
   structure A : sig type z end
end;

signature S3 =
sig
   structure B : S1   (* should work *)
   open B
   open A
end;

signature S2 =
sig
   structure B : S1
   open B A   (* shouldn't work! *)
end;
