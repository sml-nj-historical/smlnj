signature S1 =
sig
  eqtype t
  val x : t
end;

signature S2 =
sig
  structure A : sig end
  structure C : sig structure A : S1 end
  sharing A = C.A
end;
