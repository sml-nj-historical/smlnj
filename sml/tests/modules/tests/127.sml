signature S1 =
sig
  structure A : sig end  (* substructure necessary *)
end;

signature S2 =
sig
  structure B : S1
end;

signature S3 = 
sig
  structure C : S1
  structure D : S2
  sharing C = D.B
end;
