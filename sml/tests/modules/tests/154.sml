signature S =
sig
  structure A : sig end
  structure B : sig end
  sharing A = B
end;
