signature S =
sig
  datatype t = T
end;

structure A : S =
struct
  datatype t = T
end;
