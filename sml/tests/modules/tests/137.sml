signature S =
sig
  datatype T = C
  withtype s = int * T
end;

structure A : S =
struct
  datatype T = C
  withtype s = int * T
end;
