signature S =
sig
  datatype T = C
  type s = int * T
end;

structure A : S =
struct
  datatype T = C
  type s = int * T
end;
