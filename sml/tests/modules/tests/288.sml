(* 288.sml *)

signature SIG =
sig
  datatype t = A of c
  and b = B
  withtype c = b
  structure T :
    sig
      datatype u = datatype t
    end
end;

structure S =
struct
  datatype t = A of c
  and b = B
  withtype c = b
  structure T =
    struct
      datatype u = datatype t
    end
end;

structure S1 : SIG = S;
