(* 287.sml *)

signature SIG =
sig
  datatype t = A of b
  and b = B
  structure T :
    sig
      datatype u = datatype t
    end
end;

structure S =
struct
  datatype t = A of b
  and b = B
  structure T =
    struct
      datatype u = datatype t
    end
end;

structure S1 : SIG = S;
