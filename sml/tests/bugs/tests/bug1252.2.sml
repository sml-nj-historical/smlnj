(* bug1252.2.sml *)

signature SIG = sig type v end;

functor F() =
struct
  structure S : SIG =
    struct
       datatype u = U
       type v = u
    end
  structure T = 
  struct
    datatype t = T of S.v
  end
end;

structure S = F();
