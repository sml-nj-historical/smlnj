(* bug1522.1.sml *)

structure S0 =
struct
  datatype t = C | D
end;

signature S =
sig
  datatype t = datatype S0.t
end;

structure S1 :> S =
struct
  datatype t = datatype S0.t
end;

structure S2 =
struct
  datatype t = datatype S1.t
end;
