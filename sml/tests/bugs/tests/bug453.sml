(* bug453.sml *)

structure Util = 
struct
  exception Bug of string
end;

structure InstProto = 
struct
  structure U = Util
  structure S = struct end 
end;

open InstProto;

(raise U.Bug "hi"):unit;

