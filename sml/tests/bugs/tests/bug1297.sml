(* bug1297.sml *)

signature U =
sig
  structure T : sig end
end

signature V2 =
sig
  structure T1 : sig end
  structure T2 : sig end
  structure T3 : sig end
  structure T4 : sig end
  structure T5 : sig end
  structure T6 : sig end
  structure T7 : sig end

  structure U1 : U where T = T1
  structure U2 : U where T = T2
  structure U3 : U where T = T3
  structure U4 : U where T = T4
  structure U5 : U where T = T5
  structure U6 : U where T = T6
  structure U7 : U where T = T6
end
   
structure V2 :> V2 =
struct

  structure T = struct end
  structure T1 = T
  structure T2 = T
  structure T3 = T
  structure T4 = T
  structure T5 = T
  structure T6 = T
  structure T7 = T

  structure U = struct structure T = T end
  structure U1 = U
  structure U2 = U
  structure U3 = U
  structure U4 = U
  structure U5 = U
  structure U6 = U
  structure U7 = U

end
