(* tests a problem from hppa/hppaCG.sml *)

signature INSTR =
sig
  structure C : sig end
end;

structure Instr =
struct
  structure C = struct end
end;

signature FLOW =
sig
  structure I : INSTR
  structure C : sig end (* needed, with name C preceding I *)
end;

structure Flow : FLOW = 
struct
  structure I = Instr
  structure C = struct end
end;

signature E =
sig
  structure I : INSTR
  structure F : FLOW  
    sharing F.I = I
end;

functor BF(X : E) = struct end;

functor CG(Y : E where F = Flow) = (* where needed *)
struct
  structure A = BF(Y)
end;

