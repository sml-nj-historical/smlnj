(* bug1417.2.sml *)

signature DB =
sig
  datatype transaction = TRANSACTION of lockinfo
  and lockinfo = READLOCK of unit
end;

functor Evolve(Db : DB) : DB = 
struct 
  datatype lockinfo = datatype Db.lockinfo
  datatype transaction = datatype Db.transaction
end;
