(* 
 * Datatype replication should not generate a new flexible tyc stamp
 *)

signature S = 
  sig
    datatype t = A
    structure M : sig datatype u = datatype t end 
  end

functor F(X:S) =
struct
end
