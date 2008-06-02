(* This example produces a FORMstr which should be handled/translated *)

functor F(functor G() : 
		  sig 
		      type t
		      val x : t 
		  end) = 
struct 
  structure M = G()
  val y = M.x
end