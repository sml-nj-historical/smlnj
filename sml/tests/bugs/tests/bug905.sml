(* bug905.sml *)

local val lr = ref []
in
 
  functor F (type t) =
  struct
    exception Empty

    (* t -> unit *)
    fun put (x : t) = lr := [x]
 
    (* unit -> t *)
    fun get () = 
      case !lr of [x] => x | _ => raise Empty
  
  end (* functor F *)

  structure A = F(type t = unit -> unit)
  structure B = F(type t = bool)
 
  val x = B.put true 
  val y = (A.get ()) ()

end
