(* bug905.3.sml *)

structure S =
  let val lr = ref []
      functor F (type t) =
      struct
	exception Empty

	(* t -> unit *)
	fun put (x : t) = lr := [x]

	(* unit -> t *)
	fun get () = 
	  case !lr of [x] => x | _ => raise Empty

      end (* functor F *)
  in
     struct
	 structure A = F(type t = unit -> unit)
	 structure B = F(type t = bool)
     end
  end; (* let *)

val x = S.B.put true;
val y = (S.A.get ()) ();

