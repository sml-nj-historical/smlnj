(* bug905.5.sml *)

functor F =
  let val lr = ref []
      functor F1 (type t) =
      struct
	exception Empty

	(* t -> unit *)
	fun put (x : t) = lr := [x]

	(* unit -> t *)
	fun get () = 
	  case !lr of [x] => x | _ => raise Empty

      end (* functor F *)
   in F1
  end; (* let *)

structure A = F(type t = unit -> unit);
structure B = F(type t = bool);

val x = B.put true;
val y = (A.get ()) ();

