(* bug905.4.sml *)

structure S =
  let val lr = ref []
  in
     struct
      functor F (type t) =
      struct
	exception Empty

	(* t -> unit *)
	fun put (x : t) = lr := [x]

	(* unit -> t *)
	fun get () = 
	  case !lr of [x] => x | _ => raise Empty

      end (* functor F *)
     end
  end; (* let *)

structure A = S.F(type t = unit -> unit);
structure B = S.F(type t = bool);

val x = B.put true;
val y = (A.get ()) ();

