(* bug905.6.sml *)

functor H(X: sig
	       functor F1(type t): sig val put: t -> unit
				       val get: unit -> t
				   end
	     end)
  = X.F1;

functor F =
  let val lr = ref []
   in H(struct
	  functor F1 (type t) =
	    struct
	      (* t -> unit *)
	      fun put (x : t) = lr := [x]

	      (* unit -> t *)
	      fun get () = hd(!lr)

	    end (* functor F *)
       end)
  end; (* let fctexp *)

structure A = F(type t = unit -> unit);
structure B = F(type t = bool);

val x = B.put true;
val y = (A.get ()) ();

