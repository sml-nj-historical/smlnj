(* bug211.sml *)
(* causes uncaught Union *)

functor F(type t) =
struct
  structure K =
  struct 
    type primop = t
  end
  structure M : sig structure L : sig type primop sharing type primop = t end
		    sharing L=K
		end =
  struct
    structure L = K
  end
end;
