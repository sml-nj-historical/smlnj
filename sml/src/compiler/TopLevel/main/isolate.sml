(* isolate.sml
 *
 * (C) 2001, Lucent Technologies, Bell Labs
 *)
structure Isolate = struct

    exception TopLevelException of exn
    exception TopLevelCallcc

    local 
	val cont_stack = ref (nil : unit ref list)
    in 
    (** just like f x, except that it catches top-level callcc's *)
        fun isolate f x = let
	    val r = ref()
	    val _ = cont_stack := r :: !cont_stack
	    fun pop_stack() =
		case !cont_stack of
		    r' :: rest =>
		    (cont_stack := rest;
		     if r<>r' then raise TopLevelCallcc else ())
		  | _ => raise TopLevelCallcc (* can this ever happen? *)
	    val a = f x
		handle e =>
		       (pop_stack(); 
			raise (case e of TopLevelException x => x | e => e))
	in
	    pop_stack (); a
	end
    end
end
