(* plist-sig.sml
 *
 * COPYRIGHT (c) 1999 Bell Labs, Lucent Technologies.
 *
 * Property lists using Stephen Weeks's implementation.
 *)

signature PROP_LIST = 
  sig 
    type holder 

    val newHolder : unit -> holder 

    val clearHolder : holder -> unit

  (* newProp (selHolder, init)
   * creates a nre property for objects of type 'a and returns
   * functions to get the property and clear it.  The function
   * selHolder is used to select the holder field from an object
   * and init is used to create the initial property value.
   * Typically, properties are reference cells, so that they can
   * be modified.
   *)
    val newProp : (('a -> holder) * ('a -> 'b)) -> {
	    getFn : 'a -> 'b,
	    clrFn : 'a -> unit
	  }

    val newFlag : ('a -> holder) -> {
	    getFn : 'a -> bool,
	    setFn : ('a * bool) -> unit
	  }

  end

