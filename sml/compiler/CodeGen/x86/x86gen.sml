(* x86gen.sml
 *
 * Copyright (c) 2006 by The Fellowship of SML/NJ
 *)
functor X86MC (structure CCallParams : sig val frameAlign : int
					   val returnSmallStructsInRegs : bool
				       end
               val abi_variant: string option) =
  CPSCompFn(
    structure Gen=X86CG (structure CCallParams = CCallParams
                         val abi_variant = abi_variant)
    fun collect epthunk = (Gen.finish ();
			   CodeString.getCodeString (epthunk ())))
