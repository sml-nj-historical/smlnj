(* amd64gen.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

functor AMD64MC (

    structure CCallParams : sig
	val frameAlign : int
	val returnSmallStructsInRegs : bool
      end

    val abi_variant: string option

  ) = FLINTComp(
    structure Gen = AMD64CG (
	structure CCallParams = CCallParams
	val abi_variant = abi_variant)
    fun collect epthunk = (
	  Gen.finish ();
	  CodeString.getCodeString (epthunk ())))
