(* use-hook.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure UseHook =
  struct
    local
      fun dummy (s: PrimTypes.string) = ()
    in

    val useHook = PrimTypes.ref dummy

    fun use s = InlineT.! useHook s

    end

  end
