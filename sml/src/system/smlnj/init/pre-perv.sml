(* (C) 1999 Lucent Technologies, Bell Laboratories *)


structure PrePervasive = struct

    exception Span

    open Order

    datatype option = datatype Assembly.option

    exception Option

    fun getOpt (SOME x, y) = x
      | getOpt (NONE, y) = y

    fun isSome (SOME _) = PrimTypes.true
      | isSome NONE = PrimTypes.false

    fun valOf (SOME x) = x
      | valOf NONE = raise Option

    val op = : ''a * ''a -> PrimTypes.bool = InlineT.=
    val op <> : ''a * ''a -> PrimTypes.bool = InlineT.<>
end
