(* (C) 1999 Lucent Technologies, Bell Laboratories *)


structure PrePervasive = struct

    exception Span

    datatype order = LESS | EQUAL | GREATER

    datatype option = datatype Assembly.option

    exception Option

    fun getOpt (SOME x, y) = x
      | getOpt (NONE, y) = y

    fun isSome (SOME _) = true
      | isSome NONE = false

    fun valOf (SOME x) = x
      | valOf NONE = raise Option

    val op = : ''a * ''a -> bool = InlineT.=
    val op <> : ''a * ''a -> bool = InlineT.<>
end
