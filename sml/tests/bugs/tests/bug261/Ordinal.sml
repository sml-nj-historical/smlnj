(*---------------------------- Ordinal.sml ---------------------*)


signature ORD_RANGE =
  sig
    type elem
    val ord : elem -> int
    and de_ord : int -> elem
  end

functor NatFn() : ORD_RANGE =
  struct
    type elem = int
    fun ord x = x
    fun de_ord x = x
  end

functor CharFn() : ORD_RANGE =
  struct
    type elem = string
    val ord = String.ord
    val de_ord = chr
  end
