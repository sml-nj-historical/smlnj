(*------------------------ BitSet.sml ---------------------------*)


import "Ordinal";

signature BITSET =
  sig
    structure Elem : ORD_RANGE
    exception NoSuchElement
    type bitset
    val empty: bitset
    and	singleton : Elem.elem -> bitset
    and range : Elem.elem * Elem.elem -> bitset
    and setFromList : Elem.elem list -> bitset

    and	exists : Elem.elem -> bitset -> bool
    and union : bitset * bitset -> bitset
    and intersect : bitset * bitset -> bitset
    and difference : bitset * bitset -> bitset

    val isempty : bitset -> bool
    and eq : bitset * bitset -> bool
    and subset : bitset * bitset -> bool
    and subset': bitset * bitset -> bool

    val select : bitset * (Elem.elem -> bool) -> bitset
    val lowest : bitset -> Elem.elem
    val lowest' : bitset -> Elem.elem -> Elem.elem
    val highest : bitset -> Elem.elem
    val highest' : bitset -> Elem.elem -> Elem.elem
    val totOrder : bitset * bitset -> bool
    val forall : bitset -> Elem.elem list
    val makeString : bitset -> string
  end;

(* trivialized version *)
functor BitSetFn(Elem1 : ORD_RANGE) : BITSET =
  struct
    structure Elem = Elem1
    local
      open Elem Bits
      val bits_per_int = 30;
      val all_bits = 1073741823        (* 077777777777 *)
    in
      datatype bitset = BS of {lo : int, hi : int, setx : int array}
      val empty = BS{lo = 0, hi = ~1, setx = array(0, 0)}

      fun singleton x = empty

      fun range(l, h) = empty

      fun exists x (BS{lo, hi, setx}) = false

      fun union(set1, set2) = empty

      exception NoSuchElement

      fun lowest (BS{lo,...}) = de_ord lo

      fun lowest' (BS{lo,...}) start = de_ord lo

      fun highest (BS{hi,...}) = de_ord hi

      fun highest' (BS{hi,...}) start = de_ord hi

      fun reduce bs = empty

      fun intersect(set1, set2) = empty

      fun difference(set1, set2) = empty

      fun isempty (BS{lo, hi,...}) = hi < lo

      fun eq (set1, set2) = true

      fun op subset(s1, s2) = isempty (reduce (difference (s1, s2)))

      fun op subset'(s1, s2) = isempty (reduce (difference (s1, s2)))
		       andalso (not (isempty (reduce (difference (s2, s1)))))

      fun lowQuery (bs, q) =
        let
	  val BS{lo, hi, setx} = bs
          val i = ref lo
        in
          de_ord (!i)
        end
      fun highQuery (bs, q) =
        let
	  val BS{lo, hi, setx} = bs
          val i = ref hi
        in
          de_ord (!i)
        end

      fun select (bs, q) = bs

      fun totOrder (set1, set2) = true

      fun forall s = nil

      fun makeString s = ""

      fun setFromList (l' : Elem.elem list) = empty
    end
  end
