(* copyright 1998 YALE FLINT PROJECT *)

signature INTSETF =
sig
    type intset
    val empty : intset
    val singleton: int -> intset
    val make: int list -> intset

    val size: intset -> int
    val isEmpty: intset -> bool

    val add: int * intset -> intset
    val rmv: int * intset -> intset
    val member : intset -> int -> bool

    val union : intset * intset -> intset
    val diff : intset * intset -> intset
    val inter : intset * intset -> intset

    val members: intset -> int list
    val fold: (int * 'a -> 'a) -> 'a -> intset -> 'a
end

structure IntSetF :> INTSETF =
struct
local
    structure M = IntmapF
(*      fun bug msg = ErrorMsg.impossible ("IntSetF: "^msg) *)
(*      fun assert p = if p then () else bug ("assertion failed") *)
in
    type intset = unit M.intmap

    val empty = M.empty
    fun size s = M.cardinality s
    fun add (i,s) = M.add(s, i, ())
    fun rmv (i,s) = M.delete(i, s)
    fun member s i = (M.lookup s i; true) handle M.IntmapF => false

    fun union (s1,s2) = M.overlay(s1, s2)
    fun diff (s1,s2) = M.difference(s1, s2)
    fun inter (s1,s2) = diff(s1, diff(s1, s2))

    fun members s = map #1 (M.members s)
    fun fold f b s = M.fold (fn (x,(),r) => f(x, r)) b s

    fun singleton i = add(i, empty)
    fun make is = foldl add empty is
    fun isEmpty s = size s = 0

end
end

