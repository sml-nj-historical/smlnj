(*
 * A family of types and corresponding values representing natural numbers.
 *   (An encoding in SML without using dependent types.)
 *
 *   (C) 2000, Lucent Technologies, Bell Laboratories
 *
 * author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
structure Dim :> sig

    (* Internally, a value of type ('a, 'z) dim is just a number.  The trick
     * here is to give each of these numbers a different unique type.
     * 'a will be a decimal encoding of the number's value in "type digits".
     * 'z keeps track of whether the number is zero or not. *)
    type ('a, 'z) dim

    (* We can always get the internal number back... *)
    val toInt : ('a, 'z) dim -> int

    (* These two types act as "flags". They will be substituted for 'z
     * and remember whether the value is zero or not. *)
    type zero
    type nonzero

    (* These are the "type digits".  Type "dec" acts as a "terminator".
     * We chose its name so to remind us that the encoding is decimal.
     * If a nonzero value's decimal representation is "<Dn>...<D0>", then
     * its type will be "(dec dg<Dn> ... dg<D0>, nonzero) dim".  The
     * type of the zero value is "(dec, zero) dim". *)
    type dec
    type 'a dg0
    type 'a dg1
    type 'a dg2
    type 'a dg3
    type 'a dg4
    type 'a dg5
    type 'a dg6
    type 'a dg7
    type 'a dg8
    type 'a dg9

    (* Here are the corresponding constructors for ('a, 'z) dim values.
     * The type for dg0 ensures that there will be no "leading zeros" in
     * any encoding.  This ensures a 1-1 correspondence of (constructable)
     * values and their types.
     * To construct the value corresponding to a nonzero number whose
     * decimal representation is "<Dn>...<D0>", one must invoke
     * "dg<D0> (... (dg<Dn> dec)...)", i.e., the least significant
     * digit appears leftmost. *)
    val dec : (dec, zero) dim
    val dg0 : ('a, nonzero) dim -> ('a dg0, nonzero) dim
    val dg1 : ('a, 'z) dim -> ('a dg1, nonzero) dim
    val dg2 : ('a, 'z) dim -> ('a dg2, nonzero) dim
    val dg3 : ('a, 'z) dim -> ('a dg3, nonzero) dim
    val dg4 : ('a, 'z) dim -> ('a dg4, nonzero) dim
    val dg5 : ('a, 'z) dim -> ('a dg5, nonzero) dim
    val dg6 : ('a, 'z) dim -> ('a dg6, nonzero) dim
    val dg7 : ('a, 'z) dim -> ('a dg7, nonzero) dim
    val dg8 : ('a, 'z) dim -> ('a dg8, nonzero) dim
    val dg9 : ('a, 'z) dim -> ('a dg9, nonzero) dim

end = struct

    type ('a, 'z) dim = int
    fun toInt d = d

    type dec = unit
    type 'a dg0 = unit
    type 'a dg1 = unit
    type 'a dg2 = unit
    type 'a dg3 = unit
    type 'a dg4 = unit
    type 'a dg5 = unit
    type 'a dg6 = unit
    type 'a dg7 = unit
    type 'a dg8 = unit
    type 'a dg9 = unit

    type zero = unit
    type nonzero = unit

    local
	fun dg n d = 10 * d + n
    in
        val dec = 0
        val dg0 = dg 0
        val dg1 = dg 1
        val dg2 = dg 2
        val dg3 = dg 3
        val dg4 = dg 4
        val dg5 = dg 5
        val dg6 = dg 6
        val dg7 = dg 7
        val dg8 = dg 8
        val dg9 = dg 9
    end
end
