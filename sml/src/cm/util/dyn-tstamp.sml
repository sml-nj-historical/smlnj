(*
 * "Time" stamps for dynamic envirenments.
 *
 * Copyright (c) 1998 by Lucent, Bell Laboratories
 *
 * author: Matthias Blume (blume@cs.princeton.edu)
 *)
signature DYNTSTAMP = sig

    type dts

    val bogus: dts
    val ancient: dts

    val reset: unit -> unit
    val new: unit -> unit

    val current: unit -> dts
    val noshare: string -> dts

    val outdated: { context: dts, oldresult: dts } -> bool
    val join: dts * dts -> dts
    val can'tShare: dts -> string list option
end

structure DynTStamp :> DYNTSTAMP = struct

    datatype dts =
	SHARE of int
      | NOSHARE of StringSet.set

    local
	val era = ref 0
    in
	fun current () = SHARE (!era)
	fun reset () = era := 0
	fun new () = era := (!era) + 1
    end

    val bogus = SHARE 0
    val ancient = SHARE (~1)

    fun noshare s = NOSHARE (StringSet.singleton s)

    fun outdated { context = SHARE c, oldresult = SHARE r } = c > r
      | outdated _ = true

    fun join (SHARE x, SHARE y) = SHARE (if x > y then x else y)
      | join (x as NOSHARE _, SHARE _) = x
      | join (SHARE _, x as NOSHARE _) = x
      | join (NOSHARE x, NOSHARE y) = NOSHARE (StringSet.union (x, y))

    fun can'tShare (NOSHARE s) = SOME (StringSet.listItems s)
      | can'tShare _ = NONE
end
