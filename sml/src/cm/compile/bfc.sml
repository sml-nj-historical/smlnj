(*
 * Keeping binfiles for short periods of time.
 *   This is used in "stabilize" and in "make" where first there is a
 *   "compile" traversal that produces certain binfile contents, and
 *   then there is a "consumer" traversal that uses the binfile contents.
 *   No error checking is done -- the "get" operation assumes that the
 *   stuff is either in its cache or in the file system.
 *   Moreover, the static environment cannot be used (BF.senvOf will fail
 *   if the binfile had to be reloaded from disk).
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
signature BFC = sig
    type bfc
    val new : unit -> { store: SmlInfo.info * bfc -> unit,
		        get: SmlInfo.info -> bfc }
end

functor BfcFn (structure MachDepVC : MACHDEP_VC) :> BFC
    where type bfc = MachDepVC.Binfile.bfContent =
struct

    structure BF = MachDepVC.Binfile
    structure E = GenericVC.Environment
    type bfc = BF.bfContent

    val estat = E.staticPart E.emptyEnv

    fun new () = let
	val m = ref SmlInfoMap.empty

	fun store (i, bfc) = m := SmlInfoMap.insert (!m, i, bfc)

	fun get i =
	    case SmlInfoMap.find (!m, i) of
		SOME bfc => bfc
	      | NONE => let
		    val binname = SmlInfo.binname i
		    fun reader s = let
			val bfc = BF.read { stream = s, name = binname,
					    senv = estat }
		    in
			store (i, bfc);
			bfc
		    end
		in
		    SafeIO.perform { openIt = fn () => BinIO.openIn binname,
				     closeIt = BinIO.closeIn,
				     work = reader,
				     cleanup = fn () => () }
		end
    in
	{ store = store, get = get }
    end
end
