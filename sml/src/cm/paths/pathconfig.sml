(*
 * Configurable path anchors for new CM.
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)

signature PATHCONFIG = sig

    type mode

    val new : unit -> mode
    val set : mode * string * string -> unit

    val configAnchor : mode -> string -> (unit -> string) option

    val processSpecFile : mode * string -> unit
end

(*
 * The names of config anchors must be names of actual files.
 * Function configAnchor will map the name of the anchor to
 * the directory that contains the corresponding file.
 *)
structure PathConfig :> PATHCONFIG = struct

    type mode = string StringMap.map ref

    fun set (m, a, s) = m := StringMap.insert (!m, a, s)

    fun new () = ref (StringMap.empty)

    fun configAnchor m s =
	case StringMap.find (!m, s) of
	    NONE => NONE
	  | SOME _ => SOME (fn () => valOf (StringMap.find (!m, s)))

    fun processSpecFile (m, f) = let
	fun work s = let
	    fun loop () = let
		val line = TextIO.inputLine s
	    in
		if line = "" then ()
		else case String.tokens Char.isSpace line of
		    [a, d] => (set (m, a, d);
			       Say.vsay ["PathConfig: ", a, " -> ", d, "\n"];
			       loop ())
		  | _ => (Say.say [f, ": malformed line (ignored)\n"];
			  loop ())
	    end
	in
	    loop ()
	end
    in
	SafeIO.perform { openIt = fn () => TextIO.openIn f,
			 closeIt = TextIO.closeIn,
			 work = work,
			 cleanup = fn () => () }
    end
end
