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
    val cancel : mode * string -> unit
    val reset : mode -> unit

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

    fun set0 mkAbsolute (m, a, s) = let
	val s' =
	    if OS.Path.isAbsolute s then s
	    else mkAbsolute s
    in
	m := (Era.newEra (); StringMap.insert (!m, a, s'))
    end
    fun set x = let
	fun mkAbsolute rel =
	    OS.Path.mkAbsolute { path = rel,
				 relativeTo = OS.FileSys.getDir () }
    in
	set0 mkAbsolute x
    end
    fun reset m = m := (Era.newEra (); StringMap.empty)
    fun cancel (m, a) =
	(Era.newEra ();
	 (m := #1 (StringMap.remove (!m, a)))
	 handle LibBase.NotFound => ())

    fun new () = ref StringMap.empty

    fun configAnchor m s = let
	fun look () = StringMap.find (!m, s)
	fun get () =
	    case look () of
		SOME v => v
	      (* Return a bogus value here that will later cause a failure
	       * when actually opening a file.  We don't want to fail here
	       * because the anchor may come back to life later. *)
	      | NONE => concat ["$$undefined<", s, ">"]
    in
	case look () of
	    NONE => NONE
	  | SOME _ => SOME get
    end

    fun processSpecFile (m, f) = let
	val full_f_dir = OS.Path.dir (OS.FileSys.fullPath f)
	fun set x = let
	    fun mkAbsolute rel =
		OS.Path.mkAbsolute { path = rel, relativeTo = full_f_dir }
	in
	    set0 mkAbsolute x
	end
	fun work s = let
	    fun loop () = let
		val line = TextIO.inputLine s
	    in
		if line = "" then ()
		else case String.tokens Char.isSpace line of
		    [a, d] => (set (m, a, d); loop ())
		  | ["-"] => (reset m; loop ())
		  | [a] => (cancel (m, a); loop ())
		  | _ => (Say.say [f, ": malformed line (ignored)\n"]; loop ())
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
