(*
 * Parsing and executing a pre-loading spec file.
 *   This is used during bootstrap.
 *
 *   Copyright (c) 1999 by Lucent Bell Laboratories
 *
 * author: Matthias Blume (blume@cs.princeton.edu)
 *)
structure Preload :> sig
    type loader = string -> bool
    val preload : { make: loader, autoload: loader } -> loader
end = struct
    type loader = string -> bool
    fun preload { make, autoload } specfile = let
	fun work s = let
	    fun loop () = let
		fun load loader f = if loader f then loop () else false
		fun isComment l =
		    size l > 0 andalso String.sub (l, 0) = #"#"
	    in
		case TextIO.inputLine s of
		    "" => true
		  | line =>
			if isComment line then loop ()
			else (case String.tokens Char.isSpace line of
				  ["make", f] => load make f
				| ["autoload", f] => load autoload f
				| _ => (Say.say ["Illegal line in ", specfile,
						 ": ", line];
					loop ()))
	    end
	in
	    loop ()
	end
    in
	SafeIO.perform { openIt = fn () => TextIO.openIn specfile,
			 closeIt = TextIO.closeIn,
			 work = work,
			 cleanup = fn _ => () }
    end
end
