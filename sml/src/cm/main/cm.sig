(*
 * This is the signature for a "full" structure CM.  This structure gets
 * constructed in cm-boot.sml and is made available at top-level by
 * (auto-)loading the library "full-cm.cm".
 * (After system startup only a "minimal" structure CM is visible.)
 *
 *   Copyright (c) 1999 by Lucent Bell Laboratories
 *
 * author: Matthias Blume (blume@cs.princeton.edu)
 *)
signature CM = sig

    include MINIMAL_CM

    type 'a controller = { get : unit -> 'a, set : 'a -> unit }

    structure Anchor : sig
	val set : { anchor: string, path: string } -> unit
	val cancel : string -> unit
	val reset : unit -> unit
    end

    structure Control : sig
	val keep_going : bool controller
	val verbose : bool controller
	val parse_caching : int controller
	val warn_obsolete : bool controller
	val debug : bool controller
    end

    structure Library : sig
	type lib
	val known : unit -> lib list
	val descr : lib -> string
	val osstring : lib -> string
	val dismiss : lib -> unit
    end

    structure State : sig
	val synchronize : unit -> unit
	val reset : unit -> unit
	val pending : unit -> string list
    end

    structure Server : sig
	type server
	val start : { cmd : string * string list,
		      name : string,
		      pathtrans : (string -> string) option,
		      pref : int } -> server option
	val stop : server -> unit
	val kill : server -> unit
	val name : server -> string
    end

    val symval : string -> int option controller
end
