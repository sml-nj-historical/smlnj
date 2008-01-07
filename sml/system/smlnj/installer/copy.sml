(* copy.sml
 *
 *   Simple file copy.
 *
 * Copyright (c) 2008 by The Fellowship of SML/NJ
 *
 * Author: Matthias Blume (blume@tti-c.org)
 *)
structure Copy : sig

    val copy : { from : string, to : string } -> unit

end = struct

    fun copy { from, to } =
	let val ins = TextIO.openIn from
	    val outs = TextIO.openOut to
	    fun loop NONE = (TextIO.closeIn ins; TextIO.closeOut outs)
	      | loop (SOME l) = (TextIO.output (outs, l); next ())
	    and next () = loop (TextIO.inputLine ins)
	in next ()
	end
end
