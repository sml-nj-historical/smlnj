(*
 * A new "era" starts when something substantial happens that potentially
 * invalidates existing pathname elaborations.  This includes system
 * startup, changes to the path configuration, or extensive moving-about
 * of files in the filesystem.
 *
 * Copyright (c) 1999 by Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@cs.princeton.edu)
 *)
structure Era = struct
    local
	val era = ref (ref ())
    in
	fun newEra () = era := ref ()
	fun thisEra () = !era
	fun isThisEra e = e = !era
    end
end
