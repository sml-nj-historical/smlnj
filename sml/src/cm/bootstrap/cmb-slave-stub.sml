(*
 * This is a stub providing "slave" functionality for CMB.
 * (We use dynamic linking technology to avoid loading host-cmb.cm
 *  on the slave side unless it is really needed.)
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
structure CMBSlave = struct
    local
	val initialized = ref false
    in
	fun slave make s =
	    (if !initialized then ()
	     else if make "host-cmb.cm" then initialized := true
	     else raise Fail "dynamic linkage for CMB slave failed";
	     CMBSlaveHook.slave s)
    end
end
