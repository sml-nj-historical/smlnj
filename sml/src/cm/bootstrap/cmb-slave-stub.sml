(*
 * This is a stub providing "slave" functionality for CMB.
 * (We use dynamic linking technology to avoid loading target-compilers.cm
 *  on the slave side unless it is really needed.)
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
structure CMBSlave = struct
    local
	val loaded = ref StringSet.empty
    in
	fun slave load arch s = let
	    val lib = arch ^ ".cm"
	in
	    if StringSet.member (!loaded, lib) then ()
	    else if load lib then loaded := StringSet.add (!loaded, lib)
	    else  raise Fail (concat ["dynamic linkage for CMB slave ",
				      arch, " failed"]);
	    CMBSlaveHook.slave arch s
	end
    end
end
