(*
 * The hook module for the dynamically-linked CMB "slave" stub.
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
structure CMBSlaveHook = struct
    local
	type res =
	    GroupGraph.group * GeneralParams.info *
	    (DependencyGraph.sbnode -> bool)
	fun placeholder (s: string) = (NONE: res option)
	val r = ref placeholder
    in
	fun init f = r := f
	fun slave s = !r s
    end
end
