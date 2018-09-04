(*
 * Make structure _Core available to members of the init group.
 * We write "xCore", but the bootstrap compiler will replace the binding
 * of "xCore" with a binding to "_Core". See init.cmi for more details.
 *
 * (C) 2000 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
structure xCore = Core
