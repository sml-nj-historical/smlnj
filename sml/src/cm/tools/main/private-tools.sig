(*
 * A private interface to CM's tools mechanism to be used internally
 * by CM itself.
 *
 *   (C) 2000 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
signature PRIVATETOOLS = sig
    include CORETOOLS where type srcpath = SrcPath.file
		      where type presrcpath = SrcPath.prefile

    type registry

    val newRegistry : unit -> registry

    val expand : { error: string -> unit,
		   local_registry : registry,
		   spec: spec,
		   context: SrcPath.dir,
		   load_plugin: SrcPath.dir -> string -> bool }
	-> expansion

    val withPlugin : SrcPath.file -> (unit -> 'a) -> 'a
end
