(*
 * The public interface to CM's tools mechanism.
 *
 *   (C) 2000 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
signature TOOLS = sig
    (* We inherit most of this interface from CORETOOLS.  The only things
     * not in CORETOOLS are those that cannot be implemented without having
     * access to CM itself, and which would otherwise create a dependency
     * cycle. *)
    include CORETOOLS

    (* CM's say and vsay functions:  "say" unconditionally issues a
     * diagnostic message; "vsay" issues its message under control of
     * CM.Control.verbose. *)
    val say : string list -> unit
    val vsay : string list -> unit

    (* Get an anchor-configurable command name. *)
    val mkCmdName : string -> string

    (* Register a "standard" tool based on some shell command. *)
    val registerStdShellCmdTool : { tool: string,
				    class: string,
				    suffixes: string list,
				    cmdStdPath: string,
				    extensionStyle: extensionStyle,
				    template: string option,
				    dflopts: toolopts } -> unit

    (* query default class *)
    val defaultClassOf : string -> class option
end
