(*
 * Handling compile-servers.
 *
 *  This is still rather crude and not very robust.  A "real" implementation
 *  exists only for Unix.
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
signature SERVERS = sig

    (* add a compile server *)
    val start : { name: string, cmd: string * string list,
		  pathtrans: (string -> string) option,
		  pref: int } -> bool

    val stop : string -> unit

    val kill : string -> unit

    (* reset scheduler and wait until all servers are idle *)
    val reset : unit -> unit

    (* signal all servers that future cmb calls use a different dirbase *)
    val dirbase : string -> unit

    (* signal all servers that we are starting with a new .cm file *)
    val cm : SrcPath.t -> unit

    (* signal all servers that we are starting with a new CMB.make *)
    val cmb : { archos: string, root: SrcPath.t } -> unit

    (* schedule a compilation *)
    val compile : SrcPath.t -> bool

    val withServers : (unit -> 'a) -> 'a
end
