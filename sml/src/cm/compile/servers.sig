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
		  pathtrans: (string -> string) option } -> bool

    val stop : string -> unit

    val kill : string -> unit

    (* reset scheduler and wait until all servers are idle *)
    val reset : unit -> unit

    (* signal all servers that we are starting with a new .cm file *)
    val cm : SrcPath.t -> unit

    (* signal all servers that we are starting with a new CMB.make *)
    val cmb : string -> unit

    (* schedule a compilation *)
    val compile : SrcPath.t -> bool

    val evict : SmlInfo.info -> unit

    val withServers : (unit -> 'a) -> 'a
end
