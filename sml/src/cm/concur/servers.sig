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

    type server

    (* add a compile server *)
    val start : { name: string, cmd: string * string list,
		  pathtrans: (string -> string) option,
		  pref: int } -> server option

    val stop : server -> unit

    val kill : server -> unit

    val name : server -> string

    (* Reset scheduler and wait until all servers are idle.
     * The "bool" argument makes reset suitable as an argument to
     * SafeIO.perform. *)
    val reset : bool -> unit

    (* signal all servers that future cmb calls use a different dirbase *)
    val dirbase : string -> unit

    (* signal all servers that we have a new working dir *)
    val cd : string -> unit

    (* signal all servers that we are starting with a new .cm file *)
    val cm : { archos: string, project: string } -> unit

    (* signal all servers that we are starting with a new CMB.make *)
    val cmb : { archos: string, root: string } -> unit

    (* schedule a compilation *)
    val compile : string -> bool

    val withServers : (unit -> 'a) -> 'a
end
