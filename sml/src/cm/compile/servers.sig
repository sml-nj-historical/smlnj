signature SERVERS = sig

    (* add a compile server *)
    val add : { name: string, cmd: string * string list } -> unit

    (* signal all servers that we are starting with a new .cm file *)
    val start : SrcPath.context * SrcPath.t -> unit

    (* schedule a compilation *)
    val compile : SrcPath.t -> bool
end
