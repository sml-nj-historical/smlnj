structure BasicPrimTycNum : sig

    (* The numbers here are consecutive and fill [0...next_free_ptn) *)

    val ptn_void : int
    val ptn_int : int			(* default int (31 bit in SML/NJ) *)
    val ptn_real : int
    val ptn_string : int
    val ptn_exn : int
    val ptn_arrow : int
    val ptn_ref : int
    val ptn_array : int
    val ptn_vector : int

    val next_free_ptn : int

end = struct

    val ptn_void = 0
    val ptn_int = 1
    val ptn_real = 2
    val ptn_string = 3
    val ptn_exn = 4
    val ptn_arrow = 5
    val ptn_ref = 6
    val ptn_array = 7
    val ptn_vector = 8

    val next_free_ptn = 9
end
