(* ii.sml
 *
 * (C) 2001 Lucent Technologies, Bell Labs
 *
 * Framework for passing inlining information around during elaboration.
 * (Uses the "exn-hack" to avoid being even more middle-end specific.)
 *)
structure II = struct
    datatype ii =
	Info of exn
      | List of ii list
      | Null

    local
	fun bug s = ErrorMsg.impossible ("II: " ^ s)
    in
    fun isSimple (Info _) = true
      | isSimple _ = false

    fun sel (List l, i) =
	(List.nth (l, i) handle Subscript => bug "Wrong field in II.List !")
      | sel (Null, _) = Null
      | sel (Info _, i) = bug "Unexpected selection from II.Info !"
    end
end
