structure TStamp = struct

    datatype t =
	NOTSTAMP
      | STABLETSTAMP of Time.time
      | TSTAMP of Time.time

    (*
     * If f1 depends on f2, then earlier (modtime f1, modtime f2) implies
     * that f1 needs to be recompiled...     *
     *)
    fun earlier (_, NOTSTAMP) = false	(* prerequisite missing *)
      | earlier (NOTSTAMP, _) = true	(* object missing *)
      | earlier (STABLETSTAMP _, _) = false (* object stable *)
      | earlier (TSTAMP t1, STABLETSTAMP t2) = Time.< (t1, t2)
      | earlier (TSTAMP t1, TSTAMP t2) = Time.< (t1, t2)
end
