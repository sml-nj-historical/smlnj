(* absynutil.sml
 *
 * (C) 2001 Lucent Technologies, Bell Labs
 *
 * More stuff from ElabUtil should be moved here eventually.
 *)
structure AbsynUtil : sig

    val unitExp : Absyn.exp

    val TUPLEexp : Absyn.exp list -> Absyn.exp
    val TUPLEpat : Absyn.pat list -> Absyn.pat

end = struct

    val unitExp = Absyn.RECORDexp []

    fun TUPLEexp l = let
	fun build (_, []) = []
	  | build (i, e :: es) =
	    (Absyn.LABEL { number = i-1, name = Tuples.numlabel i }, e)
	    :: build (i+1, es)
    in
	Absyn.RECORDexp (build (1, l))
    end

    fun TUPLEpat l = let
	fun build (_, []) = []
	  | build (i, e :: es) = (Tuples.numlabel i, e) :: build (i+1, es)
    in
	Absyn.RECORDpat { fields = build (1, l), flex = false,
			  typ = ref Types.UNDEFty }
    end
end
