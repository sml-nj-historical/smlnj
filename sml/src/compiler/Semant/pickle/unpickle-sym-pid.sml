structure UnpickleSymPid :> sig
    val r_symbol : UnpickleUtil.session * string UnpickleUtil.reader ->
	Symbol.symbol UnpickleUtil.reader
    val r_pid : string UnpickleUtil.reader ->
	PersStamps.persstamp UnpickleUtil.reader
end = struct
    fun r_symbol (session, r_string) = let
	val m = UnpickleUtil.mkMap ()
	fun s () = let
	    fun sym con = con (r_string ())
	    fun rs  #"a" = sym Symbol.varSymbol
	      | rs #"b" = sym Symbol.tycSymbol
	      | rs #"c" = sym Symbol.sigSymbol
	      | rs #"d" = sym Symbol.strSymbol
	      | rs #"e" = sym Symbol.fctSymbol
	      | rs #"f" = sym Symbol.fsigSymbol
	      | rs #"g" = sym Symbol.fixSymbol
	      | rs #"h" = sym Symbol.labSymbol
	      | rs #"i" = sym Symbol.tyvSymbol
	      | rs _ = raise UnpickleUtil.Format
	in
	    UnpickleUtil.share session m rs
	end
    in
	s
    end

    fun r_pid r_string () =
	PersStamps.fromBytes (Byte.stringToBytes (r_string ()))
end

