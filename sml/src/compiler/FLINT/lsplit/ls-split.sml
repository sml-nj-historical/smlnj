signature LSPLIT = sig

    type flint = CompBasic.flint

    val split: flint -> flint * flint option
end

structure LSplit :> LSPLIT = struct

    type flint = CompBasic.flint

    fun bug s = ErrorMsg.impossible ("LSplit: " ^ s)

    fun split (mFkind, mLvar, [(mArgLvar, mArgLty)], mBody) =
	let
	in
	    raise Fail "notyet"
	end
      | split _ = bug "bad comp-unit argument list"

end
