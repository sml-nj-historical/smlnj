(* bin-prim-io.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 * COPYRIGHT (c) 2001 Lucent Technologies, Bell Labs
 *
 *)
structure BinPrimIO : BIN_PRIM_IO = struct

    local
	structure GenericPrimIO =
	GenericPrimIO (structure Vector = Word8Vector
		       structure Array = Word8Array
		       structure VectorSlice = Word8VectorSlice
		       structure ArraySlice = Word8ArraySlice
		       val someElem = (0w0 : Word8.word)
		       type pos = Position.int
		       val compare = PositionImp.compare)
    in
        open GenericPrimIO
    end

    (* more to come here *)
end
