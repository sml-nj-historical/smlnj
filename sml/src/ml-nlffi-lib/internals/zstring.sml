(* dealing with C's 0-terminated strings *)
structure ZString : sig

    type 'c zstring = (C.uchar, unit, 'c) C.ptr
    type 'c zstring' = (C.uchar, unit, 'c) C.ptr'

    (* the C strlen function *)
    val length : 'c zstring -> int
    val length' : 'c zstring' -> int

    (* make ML string from 0-terminated C string *)
    val toML : 'c zstring -> string
    val toML' : 'c zstring' -> string

    (* Copy contents of ML string into C string and add terminating 0. *)
    val cpML : { from: string, to: C.rw zstring } -> unit
    val cpML' : { from: string, to: C.rw zstring' } -> unit

    (* Make C-duplicate of ML string (allocate memory and then copy). *)
    val dupML : string -> C.rw zstring option
    val dupML' : string -> C.rw zstring' option
end = struct

    local
	open C
	fun get' p = Cvt.ml_uchar (Get.uchar' (Ptr.|*! p))
	fun set' (p, w) = Set.uchar' (Ptr.|*! p, Cvt.c_uchar w)
	fun nxt' p = Ptr.|+! S.uchar (p, 1)
    in
        type 'c zstring = (uchar, unit, 'c) ptr
	type 'c zstring' = (uchar, unit, 'c) ptr'

	fun length' p = let
	    fun loop (n, p) = if get' p = 0w0 then n else loop (n + 1, nxt' p)
	in
	    loop (0, p)
	end
	fun length p = length' (Light.ptr p)

	fun toML' p = let
	    fun loop (l, p) =
		case get' p of
		    0w0 => String.implode (rev l)
		  | c => loop (Char.chr (Word32.toInt c) :: l, nxt' p)
	in
	    loop ([], p)
	end
	fun toML p = toML' (Light.ptr p)

	fun cpML' { from, to } = let
	    val n = String.size from
	    fun loop (i, p) =
		if i >= n then set' (p, 0w0)
		else (set' (p, Word32.fromInt (Char.ord
						   (String.sub (from, i))));
		      loop (i+1, nxt' p))
	in
	    loop (0, to)
	end
	fun cpML { from, to } = cpML' { from = from, to = Light.ptr to }

	fun dupML' s =
	    Option.map (fn z => (cpML' { from = s, to = z }; z))
		       (C.alloc'' C.S.uchar (Word.fromInt (size s + 1)))
	fun dupML s =
	    Option.map (fn z => (cpML { from = s, to = z }; z))
		       (C.alloc C.T.uchar (Word.fromInt (size s + 1)))
    end
end
