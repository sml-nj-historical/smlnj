(* ieee-real.sml
 *
 * COPYRIGHT (c) 1996 AT&T Bell Laboratories.
 *)

structure IEEEReal : IEEE_REAL =
  struct

  (* this may cause portability problems to 64-bit systems *)
    structure Int = Int31

    exception Unordered

    datatype real_order = LESS | EQUAL | GREATER | UNORDERED

    datatype nan_mode = QUIET | SIGNALLING

    datatype float_class
      = NAN of nan_mode
      | INF
      | ZERO
      | NORMAL
      | SUBNORMAL

    datatype rounding_mode
      = TO_NEAREST
      | TO_NEGINF
      | TO_POSINF
      | TO_ZERO

    (* dbm: using original codes for rounding modes, not the ones used
     *  in SMLBasis *)
    fun intToRM (0: Int32.int) = TO_NEAREST
      | intToRM 1 = TO_ZERO
      | intToRM 2 = TO_POSINF
      | intToRM 3 = TO_NEGINF
      | intToRM _ = raise Match (* shut up compiler *)

    fun setRoundingMode TO_NEAREST	= SMLBasis.setRoundingMode 0
      | setRoundingMode TO_ZERO		= SMLBasis.setRoundingMode 1
      | setRoundingMode TO_POSINF	= SMLBasis.setRoundingMode 2
      | setRoundingMode TO_NEGINF	= SMLBasis.setRoundingMode 3

    fun getRoundingMode () = intToRM (SMLBasis.getRoundingMode ())

    type decimal_approx = {
	kind : float_class,
	sign : bool,
	digits : int list,
	exp : int
      }

    fun toString {kind, sign, digits, exp} = let
	  fun fmtExp 0 = []
	    | fmtExp i = ["E", IntImp.toString i]
	  fun fmtDigits ([], tail) = tail
	    | fmtDigits (d::r, tail) =
	      (IntImp.toString d) :: fmtDigits(r, tail)
	  in
	    case (sign, kind, digits)
	     of (true, ZERO, _) => "~0.0"
	      | (false, ZERO, _) => "0.0"
	      | (true, (NORMAL|SUBNORMAL), []) => "~0.0"
	      | (false, (NORMAL|SUBNORMAL), []) => "0.0"
	      | (true, (NORMAL|SUBNORMAL), _) =>
		  StringImp.concat("~0." :: fmtDigits(digits, fmtExp exp))
	      | (false, (NORMAL|SUBNORMAL), _) =>
		  StringImp.concat("0." :: fmtDigits(digits, fmtExp exp))
	      | (true, INF, _) => "~inf"
	      | (false, INF, _) => "inf"
	      | (_, NAN _, []) => "nan"
	      | (_, NAN _, _) =>
		  StringImp.concat("nan(" :: fmtDigits(digits, [")"]))
	    (* end case *)
	  end

(** TODO: implement fromString **)
    fun fromString s = NONE

  end;


