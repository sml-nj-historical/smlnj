(* config.sml
 *
 * COPYRIGHT (c) 1997 AT&T Labs Research.
 * COPYRIGHT (c) 1997 Bell Labs, Lucent Technologies
 *
 * Signal configurations.
 *)

structure Config : sig

    type config
    val posConfig : string -> config
    val negConfig : string -> config
    val orConfig  : (config * config) -> config
    val andConfig : (config * config) -> config

    val fixed : (Instr.machine * config) -> bool
    val evaluate : (Instr.machine * config) -> bool

    val fixedEval : (Instr.machine * config) -> bool option
	(* this combines both fixed and evaluate into a single operation.
	 * It returns NONE, if the value is not fixed, and SOME v if the
	 * value is fixed to v.
	 *)

  end = struct

    structure I = Instr

    datatype config
      = SIG of Atom.atom
      | NOT of Atom.atom
      | OR of config * config
      | AND of config * config

    fun posConfig s = SIG(Atom.atom s)
    fun negConfig s = NOT(Atom.atom s)
    val orConfig = OR
    val andConfig = AND

    fun fixed (m, c) = let
	  fun fix (SIG id) = (I.presence(m, id) <> I.UNKNOWN)
	    | fix (NOT id) = (I.presence(m, id) <> I.UNKNOWN)
	    | fix (OR(c1, c2)) = let
		val b1 = fix c1 and b2 = fix c2
		in
		  (b1 andalso evaluate(m, c1)) orelse
		  (b2 andalso evaluate(m, c2)) orelse
		  (b1 andalso b2)
		end
	    | fix (AND(c1, c2)) = let
		val b1 = fix c1 and b2 = fix c2
		in
		  (b1 andalso not(evaluate(m, c1))) orelse
		  (b2 andalso not(evaluate(m, c2))) orelse
		  (b1 andalso b2)
		end
	  in
	    fix c
	  end

    and evaluate (m, c) = let
	  fun eval (SIG id) = I.present(m, id)
	    | eval (NOT id) = not(I.present(m, id))
	    | eval (OR(c1, c2)) = eval c1 orelse eval c2
	    | eval (AND(c1, c2)) = eval c1 andalso eval c2
	  in
	    eval c
	  end

    fun fixedEval (m, c) = let
	  fun f (SIG id) = (case I.presence(m, id)
		 of I.UNKNOWN => NONE
		  | I.PRESENT => SOME true
		  | I.ABSENT => SOME false
		(* end case *))
	    | f (NOT id) = (case I.presence(m, id)
		 of I.UNKNOWN => NONE
		  | I.PRESENT => SOME false
		  | I.ABSENT => SOME true
		(* end case *))
	    | f (AND(c1, c2)) = (case (f c1, f c2)
		 of (SOME false, _) => SOME false
		  | (_, SOME false) => SOME false
		  | (SOME true, SOME true) => SOME true
		  | _ => NONE
		(* end case *))
	    | f (OR(c1, c2)) = (case (f c1, f c2)
		 of (SOME true, _) => SOME true
		  | (_, SOME true) => SOME true
		  | (SOME false, SOME false) => SOME false
		  | _ => NONE
		(* end case *))
	  in
	    f c
	  end

  end;
