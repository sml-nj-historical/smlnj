(* react.sml
 *
 * COPYRIGHT (c) 1997 AT&T Research Labs.
 * COPYRIGHT (c) 1997 Bell Labs, Lucent Technologies
 *
 * A simple ractive engine modelled after RC and SugarCubes.
 *)

structure React :> REACT =
  struct

    structure I = Instr
    structure C = Config

    type instruction = I.instruction
    type machine = I.machine

    datatype state = datatype I.state

  (* variables *)
    exception VarConflict of string
    datatype 'a var = V of {
	name : string,
	ts : I.instant ref,
	v  : 'a ref
      }
    fun newVar name v = V{name=name, ts=ref 0, v=ref v}
    fun get (V{ts, v, ...}) m = (ts := I.now m; !v)
    fun put (V{name, ts, v}, v') m = if (!ts = I.now m)
	  then raise VarConflict name
	  else (ts := I.now m; v := v')

  (* signals and configurations *)
    type config = C.config
    val posConfig = C.posConfig
    val negConfig = C.negConfig
    val orConfig = C.orConfig
    val andConfig = C.andConfig

  (* standard instruction methods *)
    type instr_state = {termFlg : bool ref}
    fun isTermMeth (s : instr_state) () = !(#termFlg s)
    fun terminateMeth (s : instr_state) () = (#termFlg s) := true
    fun resetMeth (s : instr_state) () = (#termFlg s) := false
    fun preemptMeth _ () = ()
    fun newState () = {termFlg=ref false}
    fun newInstr {reset, activation} = I.newInstruction {
	    isTerm = isTermMeth,
	    terminate = terminateMeth,
	    reset = reset,
	    preempt = preemptMeth,
	    activation = activation
	  } (newState())

    fun || (i1, i2) = let
	  val leftSts = ref SUSP
	  val rightSts = ref SUSP
	  fun resetMeth' s () = (resetMeth s (); I.reset i1; I.reset i2)
	  fun activationMeth _ m = (
		if (!leftSts = SUSP) then leftSts := I.activate(i1, m) else ();
		if (!rightSts = SUSP) then rightSts := I.activate(i2, m) else ();
		case (!leftSts, !rightSts)
		 of (TERM, TERM) => TERM
		  | (SUSP, _) => SUSP
		  | (_, SUSP) => SUSP
		  | _ => (leftSts := SUSP; rightSts := SUSP; STOP)
		(* end case *))
	  in
	    newInstr {reset = resetMeth', activation = activationMeth}
	  end

    fun & (i1, i2) = let
	  fun resetMeth' s () = (resetMeth s (); I.reset i1; I.reset i2)
	  fun activationMeth _ m =
		if (I.isTerm i1)
		  then I.activate(i2, m)
		else (case I.activate(i1, m)
		   of TERM => I.activate(i2, m)
		    | res => res
		  (* end case *))
	  in
	    newInstr {reset = resetMeth', activation = activationMeth}
	  end

    val nothing = I.newInstruction {
	    isTerm	= fn _ => fn () => true,
	    terminate	= fn _ => fn () => (),
	    reset	= fn _ => fn () => (),
	    preempt	= preemptMeth,
	    activation	= fn _ => fn _ => TERM
	  } ()

    fun stop () = newInstr {reset = resetMeth, activation = fn _ => fn _ => STOP}

    fun suspend () = newInstr {
	    reset = resetMeth,
	    activation = fn s => fn _ => (terminateMeth s (); STOP)
	  }

    fun action f = newInstr {
	    reset = resetMeth,
	    activation = fn _ => fn m => (f m; TERM)
	  }

    fun repeat (n, i) = let
	  val counter = ref n
	  fun resetMeth' s () = (resetMeth s (); counter := n)
	  fun activationMeth s m =
		if (!counter > 0)
		  then (case I.activate(i, m)
		     of TERM => (counter := !counter-1; I.reset i; TERM)
		      | res => res
		    (* end case *))
		  else TERM
	  in
	    newInstr {reset = resetMeth', activation = activationMeth}
	  end

    fun close i = let
	  fun activationMeth s m = (case I.activate(i, m)
		 of SUSP => activationMeth s m
		  | res => res
		(* end case *))
	  in
	    newInstr {reset = resetMeth, activation = activationMeth}
	  end

    fun loop i = let
	  val endReached = ref false
	  fun resetMeth' s () = (resetMeth s (); endReached := false)
	  fun activationMeth s m = (case I.activate(i, m)
		 of TERM => if (!endReached)
		      then (
			I.say(m, "instantaneous loop detected\n");
			STOP)
		      else (endReached := true; I.reset i; TERM)
		  | STOP => (endReached := false; STOP)
		  | SUSP => SUSP
		(* end case *))
	  in
	    newInstr {reset = resetMeth', activation = activationMeth}
	  end

    fun signal (name, i) = let
	  val name' = Atom.atom name
	  val state = ref 0
	  fun resetMeth' s () = (resetMeth s (); I.reset i; state := 0)
	  fun activationMeth s m = let
		val save = I.getSignal(m, name')
		in
		  I.putSignal(m, name', SOME state);
		  I.activate (i, m) before
		  I.putSignal(m, name', save)
		end
	  in
	    newInstr {reset = resetMeth', activation = activationMeth}
	  end

    fun emit name = let
	  val name' = Atom.atom name
	  fun activationMeth s m = (
		I.newMove m;
		I.emit(m, name');
		TERM)
	  in
	    newInstr {reset = resetMeth, activation = activationMeth}
	  end

    fun await c = let
	  fun activationMeth s m = (case C.fixedEval(m, c)
		 of NONE => SUSP
		  | (SOME true) => STOP
		  | (SOME false) => (
		      terminateMeth s ();
		      if (I.isEndOfInstant m) then STOP else TERM)
		(* end case *))
	  in
	    newInstr {reset = resetMeth, activation = activationMeth}
	  end

    fun when (c, i1, i2) = let
	  val value = ref NONE
	  fun resetMeth' s m = (
		resetMeth s m;
		I.reset i1; I.reset i2;
		value := NONE)
	  fun activationMeth' s m = (case !value
		 of NONE => (case C.fixedEval(m, c)
		       of NONE => SUSP
			| (SOME v) => (
			    value := SOME v;
			    if (I.isEndOfInstant m)
			      then STOP
			    else if v
			      then I.activate(i1, m)
			      else I.activate(i2, m))
		     (* end case *))
		  | (SOME true) => I.activate(i1, m)
		  | (SOME false) => I.activate(i2, m)
		(* end case *))
	  in
	    newInstr {reset = resetMeth', activation = activationMeth'}
	  end

    fun trapWith (c, i1, i2) = let
	  val activeHandle = ref false
	  val resumeBody = ref true
	  fun resetMeth' s m = (
		resetMeth s m;
		I.reset i1; I.reset i2;
		activeHandle := false;
		resumeBody := true)
	  fun activationMeth' s m =
		if (! activeHandle)
		  then I.activate(i2, m)
		  else let
		    fun chkConfig () = (case C.fixedEval(m, c)
			   of NONE => SUSP
			    | (SOME true) => ( (* actual preemption *)
				I.preempt i1;
				activeHandle := true;
				if (I.isEndOfInstant m)
				  then STOP
				  else I.activate(i2, m))
			    | (SOME false) => (
				resumeBody := true;
				STOP)
			  (* end case *))
		    in
		      if (! resumeBody)
			then (case I.activate(i1, m)
			   of STOP => (resumeBody := false; chkConfig())
			    | res => res
			  (* end case *))
			else chkConfig()
		    end
	  in
	    newInstr {reset = resetMeth', activation = activationMeth'}
	  end

    fun trap (c, i) = trapWith(c, i, nothing)

  end;
