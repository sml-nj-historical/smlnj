(* instr.sml
 *
 * COPYRIGHT (c) 1997 AT&T Research Labs.
 * COPYRIGHT (c) 1997 Bell Labs, Lucent Technologies
 *
 * The basic instruction and machine types for the reactive engine.
 *)

structure Instr :> sig

    datatype state
      = TERM
      | STOP
      | SUSP

    type instant = int

    type instruction
    type machine

  (* instruction methods *)
    val isTerm : instruction -> bool
    val terminate : instruction -> unit
    val reset : instruction -> unit
    val preempt : instruction -> unit
    val activate : (instruction * machine) -> state

  (* "pre-methods" for instructions *)
    type 'a instruction_suite = {
	isTerm	   : 'a -> unit -> bool,
	terminate  : 'a -> unit -> unit,
	reset	   : 'a -> unit -> unit,
	preempt    : 'a -> unit -> unit,
	activation : 'a -> machine -> state
      }

    val newInstruction : 'a instruction_suite -> 'a -> instruction

  (* machine methods *)
    val now : machine -> instant
    val newMove : machine -> unit
    val isEndOfInstant : machine -> bool
    val runOnce : machine -> bool
    val run : machine -> unit
    val say : (machine * string) -> unit
    val newMachine : instruction -> machine

  (* signal methods *)
    type signal_state = instant ref
    datatype presence = PRESENT | ABSENT | UNKNOWN
    val getSignal : (machine * Atom.atom) -> signal_state option
    val putSignal : (machine * Atom.atom * signal_state option) -> unit
    val present : (machine * Atom.atom) -> bool
    val presence : (machine * Atom.atom) -> presence
    val emit : (machine * Atom.atom) -> unit

  end = struct

    datatype state
      = TERM
      | STOP
      | SUSP

    type instant = int
    type signal_state = instant ref

    datatype instruction = I of {
	isTerm : unit -> bool,
	terminate : unit -> unit,
	reset : unit -> unit,
	preempt : unit -> unit,
	activation : machine -> state
      }

    and machine = M of {
	now : instant ref,
	moveFlg : bool ref,
	endOfInstant : bool ref,
	prog : instruction,
	env : signal_state AtomTable.hash_table
      }

    fun terminate (I{terminate=f, ...}) = f()
    fun isTerm (I{isTerm=f, ...}) = f()
    fun reset (I{reset=f, ...}) = f()
    fun preempt (I{preempt=f, ...}) = f()
    fun activation (I{activation=f, ...}, m) = f m

  (* "pre-methods" for instructions *)
    type 'a instruction_suite = {
	isTerm	   : 'a -> unit -> bool,
	terminate  : 'a -> unit -> unit,
	reset	   : 'a -> unit -> unit,
	preempt    : 'a -> unit -> unit,
	activation : 'a -> machine -> state
      }

    fun newInstruction (suite : 'a instruction_suite) state = I{
	    isTerm	= #isTerm suite state,
	    terminate	= #terminate suite state,
	    reset	= #reset suite state,
	    preempt	= #preempt suite state,
	    activation	= #activation suite state
	  }

    fun activate (i, m) = if (isTerm i)
	  then TERM
	  else (case activation(i, m)
	     of TERM => (terminate i; TERM)
	      | res => res
	    (* end case *))

  (* machine methods *)
    fun now (M{now=t, ...}) = !t
    fun newMove (M{moveFlg, ...}) = moveFlg := true
    fun isEndOfInstant (M{endOfInstant, ...}) = !endOfInstant
    fun runOnce (m as M{now, moveFlg, endOfInstant, prog, ...}) = let
	  fun untilStop () = (case activate(prog, m)
		 of SUSP => (
		      if !moveFlg
			then moveFlg := false
			else endOfInstant := true;
		      untilStop ())
		  | STOP => false
		  | TERM => true
		(* end case *))
	  in
	    endOfInstant := false;
	    moveFlg := false;
	    untilStop () before now := !now+1
	  end
    fun run m = let
	  fun lp () = if (runOnce m) then () else lp()
	  in
	    lp ()
	  end
    fun resetMachine (M{now, moveFlg, endOfInstant, prog, env}) = (
(** what about variables? **)
	  now := 1;
	  moveFlg := false;
	  endOfInstant := false;
	  reset prog;
	  AtomTable.app (fn r => r := 0) env)
    fun say (m, s) = TextIO.print s
    fun newMachine prog = M{
	    now = ref 1,
	    moveFlg = ref false,
	    endOfInstant = ref false,
	    prog = prog,
	    env = AtomTable.mkTable (16, Fail "Machine env")
	  }

    fun getSignal (M{env, ...}, name) = AtomTable.find env name
    fun putSignal (M{env, ...}, name, SOME s) = AtomTable.insert env (name, s)
      | putSignal _ = ()
    fun present (M{env, now, ...}, name) = (!now = !(AtomTable.lookup env name))
    fun absent (M{env, now, ...}, name) = (!now = ~(!(AtomTable.lookup env name)))
    fun emit (M{env, now, ...}, name) = (AtomTable.lookup env name) := !now

    datatype presence = PRESENT | ABSENT | UNKNOWN

    fun presence (M{env, now, endOfInstant, ...}, name) = let
	  val ts = !(AtomTable.lookup env name)
	  val now = !now
	  in
	    if (now = ts) then PRESENT
	    else if ((now = ~ts) orelse !endOfInstant) then ABSENT
	    else UNKNOWN
	  end

  end;
