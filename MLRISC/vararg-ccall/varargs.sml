(* varargs.sml
 *
 * COPYRIGHT (c) 2002 Bell Labs, Lucent Technologies
 *
 * This is an experiment to see if we can implement typed varargs
 * using combinators.
 *
 * call printf str o int o int : string -> int -> int -> unit
 * 
 *)

signature VAR_ARGS =
  sig

    type 'a valist
    type ('a, 'b) vararg = 'a valist -> ('b -> 'a) valist

    val int : ('a, int) vararg
    val real : ('a, real) vararg
    val bool : ('a, bool) vararg
    val str : ('a, string) vararg

    type 'a vararg_fn

    val call : ('a vararg_fn) -> ('a valist -> 'b valist) -> 'b

    val printf : unit vararg_fn

  end;

structure VarArgs :> VAR_ARGS =
  struct

  (* an evaluation engine that serves as a target *)
    datatype argument = datatype VarargCCall.argument

    fun arg2str (I i) = Int.toString i
      | arg2str (R r) = Real.toString r
      | arg2str (B b) = Bool.toString b
      | arg2str (S s) = concat["\"", String.toString s, "\""]

    val stk = ref([] : argument list)
    fun push arg = stk := arg :: !stk
    fun callWithArgs (cFun, x) = let
	  val args = !stk
	  in
	    stk := [];
	    IA32VarargCCall.callWithArgs (cFun, args);
	    x
	  end

    type 'a valist = ((unit -> unit) -> 'a)

    type ('a, 'b) vararg = 'a valist -> ('b -> 'a) valist

  (* combinators *)

    fun int k k' i = k(fn () => (push(I i); k'()))
    fun real k k' r = k(fn () => (push(R r); k'()))
    fun bool k k' b = k(fn () => (push(B b); k'()))
    fun str k k' s = k(fn () => (push(S s); k'()))

    fun call f spec = spec (fn k => (k(); callWithArgs f)) (fn () => ())

    type 'a vararg_fn = string * 'a

    val printf = ("printf", ())

  end
