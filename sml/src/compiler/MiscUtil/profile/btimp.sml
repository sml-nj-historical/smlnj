(*
 * (Sample) Implementation of a plug-in module for back-tracing.
 * This module hooks itself into the core environment so that
 * btrace-annotated (see btrace.sml) code will invoke the provided
 * functions "add", "push", "save", and "report".
 *
 * This module keeps track of the dynamic call-chain of annotated modules
 * (those that were compiled with SMLofNJ.Internals.BTrace.mode set to true).
 * Non-tail calls are maintained in a stack-like fashion, and in addition
 * to this the module will also track tail-calls so that a sequence of
 * GOTO-like jumps from loop-cluster to loop-cluster can be shown.
 *
 * This strategy, while certainly costly, has no more than constant-factor
 * overhead in space and time and will keep tail-recursive code tail-recursive.
 *
 *   Copyright (c) 2000 by Lucent Bell Laboratories
 *
 * author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
structure BTImp : sig
end = struct

    exception NotFound

    structure HT = IntHashTable
    structure IS = IntRedBlackSet
    structure IM = IntRedBlackMap
    structure SM = RedBlackMapFn (struct
	type ord_key = string val compare = String.compare
    end)

    type intset = IS.set

    datatype contents =
	SINGLE of int
      | CLUSTER of intset

    type stamp = unit ref

    type node = stamp * contents

    type htable = node HT.hash_table

    type stage = htable * node list ref

    type history = stage list

    val s2i_m = ref (SM.empty: int SM.map)
    val i2s_m = ref (IM.empty: string IM.map)
    val next = ref 0

    fun reset () = (s2i_m := SM.empty; i2s_m := IM.empty; next := 0)

    fun mkid s =
	case SM.find (!s2i_m, s) of
	    SOME i => i
	  | NONE => let
		val i = !next
	    in
		next := i + 1;
		s2i_m := SM.insert (!s2i_m, s, i);
		i
	    end

    fun register (i, s) = let
	fun insert () = i2s_m := IM.insert (!i2s_m, i, s)
    in
	case IM.find (!i2s_m, i) of
	    NONE => insert ()
	  | SOME s' =>
	    if s = s' then ()
	    else (print (concat ["BTrace: register: id clash between\n\t", s',
				 "\nand\n\t", s, ";\nusing latter.\n"]);
		  insert ())
    end

    fun new_ht () = HT.mkTable (16, NotFound)

    val cur = ref ([]: history)

    fun add i =
	case !cur of
	    [] => ()
	  | (ht, nlr) :: _ =>
	    (case HT.find ht i of
		 SOME (s, c) => let
		     fun toSet (SINGLE i) = IS.singleton i
		       | toSet (CLUSTER s) = s
		     fun join (set, c) = IS.union (set, toSet c)
		     fun finish (l, set) = let
			 val n = (s, CLUSTER set)
		     in
			 nlr := n :: l;
			 IS.app (fn i => HT.insert ht (i, n)) set
		     end
		     fun loop ([], set) = finish ([], set)
		       | loop ((s', c) :: t, set) =
			 if s = s' then finish (t, set)
			 else loop (t, join (set, c))
		 in
		     loop (!nlr, toSet c)
		 end
	       | NONE => let
		     val n = (ref (), SINGLE i)
		     val l = n :: !nlr
		 in
		     HT.insert ht (i, n);
		     nlr := l
		 end)

    fun push () = let
	val old = !cur
    in
	cur := (new_ht (), ref []) :: old;
	fn () => cur := old
    end

    fun save () = let
	val old = !cur
    in
	fn () => cur := old
    end

    fun report () = let
	val top = !cur
	fun do_report () = let
	    val bot = !cur
	    val isBot =
		case bot of
		    [] => (fn _ => false)
		  | (_, bot_nlr) :: _ => (fn nlr => bot_nlr = nlr)
	    fun name (what, pad, i) = let
		val n = case IM.find (!i2s_m, i) of
			    NONE => "???"
			  | SOME s => s
	    in
		concat [what, pad, " ", n, "\n"]
	    end
	    fun node (what, (_, SINGLE i), a) = name (what, "  ", i) :: a
	      | node (what, (_, CLUSTER s), a) = let
		    fun loop ([], a) = a
		      | loop ([i], a) = name (what, "-\\", i) :: a
		      | loop (h :: t, a) =
			loop (t, name ("    ", " |", h) :: a)
		    fun looph ([], a) = a
		      | looph ([i], a) = name (what, "-(", i) :: a
		      | looph (h :: t, a) =
			loop (t, name ("    ", " /", h) :: a)
		in
		    looph (IS.listItems s, a)
		end
	    fun jumps ([], a) = a
	      | jumps ([n], a) = node ("CALL", n, a)
	      | jumps (h :: t, a) = jumps (t, node ("GOTO", h, a))
	    fun calls ([], a) = a
	      | calls ((_, nlr as ref nl) :: t, a) = let
		    val a = jumps (nl, a)
		in
		    if isBot nlr then a else calls (t, a)
		end
	in
	    rev (calls (top, []))
	end
    in
	do_report
    end

    fun install () =
	SMLofNJ.Internals.BTrace.install
	    { corefns = { save = save,
			  push = push,
			  add = add,
			  register = register,
			  report = report },
	      reset = reset,
	      mkid = mkid }

    val _ = install ()
end
