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

    structure S = IntRedBlackSet
    structure M = IntRedBlackMap

    datatype descr =
	STEP of int
      | LOOP of S.set

    type stage = { num: int, descr: descr }

    type frame = { depth: int, map: int M.map, stages: stage list }

    type history = frame * frame list

    val cur : history ref =
	ref ({ depth = 0, map = M.empty, stages = [] }, [])

    val names = ref (M.empty: string M.map)
    val next = ref 0

    fun reset () = (names := M.empty; next := 0)

    fun reserve n = !next before next := !next + n

    fun register (module, fct, s) =
	names := M.insert (!names, module + fct, s)

    fun add (module, fct) = let
	val i = module + fct
	val (front, back) = !cur
	val { depth, map, stages } = front
    in
	case M.find (map, i) of
	    SOME num => let
		fun toSet (STEP i) = S.singleton i
		  | toSet (LOOP s) = s
		fun join (set, d) = S.union (set, toSet d)
		fun finish (stages, c, []) =
		    let val stage = { num = num, descr = LOOP (toSet c) }
			val front' = { depth = depth,
				       map = map,
				       stages = stage :: stages }
		    in
			cur := (front', back)
		    end
		  | finish (stages, c, l) =
		    let val s0 = foldl S.union S.empty l
			val stage = { num = num, descr = LOOP (join (s0, c)) }
			fun ins (i, m) = M.insert (m, i, num)
			val front' = { depth = depth,
				       map = S.foldl ins map s0,
				       stages = stage :: stages }
		    in
			cur := (front', back)
		    end
		fun loop ([], setl) = finish ([], LOOP S.empty, setl)
		  | loop ({ num = n', descr = d' } :: t, setl) =
		    if num = n' then finish (t, d', setl)
		    else loop (t, toSet d' :: setl)
	    in
		loop (stages, [])
	    end
	  | NONE => let
		val num = case stages of
			      [] => 0
			    | s0 :: _ => #num s0 + 1
		val stage = { num = num, descr = STEP i}
		val front' = { depth = depth,
			       map = M.insert (map, i, num),
			       stages = stage :: stages }
	    in
		cur := (front' , back)
	    end
    end

    fun push () = let
	val old as (front, _) = !cur
	val front' = { depth = #depth front + 1, map = M.empty, stages = [] }
    in
	cur := (front', op :: old);
	fn () => cur := old
    end

    fun save () = let
	val old = !cur
    in
	fn () => cur := old
    end

    fun report () = let
	val (front, back) = !cur
	fun do_report () = let
	    val (front', _) = !cur
	    val bot_depth = #depth front'
	    fun isBot (f: frame) = #depth f = bot_depth
	    fun name (w, pad, i) = let
		val n = getOpt (M.find (!names, i), "???")
	    in
		concat [w, pad, " ", n, "\n"]
	    end
	    fun stage (w, { num, descr = STEP i }, a) = name (w, "  ", i) :: a
	      | stage (w, { num, descr = LOOP s }, a) = let
		    fun loop ([], a) = a
		      | loop ([i], a) = name (w, "-\\", i) :: a
		      | loop (h :: t, a) =
			loop (t, name ("    ", " |", h) :: a)
		    fun start ([], a) = a
		      | start ([i], a) = name (w, "-(", i) :: a
		      | start (h :: t, a) =
			loop (t, name ("    ", " /", h) :: a)
		in
		    start (S.listItems s, a)
		end
	    fun jumps ([], a) = a
	      | jumps ([n], a) = stage ("CALL", n, a)
	      | jumps (h :: t, a) = jumps (t, stage ("GOTO", h, a))
	    fun calls (h, [], a) = jumps (#stages h, a)
	      | calls (h, h' :: t, a) = let
		    val a = jumps (#stages h, a)
		in
		    if isBot h then a else calls (h', t, a)
		end
	in
	    rev (calls (front, back, []))
	end
    in
	do_report
    end

    fun install () =
	SMLofNJ.Internals.BTrace.install
	    { corefns = { save = save,
			  push = push,
			  add = add,
			  reserve = reserve,
			  register = register,
			  report = report },
	      reset = reset }

    val _ = install ()
end
