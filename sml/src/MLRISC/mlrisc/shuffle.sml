(* shuffle.sml -- implements the parallel copy instruction as a sequence
 *		of moves. Makes use of asmTmpR from CELLS.
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 *)


functor Shuffle(I : INSTRUCTIONS) :
  sig
    val shuffle : 
      {mvInstr : {dst:I.ea, src:I.ea} -> I.instruction list,
       ea : int -> I.ea} 
      ->
	{regMap: int -> int,
	 temp : I.ea option,
	 dst : int list,
	 src : int list} 
	-> I.instruction list
  end = 
struct
  datatype reg = REG of int | TEMP
  fun equal (REG r1, REG r2) = r1 = r2
    | equal (TEMP, TEMP) = true
    | equal _ = false

  fun shuffle{mvInstr, ea} {regMap, temp, dst, src} = let
    fun opnd (REG dst) = ea dst
      | opnd TEMP = Option.valOf temp

    (* perform unconstrained moves *)
    fun loop((p as (rd,rs))::rest, changed, used, done, instrs) = 
	if List.exists (fn r => equal(r, rd)) used then
	   loop(rest, changed, used, p::done, instrs)
	else loop(rest, true, used, done, mvInstr{dst=opnd rd, src=opnd rs}@instrs)
      | loop([], changed, _, done, instrs) = (changed, done, instrs)

    fun cycle([], instrs) = instrs
      | cycle(moves, instrs) =
	(case loop(moves, false, map #2 moves, [], instrs)
	  of (_, [], instrs) => instrs
	   | (true, acc, instrs) => cycle(acc, instrs)
	   | (false, (rd,rs)::acc, instrs) => let
	       fun rename(p as (a,b)) = if equal(rd, b) then (a, TEMP) else p
	       val acc' = (rd, rs) :: map rename acc
	       val instrs' = mvInstr{dst=Option.valOf temp, src=opnd rd}@instrs
	       val (_, acc'', instrs'') = 
		 loop(acc', false, map #2 acc', [], instrs')
	     in cycle(acc'', instrs'')
	     end
	 (*esac*))

    (* remove moves that have been coalesced. *)
    fun rmvCoalesced(rd::rds, rs::rss) = let
	  val dst = regMap rd
	  val src = regMap rs
	in
	  if dst = src then rmvCoalesced(rds, rss)
	  else (REG dst, REG src)::rmvCoalesced(rds, rss)
	end
      | rmvCoalesced([], []) = []
  in rev (cycle (rmvCoalesced(dst, src), []))
  end
end

(*
 * $Log: shuffle.sml,v $
 * Revision 1.3  1998/02/16 13:58:21  george
 *   A register allocated temp is now associated with parallel COPYs
 *   instead of a dedicated register. The temp is used to break cycles.
 *
 * Revision 1.2  1997/07/03 13:55:56  george
 *   The shuffle routine now takes a temporary register so that it can be
 *   used for both general purpose and floating point shuffling.
 *
# Revision 1.1.1.1  1997/04/19  18:14:21  george
#   Version 109.27
#
 *)
