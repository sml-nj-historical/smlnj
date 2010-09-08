(* sourcemap.sml *)

(* Norman Ramsey: *)
(* I can imagine at least three implementations: one that doesn't
 * support resynchronization, one that supports resynchronization only at
 * column 1, and one that supports arbitrary resynchronization.
 *
 * This implementation supports arbitary resynchronization.
 *
 * Changed ErrorMsg to use SourceMap to get source locations; only the
 * formatting is done internally.
 *
 * Added SourceMap structure.
 *)

(* DBM: what is "resynchronization" and what is it used for?  Is there any
 * reason to continue to support it (and maintain the extra code complexity)?
 * If this was a feature used only by Ramsey's noweb utility, which is defunct,
 * then we could simplify the sourcemap code.  -- John claims that resynchronization
 * is still relevant (examples?). *)

structure SourceMap : SOURCE_MAP =
struct

  (* A character position is an integer.  A region is delimited by the
   * position of the start character and one beyond the end.
   * It might help to think of Icon-style positions, which fall between
   * characters.
   *)

  type charpos = int
  (* charpos is 1-based. I.e. the position of the first character in the
   * input is 1 (????) *)

  type region = charpos * charpos
    (* INVARIANT: (lo,hi) : region ==> lo <= hi *)

  type sourceloc = {fileName:string, line:int, column:int}
  (* lines and columns are 1-based (minimum value is 1) *)

(* The representation of a sourcemap is a pair of lists.
     linePos:    line numbers for newlines and resynchronizations,
                 labeled by initial positions of lines
     resynchPos: file name for resynchronization, labeled by
                 initial position for resynchronization

   The representation satisfies these invariants:
     * The lists are never empty (initialization is treated as a resynchronization).
     * Positions strictly decrease as we walk down the lists.
     * The last element in each list contains the smallest valid position.
     * For every element in resynchPos, there is a corresponding element in
       linePos with the same position.

   We could get even more clever and store file names only when they
   differ, but it doesn't seem worth it---we would have to get very
   clever about tracking column numbers and resynchronizations.
*)

  type sourcemap = {linePos:    (charpos * int)    list ref,
		    resynchPos: (charpos * string) list ref}
                    

  val nullRegion : region = (0,0)
  (* nullRegion is a conventional default region value.  It does not represent
   * a proper region, and does not have a location in the file. In particular, it
   * should not be viewed as an empty region at the beginning of the input. *)

  (* called only one place, in Source.newSource. pos argument is fixed as the
   * value of lexer_initial_position, which _should be_ 1 *)
  fun newmap (pos: charpos, fileName: string, line: int) : sourcemap =
      {resynchPos = ref [(pos, fileName)], linePos = ref [(pos, line)]}

  (* ASSUMPTION: pos > last line position in the sourcemap argument *)
  fun resynch ({resynchPos, linePos}: sourcemap) (pos, fileNameOp, line) =
      let val curFile = #2 (hd (!resynchPos))  (* current file name *)
      in  resynchPos := (pos, getOpt(fileNameOp, curFile)) :: !resynchPos;
	  linePos := (pos, line) :: !linePos
      end

  (* Since pos is the position of the newline, the next line doesn't
   * start until the succeeding position, pos+1. *)
  fun newline ({resynchPos, linePos}: sourcemap) pos =
      let val (_, line) = hd (!linePos)
      in  linePos := (pos+1, line+1) :: !linePos
      end

  fun lastLineStart({linePos, ...}: sourcemap) = #1 (hd (!linePos))

  (* A generally useful thing to do is to remove from the lists the initial
   * sequences of tuples whose positions satisfy some predicate: *)

  fun remove p ({resynchPos,linePos}: sourcemap) =
      let fun strip (l as (pos, _)::rest) = if p pos then strip  rest else l
	    | strip [] = []
       in (strip(!resynchPos), strip (!linePos))
      end

  (* We find file and line number by linear search.
   * The first position less than p is what we want.
   * The initial column depends on whether we resynchronized. *)

  fun column ((pos, file), (pos', line), p) = p - pos' + 1

  fun filepos smap p : sourceloc =
      case remove (fn pos : int => pos > p) smap
        of ((_,file)::_, (pos',line)::_) => 
           {fileName = file, line = line, column = p - pos' + 1
         | _ => bug "SourceMap.filepos"

  (* Searching regions is a bit trickier, since we track file and line
   * simultaneously.  We exploit the invariant that every file entry has a
   * corresponding line entry.  We also exploit that only file entries
   * correspond to new regions. *)

  exception Impossible
  fun fileregion smap (lo, hi) =
      if (lo,hi) = nullRegion then [] else
      let fun gather((p, file, col)::files, (p', line)::lines, region_end, answers) =
	       if p' <= lo then (* last item *)
		 ({fileName=file, line=line, column=column((p, file, col), (p', line), lo)}, 
		  region_end) :: answers
	       else if p < p' then
		   gather((p, file, col)::files, lines, region_end, answers)
	       else (* p = p'; new region *)
		   gather(files, lines, end_of (p, hd files, hd lines), 
			({fileName = file, line = line, column = col}, region_end) :: answers)
	    | gather _ = raise Impossible
	  and end_of(lastpos, xx as (p, file, col), yy as (p', line)) = 
		 {fileName=file, line=line, column=column(xx, yy, lastpos)}
	  val (files, lines) = remove (fn pos : int => pos >= hi andalso pos > lo) smap
	  val _ = if null files orelse null lines then raise Impossible else ()
	  val answer = gather(files, lines, end_of(hi, hd files, hd lines), [])
	  fun validate(({fileName=f,  line=l,  column=c}:sourceloc, 
			{fileName=f', line=l', column=c'}) :: rest) = 
		if f = f' andalso (l' > l orelse (l' = l andalso c' >= c)) then
		  validate rest 
		else 
		  raise Impossible
	    | validate [] = ()
	  (* validate checks the invariant that single regions occupy a
	   * single source file and that coordinates are nondecreasing.
	   * We have to be careful not to remove the entry for lo when
	   * pos = hi = lo. *)
       in validate answer; answer
      end

  fun newlineCount smap ((lo, hi): region) =
      let val (hifiles, hilines) = remove (fn pos : int => pos >= hi andalso pos > lo) smap
	  val (lofiles, lolines) = remove (fn pos : int =>                   pos > lo) smap
      in  length hilines - length hifiles - (length lolines - length lofiles)
      end

end (* structure SourceMap *)

(* not used -- 

  fun span ((0,0), r) = r
    | span (r, (0,0)) = r
    | span ((l1, h1), (l2, h2)) = 
      (Int.min(l1,l2), Int.max(h1,h2))

  (one call in Parse/main/parser.sml commented out)
  fun forgetOldPositions ({resynchPos, linePos} : sourcemap) =
      let val r as (p,  file, col) = hd (!resynchPos)
	  val l as (p', line)      = hd (!linePos)
      in  linePos := [l];
	  resynchPos := [if p = p' then r else (p', file, 1)]
      end

  fun positions({resynchPos,linePos}: sourcemap) (src:sourceloc) =
      let exception Unimplemented
      in  raise Unimplemented
      end
*)
