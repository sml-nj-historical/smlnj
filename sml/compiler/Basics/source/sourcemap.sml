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

(* DBM: "Resynchonization" supports a model where the input stream for a compilation
 * unit is made up of multiple source files.  These may be combined either by
 * concatenation or inserting one file in the middle of another (like #include
 * in cpp).  We'll call the pieces of source that are being combined "file segments"
 * or "segments" for short.
 *
 * We'll assume that minimal granularity for resynchonization is a 
 * source line (i.e. no switching files in the middle of a line).
 * The boundaries between source file segments will be marked by #line
 * commands embedded in comments.  These have the form '(*#line nn "filename"*)'
 * or '(*#line nn*)' where nn is a new line number at which the next segment
 * starts, and filename is the name of the file for the next segment. It is
 * assumed that the #line command comment will appear on a line by itself
 * (presumably inserted by an external preprocessor that is responsible for
 * combining source segments to form the input stream).  These #line commands
 * are recognized and interpreted by the lexer, which calls resynch to change
 * the state of the current sourcemap.
 *
 * QUESTION: Can a source region, which designates a contiguous region in the
 * input stream, cross one or more file segments?  Or should all regions created
 * during parsing and used in elaboration be within a single segment (and therefore
 * be associated with a single source file)? The type and implementation of
 * function fileregion imply that a region can span multiple segments.
 * 
 * QUESTION: Presumably, the original motivation for adding this feature was to support
 * Norman Ramsey's nw "literate programming" system, which we no longer support.
 * What new clients require this functionality?
 *) 

structure SourceMap : SOURCE_MAP =
struct

  (* A character position is an integer.  A region is delimited by the
   * position of the start character and one beyond the end.
   * It might help to think of Icon-style positions, which fall between
   * characters.
   *)

  type charpos = int
    (* charpos is 1-based. I.e. the (default) position of the first character in the
     * input stream is 1 (????) *)

  type region = charpos * charpos
    (* INVARIANT: (lo,hi) : region ==> lo <= hi 
     * If region /= (0,0), then lo < hi, i.e. all non-null regions are nonempty. *)

  type sourceloc = {fileName:string, line:int, column:int}
    (* lines and columns are 1-based (minimum value is 1) *)

(* The representation of a sourcemap is a pair of lists.
     lines: line numbers for newlines and resynchronizations,
            labeled by initial charpos of lines
     files: file name for resynchronization, labeled by
            initial position for resynchronization

   The representation satisfies these invariants:
     * The lists are never empty (initialization is treated as a resynchronization).
     * Positions strictly decrease as we walk down the lists.
     * The last element in each list contains the smallest valid position.
     * For every element in files, there is a corresponding element in
       lines with the same position.

   We could get even more clever and store file names only when they
   differ, but it doesn't seem worth it---we would have to get very
   clever about tracking column numbers and resynchronizations.
*)

  datatype line
    = LINE of int   (* line number *)
    | SYNC of int * int   (* resynch point with line and column *)
    | FILE of int * int   (* file resynch point with line and column *)

  type sourcemap = {lines: (charpos * line)   list ref,
		    files: string list ref}
  (* INVARIANTS:
   * (1) length (!lines) > 0
   * (2) length (!files) > 0
   * (3) charpos components of lines are strictly decreasing
   * (4) length (!files) =< number of SYNC elements in lines
   *)

(*
  type sourcemap = {lines: (charpos * int)    list ref,
		    files: (charpos * string) list ref}
*)                  

  val nullRegion : region = (0,0)
  (* nullRegion is a conventional default region value.  It does not represent
   * a proper region, and does not have a location in the file. In particular, it
   * should not be viewed as an empty region at the beginning of the input. *)

  (* called only one place, in Source.newSource. pos argument is fixed as the
   * value of lexer_initial_position, which _should be_ 1 *)
  fun newmap (pos: charpos, fileName: string, line: int) : sourcemap =
      {files = ref [fileName], lines = ref [(pos, FILE(line,1))]}
(*
  fun newmap (pos: charpos, fileName: string, line: int) : sourcemap =
      {files = ref [(pos, fileName)], lines = ref [(pos, line)]}
*)

  (* ASSUMPTION: pos > last line position in the sourcemap argument *)
  fun resynch ({files, lines}: sourcemap) (pos, fileNameOp, line, column) =
      let val curFile = hd (!files)  (* current file name *)
      in  case fileNameOp
            of SOME f => 
	        (files := f :: !files;
		 lines := (pos, FSYNC(line,column)) :: !lines
	     | NONE => lines := (pos, SYNC(line,column)) :: !lines
      end

(*  (* ASSUMPTION: pos > last line position in the sourcemap argument *)
  fun resynch ({files, lines}: sourcemap) (pos, fileNameOp, line) =
      let val curFile = #2 (hd (!files))  (* current file name *)
      in  files := (pos, getOpt(fileNameOp, curFile)) :: !files;
	  lines := (pos, line) :: !lines
      end
*)
  (* Since pos is the position of the newline, the next line doesn't
   * start until the succeeding position, pos+1. *)
  fun lineNo (LINE l) = l
    | lineNo (SYNC(l,_) | FILE(l,_)) = l

  fun newline ({files, lines}: sourcemap) pos =
      case !lines
        of (line :: _) =>  lines := (pos+1, LINE(lineNo(line)+1)) :: !lines
         | nil => bug "newline"
(*
  fun newline ({files, lines}: sourcemap) pos =
      let val (_, line) = hd (!lines)
      in  lines := (pos+1, line+1) :: !lines
      end
*)
  fun lastLineStart({lines, ...}: sourcemap) =
      case lines
        of (line::_) => lineNo line
         | nil => bug "lastLineStart"

  (* A generally useful thing to do is to remove from the lists the
   * lines whose initial positions excede a target position. First
   * line of result contains the target position. *)

  fun remove pos ({files,lines}: sourcemap) =
      let fun strip (lines as (pos', line)::rest, files) = 
              if pos' > pos then 
                 (case line
                    of (LINE _ | SYNC _) => strip (rest, files)
		     | FILE(l,c) => strip(rest, tl files)) 
              else (lines, files)
	    | strip [] = []
       in strip(!lines, !files)
      end
(*
  fun remove p ({files,lines}: sourcemap) =
      let fun strip (l as (pos, _)::rest) = if p pos then strip  rest else l
	    | strip [] = []
       in (strip(!files), strip (!lines))
      end
*)
  (* We find file and line number by linear search.
   * The first position less than p is what we want.
   * The initial column depends on whether we resynchronized. *)

  fun column (lineStart, pos) = pos - lineStart + 1

  exception SourceMap_filepos

  fun filepos smap pos : sourceloc =
      case remove pos smap
        of ((_,file)::_, (pos',line)::_) => 
           {fileName = file, line = line, column = column(pos', p)}
         | _ => raise SourceMap_filepos
(*
  fun filepos smap p : sourceloc =
      case remove (fn pos : int => pos > p) smap
        of ((_,file)::_, (pos',line)::_) => 
           {fileName = file, line = line, column = column(pos', p)}
         | _ => raise SourceMap_filepos
*)
  (* Searching regions is a bit trickier, since we track file and line
   * simultaneously.  We exploit the invariant that every file entry has a
   * corresponding line entry.  We also exploit that only file entries
   * correspond to new regions. *)

  exception SourceMap_fileregion

  fun fileregion smap (lo, hi) =
      if (lo,hi) = nullRegion then [] else
      let fun gather(files as (filePos, file)::files', (linePos, line)::lines',
		     region_end, answers) =
	       if linePos <= lo then (* last item *)
		 ({fileName=file, line=line, column=column(linePos,lo)}, region_end)
                 :: answers
	       else if filePos < linePos then
		 gather(files, lines', region_end, answers)
	       else (* p = p'; new region *)
		 gather(files', lines', end_of (filePos, hd files', hd lines'), 
			({fileName = file, line = line, column = col}, region_end) :: answers)
	    | gather _ = raise SourceMap_fileregion
	  and end_of(lastpos, (filePos, file), (linePos, line)) = 
		 {fileName=file, line=line, column=column(linePos, lastpos)}
	  val (files, lines) = remove (fn pos : int => pos >= hi) smap
	  val _ = if null files orelse null lines then raise SourceMap_fileregion else ()
	  val answer = gather(files, lines, end_of(hi, hd files, hd lines), [])
	  fun validate(({fileName=f,  line=l,  column=c}:sourceloc, 
			{fileName=f', line=l', column=c'}) :: rest) =
		if f = f' andalso (l' > l orelse (l' = l andalso c' >= c)) then
		  validate rest 
		else 
		  raise SourceMap_fileregion
	    | validate [] = ()
	  (* validate checks the invariant that single "region segments" making
           * up the answer list occupy a single source file and that coordinates
           * are nondecreasing. We have to be careful not to remove the entry for
           * lo when pos = hi = lo. A redundant check if the algorithm is correct. *)
       in validate answer; answer
      end

   (* newlineCount : sourcemap -> region -> int 
    * determines the number of lines involved in a region, which may be 0
    * for a region that lies within a single line. Any lines containing
    * #line commands within the region are not counted. *)
   fun newlineCount smap ((lo, hi): region) =
      let val (hifiles, hilines) = remove (fn pos : int => pos >= hi) smap
	  val (lofiles, lolines) = remove (fn pos : int => pos > lo) smap
       in (length hilines - length lolines) - (length hifiles - length lofiles)
      end

end (* structure SourceMap *)

(* not used -- 

  fun span ((0,0), r) = r
    | span (r, (0,0)) = r
    | span ((l1, h1), (l2, h2)) = 
      (Int.min(l1,l2), Int.max(h1,h2))

  (one call in Parse/main/parser.sml commented out)
  fun forgetOldPositions ({files, lines} : sourcemap) =
      let val r as (p,  file, col) = hd (!files)
	  val l as (p', line)      = hd (!lines)
      in  lines := [l];
	  files := [if p = p' then r else (p', file, 1)]
      end

  fun positions({files,lines}: sourcemap) (src:sourceloc) =
      let exception Unimplemented
      in  raise Unimplemented
      end
*)
