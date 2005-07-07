(* ansi-term-dev.sml
 *
 * COPYRIGHT (c) 2005 John Reppy (http://www.cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * A pretty-printing device for text output to ANSI terminals.  This device
 * supports the standard ANSI output attributes.
 *)

structure ANSITermDev : sig

    include PP_DEVICE
      where type style = ANSITerm.style list

  (* create an output device; if the underlying stream is connected to a TTY,
   * then styled output is enabled, otherwise it will be disabled.
   *)
    val openDev : {dst : TextIO.outstream, wid : int} -> device

  (* enable styled output by passing true to this function.  It returns
   * the previous state of the device.
   * NOTE: this function raises Fail if called while a style is active.
   *)
    val enableStyles : (device * bool) -> bool

  end = struct

    structure A = ANSITerm

    type state = {
	fg : A.color option,	(* NONE is default color for terminal *)
	bg : A.color option,	(* NONE is default color for terminal *)
	bold : bool,
	blink : bool,
	ul : bool,
	rev : bool,
	invis : bool
      }

    val initState = {
	  fg=NONE, bg=NONE,
	  bold=false, blink=false, ul=false, rev=false, invis=false
	}

  (* compute the commands to transition from one state to another *)
    fun transition (s1 : state, s2 : state) = let
	  fun needsColorReset proj = (case (proj s1, proj s2)
		 of (SOME _, NONE) => true
		  | _ => false
		(* end case *))
	  fun needsReset proj = (case (proj s1, proj s2)
		 of (true, false) => true
		  | _ => false
		(* end case *))
	(* does the state transition require reseting the attributes first? *)
	  val reset = (needsColorReset #fg orelse needsColorReset #bg
		orelse needsReset #bold orelse needsReset #blink
		orelse needsReset #ul orelse needsReset #rev
		orelse needsReset #invis)
	(* compute the commands to set the foreground color *)
	  val mv = (case (reset, #fg s1, #fg s2)
		 of (false, SOME c1, SOME c2) => if c1 = c2 then [] else [A.FG c2]
		  | (_, _, SOME c) => [A.FG c]
		  | (_, _, NONE) => []
		(* end case *))
	(* compute the commands to set the background color *)
	  val mv = (case (reset, #bg s1, #bg s2)
		 of (false, SOME c1, SOME c2) => if c1 = c2 then mv else A.FG c2 :: mv
		  | (_, _, SOME c) => A.BG c :: mv
		  | (_, _, NONE) => mv
		(* end case *))
	(* compute the commands to set the other display attributes *)
	  fun add (proj, cmd, mv) =
		if ((reset orelse not(proj s1)) andalso proj s2)
		  then cmd::mv
		  else mv
	  val mv = add (#bold, A.BF, mv)
	  val mv = add (#blink, A.BLINK, mv)
	  val mv = add (#ul, A.UL, mv)
	  val mv = add (#rev, A.REV, mv)
	  val mv = add (#invis, A.INVIS, mv)
	  in
	    case (reset, mv)
	     of (false, []) => ""
	      | (true, []) => A.toString[]
	      | (true, mv) => A.toString[] ^ A.toString mv
	      | (false, mv) => A.toString mv
	    (* end case *)
	  end

  (* apply a command to a state *)
    fun updateState1 (cmd, {fg, bg, bold, blink, ul, rev, invis}) = (
	  case cmd
	   of A.FG c =>
		{fg=SOME c,  bg=bg, bold=bold, blink=blink, ul=ul,   rev=rev,  invis=invis}
	    | A.BG c =>
		{fg=fg, bg=SOME c,  bold=bold, blink=blink, ul=ul,   rev=rev,  invis=invis}
	    | A.BF =>
		{fg=fg, bg=bg,      bold=true, blink=blink, ul=ul,   rev=rev,  invis=invis}
	    | A.BLINK =>
		{fg=fg, bg=bg,      bold=bold, blink=true,  ul=ul,   rev=rev,  invis=invis}
	    | A.UL =>
		{fg=fg, bg=bg,      bold=bold, blink=blink, ul=true, rev=rev,  invis=invis}
	    | A.REV =>
		{fg=fg, bg=bg,      bold=bold, blink=blink, ul=ul,   rev=true, invis=invis}
	    | A.INVIS =>
		{fg=fg, bg=bg,      bold=bold, blink=blink, ul=ul,   rev=rev,  invis=true}
	  (* end case *))

  (* apply a sequence of commands to a state *)
    fun updateState ([], st) = st
      | updateState (cmd::r, st) = updateState (r, updateState1 (cmd, st))

    type style = A.style list

    datatype device = DEV of {
	mode : bool ref,
	dst : TextIO.outstream,
	wid : int,
	stk : state list ref
      }

    fun top [] = initState
      | top (st::r) = st

    fun sameStyle (s1 : style, s2) = (s1 = s2)

    fun pushStyle (DEV{mode, dst, wid, stk}, sty) =
	  if (! mode)
	    then let
	      val curSt = top (!stk)
	      val newSt = updateState (sty, curSt)
	      in
		TextIO.output (dst, transition(curSt, newSt));
		stk := newSt :: !stk
	      end
	    else ()

    fun popStyle (DEV{mode, dst, wid, stk}) =
	  if (! mode)
	    then (case !stk
	       of [] => ()
		| curSt::r => let
		    val newSt = top r
		    in
		      TextIO.output (dst, transition(curSt, newSt));
		      stk := r
		    end
	      (* end case *))
	    else ()

    fun defaultStyle _ = []

    fun openDev {dst, wid} = let
(* FIXME: compute mode based on file type *)val mode = true
	  in
	    DEV{dst = dst, wid = wid, mode = ref mode, stk = ref[]}
	  end

  (* maximum printing depth (in terms of boxes) *)
    fun depth _ = NONE

  (* the width of the device *)
    fun lineWidth (DEV{wid, ...}) = SOME wid
  (* the suggested maximum width of text on a line *)
    fun textWidth _ = NONE

  (* output some number of spaces to the device *)
    fun space (DEV{dst, ...}, n) = TextIO.output (dst, StringCvt.padLeft #" " n "")

  (* output a new-line to the device *)
    fun newline (DEV{dst, ...}) = TextIO.output1 (dst, #"\n")

  (* output a string/character in the current style to the device *)
    fun string (DEV{dst, ...}, s) = TextIO.output (dst, s)
    fun char (DEV{dst, ...}, c) = TextIO.output1 (dst, c)

  (* if the device is buffered, then flush any buffered output *)
    fun flush (DEV{dst, ...}) = TextIO.flushOut dst

  (* enable styled output by passing true to this function.  It returns
   * the previous state of the device.
   *)
    fun enableStyles (DEV{stk = ref(_::_), ...}, _) =
	  raise Fail "attempt to change mode inside scope of style"
      | enableStyles (DEV{mode as ref m, ...}, flg) = (mode := flg; m)

  end
