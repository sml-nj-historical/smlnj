(* html-device.sml
 *
 * COPYRIGHT (c) 1997 Bell Labs, Lucent Technologies.
 *
 * A pretty printing device that uses HTML markup to control layout.
 *)

structure HTMLDev : sig

    include PP_DEVICE

    val styleTT : style
    val styleI : style
    val styleB : style
    val styleU : style
    val styleSTRIKE : style
    val styleEM : style
    val styleSTRONG : style
    val styleDFN : style
    val styleCODE : style
    val styleSAMP : style
    val styleKBD : style
    val styleVAR : style
    val styleCITE : style

    val color : string -> style

    val openDev : {wid : int, textWid : int option} -> device
    val done : device -> HTML.text

  end = struct

    datatype style
      = NOEMPH
      | TT | I | B | U | STRIKE | EM
      | STRONG | DFN | CODE | SAMP | KBD
      | VAR | CITE
      | COLOR of string

    datatype device = DEV of {
	lineWid : int,
	textWid : int option,
	emphStk	: (HTML.text list * style) list ref,
	txt : HTML.text list ref
      }

  (* return the current emphasis *)
    fun curEmph (DEV{emphStk, ...}) = (case !emphStk
	   of [] => NOEMPH
	    | ((_, em)::r) => em
	  (* end case *))

  (* add PCDATA to the text list *)
    fun pcdata (DEV{txt, ...}, s) = txt := HTML.PCDATA s :: !txt

  (* replace the sequence of PCDATA elements at the head of the
   * txt list with its concatenation.
   *)
    fun concatTxt (DEV{txt, ...}) = let
	  fun f ([], []) = []
	    | f (HTML.PCDATA s :: r, l) = f (r, s::l)
	    | f (r, l) = HTML.PCDATA(String.concat l) :: r
	  in
	    f (!txt, [])
	  end

  (* are two styles the same? *)
    fun sameStyle (s1 : style, s2) = (s1 = s2)

    fun wrapStyle (sty, [], tl') = tl'
      | wrapStyle (sty, tl, tl') = let
	  val t = (case tl of [t] => t | _ => HTML.TextList(List.rev tl))
	  val t = (case sty
		 of NOEMPH => t
		  | TT => HTML.TT t
		  | I => HTML.I t
		  | B => HTML.B t
		  | U => HTML.U t
		  | STRIKE => HTML.STRIKE t
		  | EM => HTML.EM t
		  | STRONG => HTML.STRONG t
		  | DFN => HTML.DFN t
		  | CODE => HTML.CODE t
		  | SAMP => HTML.SAMP t
		  | KBD => HTML.KBD t
		  | VAR => HTML.VAR t
		  | CITE => HTML.CITE t
		  | (COLOR c) => HTML.FONT{color=SOME c, size=NONE, content=t}
		(* end case *))
	  in
	    t :: tl'
	  end

  (* push/pop a style from the devices style stack.  A pop on an
   * empty style stack is a nop.
   *)
    fun pushStyle (dev as DEV{emphStk, txt, ...}, sty) = (
	  emphStk := (concatTxt dev, sty) :: !emphStk;
	  txt := [])
    fun popStyle (dev as DEV{emphStk, txt, ...}) = let
	  val (tl, sty)::r = !emphStk
	  in
	    txt := wrapStyle (sty, concatTxt dev, tl);
	    emphStk := r
	  end
 
  (* the default style for the device (this is the current style,
   * if the style stack is empty).
   *)
    fun defaultStyle _ = NOEMPH

  (* maximum printing depth (in terms of boxes) *)
    fun depth _ = NONE
  (* the width of the device *)
    fun lineWidth (DEV{lineWid, ...}) = SOME lineWid
  (* the suggested maximum width of text on a line *)
    fun textWidth (DEV{textWid, ...}) = textWid

  (* output some number of spaces to the device *)
    fun space (dev, n) =
	  pcdata(dev, concat(List.tabulate (n, fn _ => "&nbsp;")))

  (* output a new-line to the device *)
    fun newline (dev as DEV{txt, ...}) =
	  txt := HTML.BR{clear=NONE} :: (concatTxt dev)

  (* output a string/character in the current style to the device *)
    val string = pcdata
    fun char (dev, c) = pcdata(dev, str c)

  (* flush is a nop for us *)
    fun flush _ = ()

    val styleTT = TT
    val styleI = I
    val styleB = B
    val styleU = U
    val styleSTRIKE = STRIKE
    val styleEM = EM
    val styleSTRONG = STRONG
    val styleDFN = DFN
    val styleCODE = CODE
    val styleSAMP = SAMP
    val styleKBD = KBD
    val styleVAR = VAR
    val styleCITE = CITE
    val color = COLOR

    fun openDev {wid, textWid} = DEV{
	    txt = ref [],
	    emphStk = ref [],
	    lineWid = wid,
	    textWid = textWid
	  }

    fun done (dev as DEV{emphStk = ref [], txt, ...}) = (case (concatTxt dev)
	   of [t] => (txt := []; t)
	    | l => (txt := []; HTML.TextList(List.rev l))
	  (* end case *))
      | done _ = raise Fail "device is not done yet"

  end; (* HTMLDev *)

