(*
 * main.sml - Driver routine ("main") for ml-ffigen.
 *
 *  (C) 2001, Lucent Technologies, Bell Labs
 *
 * author: Matthias Blume (blume@research.bell-labs.com)
 *)
structure Main = struct
  local

    fun tgt (n, sz, sh, cc) =
	{ name  = n, sizes = sz, shift = sh, stdcall = cc }

    val default_target =
	tgt (DefaultName.name,
	     DefaultSizes.sizes, DefaultEndian.shift, DefaultCC.stdcall)

    val target_table =
	[tgt ("sparc-unix",
	      SizesSparc.sizes, EndianBig.shift, CC_ccall.stdcall),
	 tgt ("x86-unix",
	     SizesX86.sizes, EndianLittle.shift, CC_ccall.stdcall),
	 tgt ("x86-win32",
	      SizesX86.sizes, EndianLittle.shift, CC_stdcall.stdcall)
	 (* needs to be extended ... *)
	 ]

    fun find_target tg =
	case List.find (fn x => tg = #name x) target_table of
	    SOME t => t
	  | NONE => raise Fail (concat ["unknown target: " ^ tg])

    fun main0 (arg0, args) = let
	fun substitute (tmpl, s, t) = let
	    fun loop ([], a) = String.implode (rev a)
	      | loop (#"%" :: #"s" :: l, a) = loop (l, push (s, a))
	      | loop (#"%" :: #"t" :: l, a) = loop (l, push (t, a))
	      | loop (c :: l, a) = loop (l, c :: a)
	    and push (x, a) = List.revAppend (String.explode x, a)
	in
	    loop (String.explode tmpl, [])
	end
	fun mangle f = let
	    fun dot #"." = true
	      | dot _ = false
	    fun sep #"_" = true
	      | sep #"-" = true
	      | sep _ = false
	    fun finish l = let
		val fields = List.concat (map (String.fields sep) l)
		fun allUp x = String.map Char.toUpper x
		fun firstUp x =
		    case String.explode x of
			h :: t => String.implode
				      (Char.toUpper h :: map Char.toLower t)
		      | [] => ""
		val sigfields = map allUp fields
		val strfields = map firstUp fields
		val sgn =
		    case sigfields of
			[] => raise Fail ("file name without significant \
					  \characters: " ^ f)
		      | h :: t => String.concat (h ::
						 foldr (fn (x, l) =>
							   "_" :: x :: l)
						       [] t)
		val stn = String.concat strfields
	    in
		(sgn, stn)
	    end
	in
	    case rev (String.fields dot f) of
		("c" | "h") :: (l as (_ :: _)) => finish (rev l)
	      | l => finish (rev l)
	end

	val sgf = ref NONE
	val stf = ref NONE
	val cmf = ref NONE
	val sgn = ref NONE
	val stn = ref NONE
	val asu = ref false
	val wid = ref NONE
	val lsp = ref NONE
	val target = ref default_target
	val wrq = ref NONE

	fun proc [hfile] =
	    let val ifile = OS.FileSys.tmpName ()
		val cpp_tmpl = getOpt (OS.Process.getEnv "FFIGEN_CPP",
				       "gcc -E -U__GNUC__ %s > %t")
		val cpp = substitute (cpp_tmpl, hfile, ifile)
		val hfile_file = OS.Path.file hfile
		val sgf = getOpt (!sgf, hfile_file ^ ".sig")
		val stf = getOpt (!stf, hfile_file ^ ".sml")
		val cmf = getOpt (!cmf, hfile_file ^ ".cm")
		val (g_sgn, g_stn) = mangle hfile_file
		val sgn = getOpt (!sgn, g_sgn)
		val stn = getOpt (!stn, g_stn)
		val _ = if OS.Process.system cpp <> OS.Process.success then
			    raise Fail ("C-preprocessor failed: " ^ cpp)
			else ()
	    in
		Gen.gen { idlfile = hfile,
			  idlsource = ifile,
			  sigfile = sgf,
			  strfile = stf,
			  cmfile = cmf,
			  signame = sgn,
			  strname = stn,
			  allSU = !asu,
			  lambdasplit = !lsp,
			  weightreq = !wrq,
			  wid = getOpt (!wid, 75),
			  target = !target }
		handle e => (OS.FileSys.remove ifile handle _ => (); raise e);
		OS.FileSys.remove ifile handle _ => ();
		OS.Process.success
	    end
	  | proc ("-sigfile" :: f :: l) = (sgf := SOME f; proc l)
	  | proc ("-strfile" :: f :: l) = (stf := SOME f; proc l)
	  | proc ("-cmfile" :: f :: l) = (cmf := SOME f; proc l)
	  | proc ("-signame" :: n :: l) = (sgn := SOME n; proc l)
	  | proc ("-strname" :: n :: l) = (stn := SOME n; proc l)
	  | proc ("-allSU" :: l) = (asu := true; proc l)
	  | proc ("-width" :: i :: l) = (wid := Int.fromString i; proc l)
	  | proc ("-lambdasplit" :: s :: l) = (lsp := SOME s; proc l)
	  | proc ("-target" :: tg :: l) = (target := find_target tg; proc l)
	  | proc ("-light" :: l) = (wrq := SOME false; proc l)
	  | proc ("-heavy" :: l) = (wrq := SOME true; proc l)
	  | proc _ =
	    raise Fail
	     (concat ["usage: ", arg0,
	    " \\\n\t[-sigfile sigfile] [-strfile strfile] [-cmfile cmfile] \
            \ \\\n\t[-signame signame] [-strname strname] [-allSU] \
	    \ \\\n\t[-width linewidth] [-lambdasplit spec] [-target arch-os] \
	    \ \\\n    idlfile"])
    in
	proc args
    end
  in
    fun main args = main0 args
	handle exn => (TextIO.output (TextIO.stdErr, General.exnMessage exn);
		       TextIO.output (TextIO.stdErr, "\n");
		       OS.Process.failure)
  end
end
