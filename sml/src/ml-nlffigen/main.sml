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
	fun proc ([hfile],
		  sgf, stf, cmf, sgn, stn, asu, wid, lsp, t) =
	    let val ifile = OS.FileSys.tmpName ()
		val cpp_tmpl = getOpt (OS.Process.getEnv "FFIGEN_CPP",
				       "gcc -E -U__GNUC__ %s > %t")
		val cpp = substitute (cpp_tmpl, hfile, ifile)
		val hfile_file = OS.Path.file hfile
		val sgf = getOpt (sgf, hfile_file ^ ".sig")
		val stf = getOpt (stf, hfile_file ^ ".sml")
		val cmf = getOpt (cmf, hfile_file ^ ".cm")
		val (g_sgn, g_stn) = mangle hfile_file
		val sgn = getOpt (sgn, g_sgn)
		val stn = getOpt (stn, g_stn)
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
			  allSU = asu,
			  lambdasplit = lsp,
			  wid = getOpt (wid, 75),
			  target = t }
		handle e => (OS.FileSys.remove ifile handle _ => (); raise e);
		OS.FileSys.remove ifile handle _ => ();
		OS.Process.success
	    end
	  | proc ("-sigfile" :: f :: l,
		  _, stf, cmf, sgn, stn, asu, wid, lsp, t) =
	    proc (l, SOME f, stf, cmf, sgn, stn, asu, wid, lsp, t)
	  | proc ("-strfile" :: f :: l,
		  sgf, _, cmf, sgn, stn, asu, wid, lsp, t) =
	    proc (l, sgf, SOME f, cmf, sgn, stn, asu, wid, lsp, t)
	  | proc ("-cmfile" :: f :: l,
		  sgf, stf, _, sgn, stn, asu, wid, lsp, t) =
	    proc (l, sgf, stf, SOME f, sgn, stn, asu, wid, lsp, t)
	  | proc ("-signame" :: n :: l,
		  sgf, stf, cmf, _, stn, asu, wid, lsp, t) =
	    proc (l, sgf, stf, cmf, SOME n, stn, asu, wid, lsp, t)
	  | proc ("-strname" :: n :: l,
		  sgf, stf, cmf, sgn, _, asu, wid, lsp, t) =
	    proc (l, sgf, stf, cmf, sgn, SOME n, asu, wid, lsp, t)
	  | proc ("-allSU" :: l,
		  sgf, stf, cmf, sgn, stn, _, wid, lsp, t) =
	    proc (l, sgf, stf, cmf, sgn, stn, true, wid, lsp, t)
	  | proc ("-width" :: i :: l,
		  sgf, stf, cmf, sgn, stn, asu, _, lsp, t) =
	    proc (l, sgf, stf, cmf, sgn, stn, asu, Int.fromString i, lsp, t)
	  | proc ("-lambdasplit" :: s :: l,
		  sgf, stf, cmf, sgn, stn, asu, wid, _, t) =
	    proc (l, sgf, stf, cmf, sgn, stn, asu, wid, SOME s, t)
	  | proc ("-target" :: tg :: l,
		  sgf, stf, cmf, sgn, stn, asu, wid, lsp, _) =
	    proc (l, sgf, stf, cmf, sgn, stn, asu, wid, lsp, find_target tg)
	  | proc _ =
	    raise Fail
	     (concat ["usage: ", arg0,
	    " \\\n\t[-sigfile sigfile] [-strfile strfile] [-cmfile cmfile] \
            \ \\\n\t[-signame signame] [-strname strname] [-allSU] \
	    \ \\\n\t[-width linewidth] [-lambdasplit spec] [-target arch-os] \
	    \ \\\n    idlfile"])
    in
	proc (args, NONE, NONE, NONE, NONE, NONE, false, NONE, NONE,
	      default_target)
    end
  in
    fun main args = main0 args
	handle exn => (TextIO.output (TextIO.stdErr, General.exnMessage exn);
		       TextIO.output (TextIO.stdErr, "\n");
		       OS.Process.failure)
  end
end
