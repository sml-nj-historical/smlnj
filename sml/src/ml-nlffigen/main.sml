(*
 * main.sml - Driver routine ("main") for ml-ffigen.
 *
 *  (C) 2001, Lucent Technologies, Bell Labs
 *
 * author: Matthias Blume (blume@research.bell-labs.com)
 *)
structure Main = struct
  local

    structure RE =
        RegExpFn (structure P = AwkSyntax
		  structure E = DfaEngine)

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
	fun substitute (tmpl, opts, s, t) = let
	    fun loop ([], a) = String.implode (rev a)
	      | loop (#"%" :: #"s" :: l, a) = loop (l, push (s, a))
	      | loop (#"%" :: #"t" :: l, a) = loop (l, push (t, a))
	      | loop (#"%" :: #"o" :: l, a) = loop (l, push (opts, a))
	      | loop (c :: l, a) = loop (l, c :: a)
	    and push (x, a) = List.revAppend (String.explode x, a)
	in
	    loop (String.explode tmpl, [])
	end

	val dir = ref "NLFFI-Generated"
	val cmf = ref "nlffi-generated.cm"
	val prefix = ref ""
	val ems = ref []
	val libh = ref "Library.libh"
	val cmpl = ref true
	val asu = ref false
	val wid = ref NONE
	val lsp = ref NONE
	val target = ref default_target
	val wrq = ref NONE
	val namedargs = ref false
	val cppopts = ref ""
	val regexp = ref NONE

	fun finish cfiles = let
	    fun mkidlsource cfile = let
		val ifile = OS.FileSys.tmpName ()
		val cpp_tmpl = getOpt (OS.Process.getEnv "FFIGEN_CPP",
				       "gcc -E -U__GNUC__ %o %s > %t")
		val cpp = substitute (cpp_tmpl, !cppopts, cfile, ifile)
	    in
		if OS.Process.system cpp <> OS.Process.success then
		    raise Fail ("C-preprocessor failed: " ^ cpp)
		else ();
		ifile
	    end

	    val match =
		case !regexp of
		    NONE => (fn _ => false)
		  | SOME re =>
		    (fn s => let fun creader p =
				     if p >= size s then NONE
				     else SOME (String.sub (s, p), p + 1)
			     in
				 isSome (StringCvt.scanString (RE.prefix re) s)
			     end)
	in
	    Gen.gen { cfiles = cfiles,
		      match = match,
		      mkidlsource = mkidlsource,
		      dirname = !dir,
		      cmfile = !cmf,
		      prefix = !prefix,
		      extramembers = !ems,
		      libraryhandle = !libh,
		      complete = !cmpl,
		      allSU = !asu,
		      lambdasplit = !lsp,
		      weightreq = !wrq,
		      wid = getOpt (!wid, 75),
		      namedargs = !namedargs,
		      target = !target };
	    OS.Process.success
	end

	fun iscppopt opt =
	    size opt > 2 andalso
	    String.sub (opt, 0) = #"-" andalso
	    Char.contains "IDU" (String.sub (opt, 1))

	fun addcppopt opt =
	    cppopts := (case !cppopts of
			    "" => opt
			  | opts => concat [opts, " ", opt])

	fun proc ("-allSU" :: l) = (asu := true; proc l)
	  | proc ("-width" :: i :: l) = (wid := Int.fromString i; proc l)
	  | proc ("-lambdasplit" :: s :: l) = (lsp := SOME s; proc l)
	  | proc ("-target" :: tg :: l) = (target := find_target tg; proc l)
	  | proc ("-light" :: l) = (wrq := SOME false; proc l)
	  | proc ("-heavy" :: l) = (wrq := SOME true; proc l)
	  | proc ("-namedargs" :: l) = (namedargs := true; proc l)
	  | proc ("-incomplete" :: l) = (cmpl := false; proc l)
	  | proc ("-libhandle" :: lh :: l) = (libh := lh; proc l)
	  | proc ("-include" :: es :: l) = (ems := es :: !ems; proc l)
	  | proc ("-prefix" :: p :: l) = (prefix := p; proc l)
	  | proc ("-dir" :: d :: l) = (dir := d; proc l)
	  | proc ("-cmfile" :: f :: l) = (cmf := f; proc l)
	  | proc ("-cppopt" :: opt :: l) = (addcppopt opt; proc l)
	  | proc ("-version" :: _) =
	    (TextIO.output (TextIO.stdOut, Gen.version ^ "\n");
	     OS.Process.exit OS.Process.success)
	  | proc ("-match" :: re :: l) =
	    (regexp := SOME (RE.compileString re); proc l)
	  | proc ("--" :: cfiles) = finish cfiles
	  | proc (l0 as (opt :: l)) =
	    if iscppopt opt then (addcppopt opt; proc l) else finish l0
	  | proc cfiles = finish cfiles
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
