(* parsercontrol.sml
 *
 * (C) 2001 Lucent Technologies, Bell Labs
 *)
signature PARSER_CONTROL = sig
    val primaryPrompt : string ref
    val secondaryPrompt : string ref

    (* turn on lazy keywords and lazy declaration processing *)
    val lazysml : bool ref		(* default false *)
    (* controls "overload" as keyword *)
    val overloadKW : bool ref
    (* controls "link_plugin" as keyword *)
    val linkPluginKW : bool ref
    (* controls backquote quotation *)
    val quotation : bool ref
end

structure ParserControl : PARSER_CONTROL = struct

    val priority = [10, 10, 3]
    val obscurity = 3
    val prefix = "parser"

    val registry = ControlRegistry.new { help = "parser settings" }

    val _ = BasicControl.nest (prefix, registry, priority)

    val string_cvt = ControlUtil.Cvt.string
    val flag_cvt = ControlUtil.Cvt.bool

    val nextpri = ref 0

    fun new (c, n, h, d) = let
	val r = ref d
	val p = !nextpri
	val ctl = Controls.control { name = n,
				     pri = [p],
				     obscurity = obscurity,
				     help = h,
				     ctl = r }
    in
	nextpri := p + 1;
	ControlRegistry.register
	    registry
	    { ctl = Controls.stringControl c ctl,
	      envName = SOME (ControlUtil.EnvName.toUpper "PARSER_" n) };
	r
    end


    val primaryPrompt =
	new (string_cvt, "primary-prompt", "primary prompt", "- ")

    val secondaryPrompt =
	new (string_cvt, "secondary-prompt", "secondary prompt","= ")

    val lazysml =
	new (flag_cvt, "lazy-keyword",
	     "whether `lazy' is considered a keyword", false)

    val overloadKW =
	new (flag_cvt, "overload",
	     "whether (_)overload keyword is enabled", false)

    val linkPluginKW =
	new (flag_cvt, "link-plugin",
	     "whether link_plugin keyword is enabled", false)

    val quotation =
	new (flag_cvt, "quotations",
	     "whether (anti-)quotations are recognized", false)
end
