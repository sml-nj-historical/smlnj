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
    (* controls backquote quotation *)
    val quotation : bool ref
end

structure ParserControl : PARSER_CONTROL = struct

    val priority = [10, 10, 3]
    val obscurity = 3
    val prefix = "parser"

    val registry = ControlRegistry.new { help = "parser settings" }

    val _ = BasicControl.nest (prefix, registry)

    val string_cvt = { tyName = "string",
		       fromString = SOME,
		       toString = fn x => x }

    val flag_cvt = { tyName = "bool",
		     fromString = Bool.fromString,
		     toString = Bool.toString }


    fun new (c, n, e, h, d) = let
	val r = ref d
	val ctl = Controls.control { name = n,
				     pri = priority,
				     obscurity = obscurity,
				     help = h,
				     ctl = r }
    in
	ControlRegistry.register
	    registry
	    { ctl = Controls.stringControl c ctl,
	      envName = SOME ("PARSER_" ^ e) };
	r
    end


    val primaryPrompt =
	new (string_cvt, "primary-prompt", "PRIMARY_PROMPT",
	     "primary prompt", "- ")

    val secondaryPrompt =
	new (string_cvt, "secondary-prompt", "SECONDARY_PROMPT",
	     "secondary prompt","= ")

    val lazysml =
	new (flag_cvt, "lazy-keyword", "LAZY_KEYWORD",
	     "whether `lazy' is considered a keyword", false)

    val overloadKW =
	new (flag_cvt, "overload", "OVERLOAD",
	     "whether (_)overload keyword is enabled", false)

    val quotation =
	new (flag_cvt, "quotations", "QUOTATION",
	     "whether (anti-)quotations are recognized", false)
end
