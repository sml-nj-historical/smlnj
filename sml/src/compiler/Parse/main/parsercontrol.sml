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

    val m = Controls.registry { name = "parser settings",
				priority = [10, 10, 3],
				obscurity = 3,
				prefix = "parser-",
				default_suffix = SOME "-default",
				mk_ename = NONE }

    val string_r = Controls.group m Controls.string

    val flag_r = Controls.group m Controls.bool

    val primaryPrompt =
	Controls.new string_r
		     { stem = "primary-prompt",
		       descr = "primary prompt",
		       fallback = "- " }

    val secondaryPrompt =
	Controls.new string_r
		     { stem = "secondary-prompt",
		       descr = "secondary prompt",
		       fallback = "= " }

    val lazysml =
	Controls.new flag_r
		     { stem = "lazy-keyword",
		       descr = "whether `lazy' is considered a keyword",
		       fallback = false }

    val overloadKW =
	Controls.new flag_r
		     { stem = "overload",
		       descr = "whether (_)overload keyword is enabled",
		       fallback = false }

    val quotation =
	Controls.new flag_r
		     { stem = "quotations",
		       descr = "whether (anti-)quotations are recognized",
		       fallback = false }
end
