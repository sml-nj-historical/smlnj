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
    val primaryPrompt = ref "- "
    val secondaryPrompt = ref "= "
    val lazysml = ref false
    val overloadKW = ref false
    val quotation = ref false 
end
