(* Copyright 1989 by AT&T Bell Laboratories *)
signature SYMBOL = sig
    type symbol
    datatype namespace =
       VALspace | TYCspace | SIGspace | STRspace | FCTspace | FIXspace |
       LABspace | TYVspace | FSIGspace
    val eq: symbol * symbol -> bool
    and symbolGt : symbol * symbol -> bool
    and symbolCMLt : symbol * symbol -> bool
    and varSymbol: string -> symbol
    and tycSymbol: string -> symbol
    and sigSymbol: string -> symbol
    and strSymbol: string -> symbol
    and fctSymbol: string -> symbol
    and fsigSymbol: string -> symbol
    and fixSymbol: string -> symbol
    and labSymbol: string -> symbol
    and tyvSymbol: string -> symbol
    and var'n'fix : string -> symbol * symbol
    and name: symbol -> string
    and number: symbol -> int
    val nameSpace : symbol -> namespace
    val nameSpaceToString : namespace -> string
    val symbolToString : symbol -> string

(* Probably should merge STRspace and FCTspace into one namespace.
   Similarly for SIGspace and FSIGspace. *)

end

signature FASTSYMBOL = 
  sig
    type raw_symbol
    type symbol
    val rawSymbol: int * string -> raw_symbol
    val sameSpaceSymbol : symbol -> raw_symbol -> symbol
    val varSymbol: raw_symbol -> symbol
    val tycSymbol: raw_symbol -> symbol
    val sigSymbol: raw_symbol -> symbol
    val strSymbol: raw_symbol -> symbol
    val fctSymbol: raw_symbol -> symbol
    val fixSymbol: raw_symbol -> symbol
    val labSymbol: raw_symbol -> symbol
    val tyvSymbol: raw_symbol -> symbol
    val fsigSymbol: raw_symbol -> symbol
    val var'n'fix : raw_symbol -> symbol * symbol
  end (* signature FASTSYMBOL *)


