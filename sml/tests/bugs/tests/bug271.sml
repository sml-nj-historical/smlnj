(* bug271.sml *)
(* secondary compiler bug *)

signature IntMapSig =
    sig
        type 'a map
        val apply: ('a map)*int -> 'a
        exception NotFound
    end;
 
signature ValueSig =
    sig
        type value
    end;
 
signature SymbolSig =
    sig
        type sym
        val hash: sym -> int
    end;
 
functor SymTblFct(structure IntMap: IntMapSig
                  structure Val: ValSig
                  structure Sym: SymSig):
    sig
        type table
        exception Lookup
        val lookup: table * Sym.sym -> Val.value
        val update: table * Sym.sym * Val.value -> table
    end =
    struct
        datatype table = TBL of (Sym.sym * Val.value)list IntMap.map
        exception Lookup

        fun find(sym,[]) = raise Lookup
         |   find(sym,(sym',v)::rest) =
              if sym = sym' then v else find(sym,rest);

        fun lookup(TBL map, s) =
            let val n = Sym.hash(s)
                val l = IntMap.apply(map,n)
            in find(s,l)
            end handle IntMap.NotFound => raise Lookup

    (* ... *)
    end;
