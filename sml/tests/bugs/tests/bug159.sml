(* bug 159 *)
(* caused Compiler bug: TypesUtil.lookTycPath while printing type of con ID *)
signature SYMTAB =
    sig
        type ident
    end

signature LEX =
    sig
        structure Symtab : SYMTAB

        datatype lexeme =
            ID of Symtab.ident
          | DELIM
    end

structure Symtab =
    struct
        type ident = string
    end

functor lex( symtab : SYMTAB ) =
    struct
        structure Symtab : SYMTAB = symtab
        datatype lexeme =
            ID of Symtab.ident
          | DELIM
    end

structure Lex : LEX = lex( Symtab )
