(*
 * semant/modname.sig:
 *   `module name' abstraction and related types
 *
 *   Copyright (c) 1999 by Lucent Technologies
 *
 * author: Matthias Blume (blume@cs.princeton.edu)
 *)
signature MODNAME = sig

    type symbol = GenericVC.Symbol.symbol
    type t
    type set
    type path
    eqtype namespace

    exception ModuleNameError and PathError

    val equal: t * t -> bool
    val namespaceOf: t -> namespace
    val nameOf: t -> string
    val symbolOf: t -> symbol
    val makestring: t -> string
    val ofSymbol: symbol -> t
    val filterSymbols: symbol list -> t list

    val STRspace: namespace
    val SIGspace: namespace
    val FCTspace: namespace
    val FSIGspace: namespace

    val create: namespace * string -> t

    val structMN: string -> t
    val sigMN: string -> t
    val functMN: string -> t
    val funsigMN: string -> t

    val pathFirstModule: path -> t
    val restOfPath: path -> path option
    val pathLastModule: path -> t
    val pathOfSymbolList: symbol list -> path
    val mnListOfPath: path -> t list
    val pathOfMNList: t list -> path
    val createPathSML: string list * t -> path
    val nameOfPath: path -> string

    val memberOf: set -> t -> bool
    val singleton: t -> set
    val union: set * set -> set
    val intersection: set * set -> set
    val difference: set * set -> set
    val add: t * set -> set
    val addl: t list * set -> set
    val makeset: t list -> set
    val makelist: set -> t list
    val empty: set
    val isEmpty: set -> bool
    val fold: (t * 'a -> 'a) -> 'a -> set -> 'a
    val sameSet: set * set -> bool
end
