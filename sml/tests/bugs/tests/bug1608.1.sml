(* bug1608.1.sml *)

signature CELLS_BASIS =
sig
   type cellkindInfo
   type cellkindDesc
   datatype cellkind = GP | FP | CC | MEM | CTRL
      | MISC_KIND of cellkindInfo ref
   datatype cell =
      CELL of {id   : int, col  : cellColor ref, desc : cellkindDesc }
   and cellColor =
         MACHINE of int | PSEUDO | ALIASED of cell | SPILLED
   structure HashTable : MONO_HASH_TABLE where type Key.hash_key = cell
   structure ColorTable : MONO_HASH_TABLE where type Key.hash_key = cell

end

structure CellsInternal =
struct

   datatype cellkindInfo = INFO of {name:string, nickname:string}
   datatype cellkind = GP | FP | CC | MEM | CTRL
      | MISC_KIND of cellkindInfo ref (* client defined *)
   datatype cellkindDesc =
        DESC of
        {kind             : cellkind,
         counter          : int ref,
         low              : int,
         high             : int,
         toString         : int -> string,
         toStringWithSize : int * int -> string,
         defaultValues    : (int * int) list,
         physicalRegs     : cell Array.array ref,
         zeroReg          : int option
        }
   and cell = CELL of {id   : int, col  : cellColor ref, desc : cellkindDesc }
   and cellColor = MACHINE of int | PSEUDO | ALIASED of cell | SPILLED
end
                              
structure CellsBasis : CELLS_BASIS =
struct

   structure I = CellsInternal

   datatype cellkind     = datatype I.cellkind
   datatype cellkindInfo = datatype I.cellkindInfo
   datatype cellkindDesc = datatype I.cellkindDesc
   datatype cell         = datatype I.cell
   datatype cellColor    = datatype I.cellColor

   fun error msg = (print msg; raise Match)

    fun hashCell _ = error "hashCell"
    fun hashColor _ = error "hashColor"
    fun sameCell _ = error "sameCell"
    fun sameColor _ = error "sameColor"

    structure HashTable =
      HashTableFn(type hash_key = cell
                  val hashVal = hashCell
                  val sameKey = sameCell)

    structure ColorTable =
      HashTableFn(type hash_key = cell
                  val hashVal = hashColor
                  val sameKey = sameColor)

end

signature CELLS_COMMON =
sig
   include CELLS_BASIS
end
         where type cellkind     = CellsBasis.cellkind
             and type cellkindDesc = CellsBasis.cellkindDesc
             and type cellkindInfo = CellsBasis.cellkindInfo
             and type cell         = CellsBasis.cell
             and HashTable = CellsBasis.HashTable
             and ColorTable = CellsBasis.ColorTable                   
