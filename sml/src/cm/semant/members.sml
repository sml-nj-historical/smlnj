(* just a placeholder so far *)

(*
 * Collections of members in CM descriptions.
 *   Involves:
 *     - running tools
 *     - fully analyzing sub-groups and sub-libraries
 *     - parsing ML files and getting their export lists
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
signature MEMBERCOLLECTION = sig

    type collection
    type environment

    val empty : collection
    val expandOne : AbsPath.t * string option -> collection
    val sequential : collection * collection -> collection

    val envOf : collection -> environment

    val num_look : environment -> string -> int
    val ml_look : environment -> GenericVC.Symbol.symbol -> bool
    val cm_look : environment -> string -> bool
end

structure MemberCollection :> MEMBERCOLLECTION = struct

    type collection = unit
    type environment = unit

    val empty = ()
    fun expandOne (f: AbsPath.t, c: string option) = ()
    fun sequential (c1: collection, c2: collection) = ()

    fun envOf (c: collection) = ()

    fun num_look (e: environment) (s: string) = 0
    fun ml_look (e: environment) (s: GenericVC.Symbol.symbol) = false
    fun cm_look (e: environment) (s: string) = false
end
