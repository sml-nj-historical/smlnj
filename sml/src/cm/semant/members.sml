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

    val expandOne : AbsPath.t * string option -> collection
    val sequential : collection * collection -> collection

    val num_look : collection -> string -> int
    val ml_look : collection -> GenericVC.Symbol.symbol -> bool
    val cm_look : collection -> string -> bool
end

structure MemberCollection :> MEMBERCOLLECTION = struct

    type collection = unit

    fun expandOne (f: AbsPath.t, c: string option) = ()
    fun sequential (c1: collection, c2: collection) = ()

    fun num_look (c: collection) (s: string) = 0
    fun ml_look (c: collection) (s: GenericVC.Symbol.symbol) = false
    fun cm_look (c: collection) (s: string) = false
end
