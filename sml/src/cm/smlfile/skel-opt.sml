(* just a placeholder so far *)

(*
 * Optimizing skeletons.
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
signature SKELOPT = sig
    val opt : Skeleton.decl -> Skeleton.decl
end

structure SkelOpt :> SKELOPT = struct
    fun opt d = d
end
