(* bug1173.sml *)

(* Author : Thomas Schmidt (schmidts@tagetes.informatik.uni-stuttgart.de)
 * Date   : 18.03.97, 21.03.97
 * System : Linux 1.2.13
 * Version: 1.09.21.1 and 1.09.25
 * File   : sml-bugtest.sml
 * Use for: Test the presence of the bug that prevents types to be
 *          exported from abstractions.
 *)

(* First a little preparation *)
signature S =
    sig
        type t
    end

structure R: S =
    struct
        type t = real
    end

structure I: S =
    struct
        type t = int
    end

(* The abstraction/open combination can be replaced by the :> - Operator in
 * 1.09.25+. Doesn't change the behaviour, though:
 *)
(*
functor F(T:S) :>
    sig type t = T.t
        val one: T.t -> T.t
        val two: t -> t
    end =
struct
    type t = T.t
    fun one t = t
    fun two t = t
end
*)

functor F(T:S) =
    struct
        structure dummy :>
            sig type t = T.t
                val one: T.t -> T.t
                val two: t -> t
            end
        = struct
            type t = T.t
            fun one t = t
            fun two t = t
          end

        open dummy
    end

structure FI = F(I);
structure FR = F(R);

(* This is executed in 109.21.1 without problems.
 * The Problem lies here with .25. Sometimes it works, sometimes it doesn't.
 *)

(* works under .25 *)
FI.one 2;
FR.one 2.0;
FR.two 2.0;

(* This doesn't work under .25 *)
FI.two 2;
