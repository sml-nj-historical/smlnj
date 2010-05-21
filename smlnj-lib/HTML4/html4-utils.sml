(* ______________________________________________________________________
   html4-utils.sml

   Defines a set of utility data types and functions for the HTML 4 parser.
   ______________________________________________________________________ *)

structure HTML4Utils = struct

datatype 'a tree = Nd of 'a tree list
                 | Lf of 'a

type tag_payload = string * (Atom.atom * string option) list

fun attrToStr (name, NONE) = Atom.toString name
  | attrToStr (name, SOME a_val) = String.concat [Atom.toString name, " = ",
                                                  a_val]

fun attrsToStr attrs = String.concatWith "  " (map attrToStr attrs)

fun payloadToStr (payload, []) = payload
  | payloadToStr (_, attrs as (attr :: _)) = attrsToStr attrs

end

(* ______________________________________________________________________
   End of html4-utils.sml
   ______________________________________________________________________ *)
