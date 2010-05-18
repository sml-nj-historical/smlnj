(* ______________________________________________________________________
   html4-utils.sml

   Defines a set of utility data types and functions for the HTML 4 parser.
   ______________________________________________________________________ *)

structure HTML4Utils = struct

datatype 'a tree = Nd of 'a tree list
                 | Lf of 'a

end

(* ______________________________________________________________________
   End of html4-utils.sml
   ______________________________________________________________________ *)
