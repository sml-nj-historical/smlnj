(* int64.sml
 *
 *   64-bit integers
 *
 *   (This is still in its very early stages.)
 *
 * Copyright (c) 2004 by The Fellowship of SML/NJ
 *
 * Author: Matthias Blume (blume@tti-c.org)
 *)
structure Int64 (* : INTEGER *) = struct

    type int = Int64.int

    local structure I64 = InlineT.Int64
    in

    val extern = I64.extern
    val intern = I64.intern

    end
end
