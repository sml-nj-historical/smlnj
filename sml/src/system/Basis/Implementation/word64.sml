(* word64.sml
 *
 *   64-bit word support
 *
 *   (This is still in its very early stages.)
 *
 * Copyright (c) 2004 by The Fellowship of SML/NJ
 *
 * Author: Matthias Blume (blume@tti-c.org)
 *)
structure Word64 (* : WORD *) = struct

    type word = Word64.word

    local structure W64 = InlineT.Word64
    in

    val extern = W64.extern
    val intern = W64.intern

    end
end
