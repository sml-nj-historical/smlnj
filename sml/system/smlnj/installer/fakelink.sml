(* fakelink.sml
 *
 *   Make a "link" by actually making a copy.
 *
 * Copyright (c) 2007 by The Fellowship of SML/NJ
 *
 * Author: Matthias Blume (blume@tti-c.org)
 *)
structure Link = struct
    fun link { old, new } = Copy.copy { from = old, to = new }
end
