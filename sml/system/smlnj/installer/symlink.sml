(* symlink.sml
 *
 *   Create an actual symbolic link (requires Posix).
 *
 * Copyright (c) 2007 by The Fellowship of SML/NJ
 *
 * Author: Matthias Blume (blume@tti-c.org)
 *)
structure Link = struct val link = Posix.FileSys.symlink end
