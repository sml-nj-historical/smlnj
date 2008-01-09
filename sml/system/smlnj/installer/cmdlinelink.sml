(* cmdlinelink.sml
 *
 *   Make a symbolic link using the command line (under Unix).
 *   This is used in the absence of Posix.FileSys.symlink.
 *
 * Copyright (c) 2008 by The Fellowship of SML/NJ
 *
 * Author: Matthias Blume (blume@tti-c.org)
 *)
structure Link = struct
    fun link { old, new } =
	ignore (OS.Process.system (concat ["ln -s ", old, " ", new]))
end
