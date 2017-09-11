#!/bin/sh
#
# regenerate the emitters for the AMD64 from the MDL file
#

sml -DUNSHARED_MLRISC <<XXXX
CM.make "Tools/MDL/sources.cm";
MDLGen.gen("amd64/amd64.mdl");
XXXX
