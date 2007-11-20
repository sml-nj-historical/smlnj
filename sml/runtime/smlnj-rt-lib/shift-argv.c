/* shift-argv.c
 *
 * COPYRIGHT (c) 2007 by The Fellowship of SML/NJ
 */

#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "smlnj-runtime.h"

/* shiftArgv:
 */
void shiftArgv ()
{
  if (*CmdLineArgs != NIL(char *))
    ++CmdLineArgs;
}
