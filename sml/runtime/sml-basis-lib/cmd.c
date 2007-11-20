/* cmd.c
 *
 * COPYRIGHT (c) 2001 Bell Labs, Lucent Technologies.
 *
 * Command-line support for SML'97 Basis.
 */

#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "sml-basis.h"

/* cmdNname : unit -> string
 */
void cmdName (idl_string *s)
{
    *s = MLCmdName;
}

/* cmdArgs:
 */
ML_string_list_t cmdArgs (ml_state_t *msp)
{
    return ML_CStringList (msp, CmdLineArgs);
}
