/* raw-argv.c
 *
 * COPYRIGHT (c) 1992 by AT&T Bell Laboratories.
 */

#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "smlnj-runtime.h"

/* rawArgv:
 */
ML_string_list_t rawArgv (ml_state_t *msp)
{
    return ML_CStringList (msp, RawArgs);
}

