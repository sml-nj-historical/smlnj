/* list-signals.c
 *
 * COPYRIGHT (c) 1995 by AT&T Bell Laboratories.
 */

#include "ml-base.h"
#include "ml-signals.h"
#include "smlnj-runtime.h"

/* listSignals : unit -> sysconst list
 *
 * List the supported signals.
 */
ML_sysconst_list_t listSignals (ml_state_t *msp)
{
    return ListSignals (msp);
}

