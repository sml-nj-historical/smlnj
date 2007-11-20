/* set-sig-mask.c
 *
 * COPYRIGHT (c) 1995 by AT&T Bell Laboratories.
 */

#include "ml-base.h"
#include "ml-values.h"
#include "ml-signals.h"
#include "smlnj-runtime.h"

/* setSigMask : sysconst list option -> unit
 *
 * Mask the listed signals.
 */
void setSigMask (ml_state_t *msp, ML_sysconst_list_opt_t m)
{
    SetSignalMask (m);
}

