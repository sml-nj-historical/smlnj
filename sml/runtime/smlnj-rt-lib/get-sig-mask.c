/* get-sig-mask.c
 *
 * COPYRIGHT (c) 1995 by AT&T Bell Laboratories.
 */

#include "ml-base.h"
#include "ml-signals.h"
#include "smlnj-runtime.h"

/* getSigMask : unit -> sysconst list option
 *
 */
ML_sysconst_list_opt_t getSigMask (ml_state_t *msp)
{
    return GetSignalMask (msp);
}

