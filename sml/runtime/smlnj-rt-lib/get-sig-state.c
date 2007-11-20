/* get-sig-state.c
 *
 * COPYRIGHT (c) 1995 by AT&T Bell Laboratories.
 */

#include "ml-base.h"
#include "ml-values.h"
#include "ml-state.h"
#include "ml-signals.h"
#include "smlnj-runtime.h"

/* getSigState : sysconst -> int
 *
 */
int getSigState (ml_state_t *msp, ML_sysconst_t sc)
{
    return GetSignalState (msp->ml_vproc, REC_SELINT(sc, 0));
}

