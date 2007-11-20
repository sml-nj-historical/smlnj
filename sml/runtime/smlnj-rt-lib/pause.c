/* pause-until-sig.c
 *
 * COPYRIGHT (c) 1995 by AT&T Bell Laboratories.
 */

#include "ml-base.h"
#include "ml-values.h"
#include "ml-state.h"
#include "ml-signals.h"
#include "smlnj-runtime.h"

/* pause-until-sig : unit -> unit
 *
 * Pause until the next signal.
 */
void pauseUntilSig (ml_state_t *msp)
{
    PauseUntilSignal (msp->ml_vproc);
}

