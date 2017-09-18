/*! \file sleep.c
 *
 * \author John Reppy
 *
 * Support for Posix.Process.sleep function
 */

/*
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 */

#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "ml-c.h"
#include "cfun-proto-list.h"
#include <unistd.h>

/* _ml_P_Process_sleep : int -> int
 *
 * Suspend execution for interval in seconds.  Returns 0 on normal completion, or
 * else returns the amount of remaining time when signaled.
 *
 * TODO: generalize to finer-grain sleeping (bug #173)
 */
ml_val_t _ml_P_Process_sleep (ml_state_t *msp, ml_val_t arg)
{
#if defined(HAS_NANOSLEEP)
#error nanosleep support not implemented yet
#elif defined(HAS_USLEEP)
#error usleep support not implemented yet
#else
    return INT_CtoML(sleep(INT_MLtoC(arg)));
#endif

} /* end of _ml_P_Process_sleep */
