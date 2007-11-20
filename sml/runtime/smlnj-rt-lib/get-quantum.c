/* get-quantum.c
 *
 * COPYRIGHT (c) 1996 AT&T Research.
 */

#include "ml-base.h"
#include "ml-values.h"
#include "smlnj-runtime.h"
#include "profile.h"

/* getQuantum : unit -> int
 *
 * Return the profile timer quantim in microseconds.
 */
int getQuantum ()
{
    return PROFILE_QUANTUM_US;
}
