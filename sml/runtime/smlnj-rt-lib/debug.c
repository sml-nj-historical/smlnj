/* debug.c
 *
 * COPYRIGHT (c) 1995 by AT&T Bell Laboratories.
 *
 * Print a string out to the debug stream.
 */

#include "ml-base.h"
#include "ml-values.h"
#include "smlnj-runtime.h"

/* debug : string -> unit
 *
 */
void debug (ml_state_t *msp, idl_string s)
{
    SayDebug (s);
}

