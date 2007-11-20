/* dummy.c
 *
 * COPYRIGHT (c) 1995 by AT&T Bell Laboratories.
 *
 * This is a dummy run-time routine for when we would like to call
 * a null C function.
 */

#include "ml-base.h"
#include "ml-values.h"
#include "smlnj-runtime.h"

/* dummy : string -> unit
 *
 * The string argument can be used as a unique marker.
 */
void dummy (idl_string s)
{
  /*
    char	*s = STR_MLtoC(arg);
  */

}

