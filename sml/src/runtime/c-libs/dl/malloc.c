/* malloc.c
 *
 * COPYRIGHT (c) 2001 by Lucent Technologies, Bell Laboratories
 */

#include <stdlib.h>
#include "ml-unixdep.h"
#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "ml-c.h"
#include "cfun-proto-list.h"

/* _ml_P_Dynload_malloc : Word32.word -> Word32.word option
 *
 * Do a regular malloc call.
 */
ml_val_t _ml_U_Dynload_malloc (ml_state_t *msp, ml_val_t arg)
{
  size_t nbytes = WORD_MLtoC (arg);
  void *p = malloc (nbytes);
  ml_val_t r, w;

  if (p == NULL)
    r = OPTION_NONE;
  else {
    WORD_ALLOC (msp, w, (Word_t) p);
    OPTION_SOME (msp, r, w);
  }
  return r;
}
