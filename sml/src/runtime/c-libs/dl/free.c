/* free.c
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

/* _ml_P_Dynload_free : Word32.word -> unit
 *
 * Do a regular free call.
 */
ml_val_t _ml_U_Dynload_free (ml_state_t *msp, ml_val_t arg)
{
  void *p = (void *) WORD_MLtoC (arg);

  free (p);
  return ML_unit;
}
