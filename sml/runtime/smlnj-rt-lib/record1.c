/* record1.c
 *
 * COPYRIGHT (c) 1998 Bell Labs, Lucent Technologies.
 *
 * Create a singleton record.
 */


#include "ml-base.h"
#include "ml-values.h"
#include "ml-state.h"
#include "ml-objects.h"
#include "smlnj-runtime.h"

/* record1 : object -> object
 *
 */
ML_object_t record1 (ml_state_t *msp, ML_object_t obj)
{
    ml_val_t    res;

    REC_ALLOC1(msp, res, obj);

    return res;

}

