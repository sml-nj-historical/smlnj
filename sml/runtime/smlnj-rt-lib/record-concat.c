/* record-concat.c
 *
 * COPYRIGHT (c) 1998 Bell Labs, Lucent Technologies.
 *
 * Concatenation for records.
 */


#include "ml-base.h"
#include "ml-values.h"
#include "ml-state.h"
#include "ml-objects.h"
#include "gc.h"
#include "smlnj-runtime.h"
#include "ml-c.h"

/* recordConcat : (object * object) -> object
 *
 */
ML_object_t recordConcat (ml_state_t *msp, ML_object_t r1, ML_object_t r2)
{
    if (r1 == ML_unit)
	return r2;
    else if (r2 == ML_unit)
	return r1;
    else {
	ml_val_t	res = RecordConcat (msp, r1, r2);

	if (res == ML_unit)
	    return RAISE_ERROR(msp, "recordConcat: not a record");
	else
	    return res;
    }

}
