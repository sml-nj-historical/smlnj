/* blast_in.c
 *
 * COPYRIGHT (c) 1994 by AT&T Bell Laboratories.
 */

#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "ml-c.h"
#include "heap-io.h"
#include "smlnj-runtime.h"

/* blastIn : string -> 'a
 *
 * Build an ML object from a string.
 */
ML_object_t blastIn (ml_state_t *msp, idl_string s)
{
    bool_t	errFlg = FALSE;
    ml_val_t	obj;

    obj = BlastIn (msp, s, OBJ_LEN(s), &errFlg);

    if (errFlg)
	return RAISE_ERROR(msp, "blastIn");
    else
	return obj;

}

