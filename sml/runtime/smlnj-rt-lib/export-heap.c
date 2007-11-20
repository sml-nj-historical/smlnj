/* export-heap.c
 *
 * COPYRIGHT (c) 1995 by AT&T Bell Laboratories.
 */

#include "ml-osdep.h"
#include <stdio.h>
#include <string.h>
#include "ml-base.h"
#include "ml-values.h"
#include "ml-state.h"
#include "heap-io.h"
#include "ml-c.h"
#include "smlnj-runtime.h"

/* exportHeap : string -> bool
 *
 * Export the world to the given file and return false (the exported version
 * returns true).
 */
ML_bool_t exportHeap (ml_state_t *msp, idl_string arg)
{
    char	fname[1024];
    FILE	*file;
    int		sts;

    QualifyImageName (strcpy(fname, arg));

    if ((file = fopen(fname, "wb")) == NULL)
	return RAISE_ERROR(msp, "unable to open file for writing");

    msp->ml_arg = ML_true;
    sts = ExportHeapImage (msp, file);
    fclose (file);

    if (sts == SUCCESS)
	return ML_false;
    else
	return RAISE_ERROR(msp, "export failed");

}

