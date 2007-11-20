/* set-time-array.c
 *
 * COPYRIGHT (c) 1996 AT&T Research.
 */

#include "ml-base.h"
#include "ml-c.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "ml-globals.h"
#include "smlnj-runtime.h"
#include "profile.h"

extern void EnableProfSignals (void);
extern void DisableProfSignals (void);

/* setTimeArray : word array option -> unit
 *
 * Set the profile array reference; NONE means that there is no array.
 */
void setTimeArray (ml_state_t *msp, ML_int_arr_opt_t prof_cnt_array)
{
#ifdef OPSYS_UNIX
    bool_t	enabled = (ProfCntArray != ML_unit);
    int		i;

    if (prof_cnt_array != OPTION_NONE) {
	ProfCntArray = OPTION_get(prof_cnt_array);
	if (! enabled) {
	  /* add ProfCntArray to the C roots */
	    CRoots[NumCRoots++] = &ProfCntArray;
	  /* enable profiling signals */
	    EnableProfSignals ();
	}
    }
    else if (enabled) {
      /* remove ProfCntArray from the C roots */
	for (i = 0;  i < NumCRoots;  i++) {
	    if (CRoots[i] == &ProfCntArray) {
		CRoots[i] = CRoots[--NumCRoots];
		break;
	    }
	}
      /* disable profiling signals */
	DisableProfSignals ();
	ProfCntArray = ML_unit;
    }

#else
    /*  FIXME: raise exn
    return RAISE_ERROR(msp, "time profiling not supported");
    */
#endif

}

