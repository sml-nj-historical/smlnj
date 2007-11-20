/* set-timer.c
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * NOTE: this implementation is UNIX-specific right now; I would like to
 * define an OS abstraction layer for interval timers, which would cover
 * both alarm timers and profiling, but I need to look at what other systems
 * do first.
 */

#ifdef OPSYS_UNIX
#  include "ml-unixdep.h"
#  include <sys/time.h>
#endif
#include "ml-base.h"
#include "ml-c.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "smlnj-runtime.h"
#include "profile.h"

/* setTimer : bool -> unit
 *
 * Turn the profile timer on/off.
 */
void setTimer (ml_state_t *msp, ML_bool_t arg)
{
#ifdef HAS_SETITIMER
    struct itimerval	new_itv;
    int			sts;

    if (arg == ML_false) {
	new_itv.it_interval.tv_sec	=
	new_itv.it_value.tv_sec		=
	new_itv.it_interval.tv_usec	=
	new_itv.it_value.tv_usec	= 0;
    }
    else if (ProfCntArray == ML_unit) {
        /*  FIXME: raise exn
	return RAISE_ERROR(msp, "no count array set");
	*/
    }
    else {
	new_itv.it_interval.tv_sec	=
	new_itv.it_value.tv_sec		= 0;
	new_itv.it_interval.tv_usec	=
	new_itv.it_value.tv_usec	= PROFILE_QUANTUM_US;
    }

    sts = setitimer (ITIMER_VIRTUAL, &new_itv, NIL(struct itimerval *));

    /*  FIXME: check and raise exn
    CHK_RETURN_UNIT(msp, sts);
    */

#else
    /*  FIXME: raise exn
    return RAISE_ERROR(msp, "time profiling not supported");
    */
#endif

}

