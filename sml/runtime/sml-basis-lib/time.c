/* time.c
 *
 * COPYRIGHT (c) 2001 Bell Labs, Lucent Technologies.
 *
 * Time support for SML'97 Basis.
 */

#include "ml-osdep.h"
#if defined(HAS_GETTIMEOFDAY)
#  if defined(OPSYS_WIN32)
#    include <sys/types.h>
#    include <sys/timeb.h>
#  else
#    include <sys/time.h>
#  endif
#else
#  error no timeofday mechanism
#endif   
#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "vproc-state.h"
#include "ml-state.h"
#include "ml-timer.h"
#include "sml-basis.h"

/* now:
 */
void now (Time_t *t)
{
#ifdef HAS_GETTIMEOFDAY
#  if defined(OPSYS_UNIX)
    {
	struct timeval	tv;

	gettimeofday (&tv, NIL(struct timezone *));
	t->seconds = tv.tv_sec;
	t->uSeconds = tv.tv_usec;
    }
#  elif defined(OPSYS_WIN32)
  /* we could use Win32 GetSystemTime/SystemTimetoFileTime here,
   * but the conversion routines for 64-bit 100-ns values
   * (in the mapi dll) are non-Win32s
   *
   * we'll use time routines from the C runtime for now.
   */
    {
	struct _timeb tb;

	_ftime(&tb);
	t->seconds = tb.time;
	t->uSeconds = tb.millitm*1000;
    }
#  else
#    error timeofday not defined for OS
#  endif
#else
#  error no timeofday mechanism
#endif

} /* end of now */


/* getCPUTime:
 */
void getCPUTime (ml_state_t *msp, Time_t *u, Time_t *s, Time_t *g)
{
    vproc_state_t	*vsp = msp->ml_vproc;

    GetCPUTime (u, s);
    g->seconds = vsp->vp_gcTime->seconds;
    g->uSeconds = vsp->vp_gcTime->uSeconds;

} /* end of getCPUTime */

