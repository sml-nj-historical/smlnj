/*! \file localoffset.c
 *
 * \author John Reppy
 *
 * Runtime support for determining the local offset in seconds from UTC.
 */

/*
 * COPYRIGHT (c) 2015 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 */

#  include "ml-osdep.h"
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
#include <time.h>
#include "ml-base.h"
#include "ml-objects.h"
#include "cfun-proto-list.h"

/* _ml_Date_localoffset : unit -> Int32.int
 *
 * Returns the offset from UTC of the current time in the local timezone.
 * This value reflects not only the geographical location of the host system, but
 * also daylight savings time (if it is in effect).
 */
ml_val_t _ml_Date_localoffset (ml_state_t *msp, ml_val_t arg)
{
    time_t	now, t;
    struct tm	*tm;
    int		isDST;
    ml_val_t	res;

  /* get the current time in seconds */
    now = time(NIL(time_t *));

  /* get the local timezone's daylight saving's time info */
    tm = localtime (&now);
    isDST = tm->tm_isdst;

  /* convert to UTC and local tm structs */
    tm = gmtime (&now);

  /* convert the UTC tm struct back into seconds using the local timezone info (including
   * the daylight savings time field from localTM).  The local offset will be the difference
   * between this value and now.
   */
    tm->tm_isdst = isDST;
    t = mktime (tm);

    INT32_ALLOC(msp, res, t - now);
    return res;

} /* end of _ml_Date_localoffset */
