/* date.c
 *
 * COPYRIGHT (c) 2001 Bell Labs, Lucent Technologies.
 *
 * Date support for SML'97 Basis.
 */

#include <time.h>
#include "ml-base.h"
#include "sml-basis.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "vproc-state.h"
#include "ml-state.h"
#include "ml-timer.h"
#include "ml-c.h"

#define DATE_LEN	24	/* we discard the trailing \n\0 */

#define COPY_TM(DATE, TM) do {				\
	(DATE)->tm_sec		= (TM)->tm_sec;		\
	(DATE)->tm_min		= (TM)->tm_min;		\
	(DATE)->tm_hour		= (TM)->tm_hour;	\
	(DATE)->tm_mday		= (TM)->tm_mday;	\
	(DATE)->tm_mon		= (TM)->tm_mon;		\
	(DATE)->tm_year		= (TM)->tm_year;	\
	(DATE)->tm_wday		= (TM)->tm_wday;	\
	(DATE)->tm_yday		= (TM)->tm_yday;	\
	(DATE)->tm_isdst	= (TM)->tm_isdst;	\
    } while(0)

/* gmTime:
 */
void gmTime (Time_t *t, Date_t *date)
{
    time_t	tim = t->seconds;
    struct tm	*tm;

    tm = gmtime (&tim);
    COPY_TM (date, tm);

} /* end of gmTime */

/* localTime:
 */
void localTime (Time_t *t, Date_t *date)
{
    time_t	tim = t->seconds;
    struct tm	*tm;

    tm = localtime (&tim);
    COPY_TM (date, tm);

} /* end of localTime */

/* ascTime : {tm_sec : int, tm_min : int, tm_hour : int, tm_mday : int, tm_mon : int,
              tm_year : int, tm_wday : int, tm_yday : int, tm_isdst : int} -> string
 *
 * This takes a nine-tuple date (fields sec, min, hour, mday, mon, year, wday,
 * yday, and isdst), and converts it into a string representation.
 */
ML_string_t ascTime (ml_state_t *msp, Date_t *date) 
{
    ml_val_t	res;
    struct tm	tm;

    memset (&tm, 0, sizeof(tm));
    COPY_TM (&tm, date);
    res = ML_AllocString(msp, DATE_LEN);
    strncpy (STR_MLtoC(res), asctime(&tm), DATE_LEN);

    return res;
}

/* strfTime :
 *    (string * {tm_sec : int, tm_min : int, tm_hour : int, tm_mday : int, tm_mon : int,
                 tm_year : int, tm_wday : int, tm_yday : int, tm_isdst : int}) -> string
 *
 * This takes a format field and nine integer fields (sec, min, hour, mday, mon,
 * year, wday, yday, and isdst), and converts it into a string representation
 * according to the format string.
 */
ML_string_t strfTime (ml_state_t *msp, ML_string_t fmt, Date_t *date)
{
    ml_val_t	res;
    struct tm	tm;
    char	buf[512];
    size_t	sz;

    memset (&tm, 0, sizeof(tm));
    COPY_TM (&tm, date);

    sz = strftime (buf, sizeof(buf), STR_MLtoC(fmt), &tm);
    if (sz > 0) {
	res = ML_AllocString(msp, sz);
	strncpy (STR_MLtoC(res), buf, sz);
	return res;
    }
    else
	return RAISE_ERROR(msp, "strftime failed");
}


/* mkTime : {tm_sec : int, tm_min : int, tm_hour : int, tm_mday : int, tm_mon : int,
             tm_year : int, tm_wday : int, tm_yday : int, tm_isdst : int}
 *	-> Int32.int
 *
 * This takes a 9-tuple with the fields: tm_sec, tm_min, tm_hour, tm_mday,
 * tm_mon, tm_year, tm_wday, tm_yday, tm_isdst, and returns the corresponding
 * localtime value (in seconds).
 */
void mkTime (Date_t *date, Time_t *time)
{
    struct tm	tm;
    time_t	t;

    memset (&tm, 0, sizeof(tm));
    COPY_TM (&tm, date);

    t = mktime (&tm);

    /*  FIXME
    if (t < 0) {
	return RAISE_ERROR(msp, "Invalid date");
    }
    else {
	ml_val_t	res;

	INT32_ALLOC(msp, res, t);
	return res;
    }
    */
}
