#include <stdint.h>
#include <stdarg.h>
#include <float.h>
#include <limits.h>
#include <math.h>
#include <stdarg.h>
#include "node.h"
#include "coding.h"

uint8_t *
aff_decode_double(double *data, uint8_t *buf, uint32_t size)
{
    const uint64_t me = ((1ull<<(sizeof (uint64_t)*CHAR_BIT-1))-1);
    uint64_t s, e, m, p;
    double v;
    uint8_t *ptr;

    ptr = aff_decode_u64(&p, buf, size);
    if (ptr == 0)
	return 0;

    m = p & ((1ull << (DBL_MANT_DIG - 1)) - 1);
    e = (p & me) >> (DBL_MANT_DIG-1);
    s = (p >> (sizeof (uint64_t) * CHAR_BIT-1))? 1: 0;

    switch (e) {
    case DBL_MAX_EXP - DBL_MIN_EXP + 2:
	if (m == 0) {
	    if (s)
		*data = -INFINITY;
	    else
		*data = INFINITY;
	} else {
	    *data = NAN;
	}
	break;
    case 1:
	v = scalbn((double)m, DBL_MIN_EXP - DBL_MANT_DIG);
	if (s) {
	    *data = -v;
	} else {
	    *data = v;
	}
	break;
    case 0:
	if (s) {
	    *data = -0.0;
	} else {
	    *data = +0.0;
	}
	break;
    default:
	e = e + DBL_MIN_EXP - 2;
	v = scalbn(scalbn((double)m, -DBL_MANT_DIG+1) + 1.0, e);
	if (s) {
	    *data = -v;
	} else {
	    *data = v;
	}
	break;
    }
    return ptr;
}
