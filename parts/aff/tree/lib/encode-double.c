#include <stdint.h>
#include <stdarg.h>
#include <float.h>
#include <limits.h>
#include <math.h>
#include "node.h"
#include "coding.h"

uint8_t *
aff_encode_double(uint8_t *buf, uint32_t size, double data)
{
    uint64_t s, e, m, p;

    switch(fpclassify(data)) {
    case FP_INFINITE:
	s = signbit(data)? 1: 0;
	e = DBL_MAX_EXP - DBL_MIN_EXP + 2;
	m = 0;
	break;
    case FP_NAN:
	s = 0;  /* There is no portable way to restore the sign of NaN */
	e = DBL_MAX_EXP - DBL_MIN_EXP + 2;
	m = (1ll << (DBL_MANT_DIG - 2));
	break;
    case FP_NORMAL:
	s = signbit(data)? 1: 0;
	e = ilogb(data);
	m = floor(scalbn(scalbn(fabs(data), -e) - 1, DBL_MANT_DIG - 1));
	e = e - DBL_MIN_EXP + 2;
	break;
    case FP_SUBNORMAL:
	s = signbit(data)? 1: 0;
	e = 1;
	m = floor(scalbn(fabs(data), -DBL_MIN_EXP + DBL_MANT_DIG));
	break;
    case FP_ZERO:
	s = signbit(data)? 1: 0;
	e = 0;
	m = 0;
	break;
    default:
	return 0;
    }
    p = (s << (sizeof (double) * CHAR_BIT - 1)) | (e << (DBL_MANT_DIG - 1)) | m;

    return aff_encode_u64(buf, size, p);
}
