#include <stdint.h>
#include <stdio.h>
#ifdef HAS_INTEGRAL_OFF_T
#include <sys/types.h>
#endif
#include <assert.h>
#include "io.h"

int
aff_file_setpos(FILE *f, uint64_t p)
{
#if defined(HAS_INTEGRAL_OFF_T) || defined(__APPLE__)
    off_t off = p;

    assert(sizeof (off_t) >=  sizeof (uint64_t));

    if (fseeko(f, off, SEEK_SET) != 0)
	return 1;

    return 0;
#else
#error No implementation for aff_file_setpos()
    return 1;
#endif
}
