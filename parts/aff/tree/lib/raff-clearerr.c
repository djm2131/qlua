#include <stdint.h>
#include <stdio.h>
#include "md5.h"
#include "aff-i.h"

int
aff_reader_clearerr(struct AffReader_s *aff)
{
    if (aff == 0 || aff->fatal_error)
	return 1;
    aff->error = NULL;
    return 0;
}
