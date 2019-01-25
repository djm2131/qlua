#include <stdint.h>
#include <stdio.h>
#include "md5.h"
#include "aff-i.h"

int
aff_writer_clearerr(struct AffWriter_s *aff)
{
    if (aff == 0 || aff->fatal_error)
	return 1;
    aff->error = NULL;
    return 0;
}
