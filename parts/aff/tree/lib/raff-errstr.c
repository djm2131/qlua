#include <stdint.h>
#include <stdio.h>
#include "md5.h"
#include "aff-i.h"

const char *
aff_reader_errstr(struct AffReader_s *aff)
{
    if (aff == 0)
	return "AffReader is not allocated";
    return aff->error;
}
