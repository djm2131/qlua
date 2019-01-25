#include <stdint.h>
#include <stdio.h>
#include "md5.h"
#include "aff-i.h"

const char *
aff_writer_errstr(struct AffWriter_s *aff)
{
    if (aff == 0)
	return "AffWriter is not allocated";
    else
	return aff->error;
}
