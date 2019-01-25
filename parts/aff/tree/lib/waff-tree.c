#include <stdint.h>
#include <stdio.h>
#include "tree.h"
#include "md5.h"
#include "aff-i.h"

struct AffTree_s *
aff_writer_tree(struct AffWriter_s *aff)
{
    if (aff == 0)
	return 0;
    else
	return aff->tree;
}
