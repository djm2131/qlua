#include <stdint.h>
#include <stdio.h>
#include "tree.h"
#include "md5.h"
#include "aff-i.h"

struct AffTree_s *
aff_reader_tree(struct AffReader_s *aff)
{
    if (aff == 0)
	return 0;
    else
	return aff->tree;
}
