#include <stdint.h>
#include <stdio.h>
#include "stable.h"
#include "tree.h"
#include "md5.h"
#include "aff-i.h"

struct AffNode_s *
aff_writer_root(struct AffWriter_s *aff)
{
    if (aff == 0)
	return 0;
    else
	return aff_tree_root(aff->tree);
}
