#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include "stable.h"
#include "node-i.h"
#include "tree-i.h"

struct AffNode_s *
aff_tree_index(const struct AffTree_s *tt, uint64_t index)
{
    const struct Block_s *b;

    if (tt == 0)
	return 0;
    if (index == 0)
	return (struct AffNode_s *)&tt->root;
    for (b = &tt->block; b; b = b->next) {
	if (index < b->start + b->used)
	    return (struct AffNode_s *)&b->node[index - b->start];
    }
    return 0;
}
