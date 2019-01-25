#include <stdint.h>
#include <stdarg.h>
#include "stable.h"
#include "node-i.h"
#include "tree-i.h"

void
aff_tree_foreach(const struct AffTree_s *tt,
		 void (*proc)(struct AffNode_s *node, void *arg),
		 void *arg)
{
    const struct Block_s *b;
    uint32_t i;
    
    if (tt == 0)
	return;

    for (b = &tt->block; b; b = b->next) {
	for (i = 0; i < b->used; i++) {
	    proc((struct AffNode_s *)&b->node[i], arg);
	}
    }
}
