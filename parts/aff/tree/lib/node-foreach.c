#include <stdint.h>
#include <stdarg.h>
#include "node-i.h"


void
aff_node_foreach(struct AffNode_s *node,
		 void (*proc)(struct AffNode_s *node, void *arg),
		 void *arg)
{
    struct AffNode_s *ch;
    
    if (node == 0)
	return;

    for (ch = node->children; ch; ch = ch->next) {
	proc(ch, arg);
    }
}
