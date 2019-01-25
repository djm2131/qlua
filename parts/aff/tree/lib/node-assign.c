#include <stdint.h>
#include <stdarg.h>
#include "node-i.h"

int
aff_node_assign(struct AffNode_s *node,
		enum AffNodeType_e type,
		uint32_t size,
		uint64_t offset)
{
    if (node == 0)
	return 1;
    if (node->type != affNodeVoid)
	return 2;
    if (type == affNodeVoid)
	return 0;
    node->type = type;
    node->size = size;
    node->offset = offset;
    return 0;
}
