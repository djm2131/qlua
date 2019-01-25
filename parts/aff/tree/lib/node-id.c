#include <stdint.h>
#include <stdarg.h>
#include "node-i.h"

uint64_t
aff_node_id(const struct AffNode_s *tn)
{
    if (tn == 0)
	return -1;
    return tn->id;
}
