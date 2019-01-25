#include <stdint.h>
#include <stdarg.h>
#include "node-i.h"

uint64_t
aff_node_offset(const struct AffNode_s *tn)
{
    if (tn == 0)
	return 0;
    return tn->offset;
}
