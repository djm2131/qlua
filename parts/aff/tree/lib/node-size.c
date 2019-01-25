#include <stdint.h>
#include <stdarg.h>
#include "node-i.h"

uint32_t
aff_node_size(const struct AffNode_s *tn)
{
    if (tn == 0)
	return 0;
    return tn->size;
}
