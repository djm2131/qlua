#include <stdint.h>
#include <stdarg.h>
#include "node-i.h"

struct AffNode_s *
aff_node_parent(const struct AffNode_s *tn)
{
    if (tn == 0)
	return 0;
    return (struct AffNode_s *)tn->key.parent;
}
