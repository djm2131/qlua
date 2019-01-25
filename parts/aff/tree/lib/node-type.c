#include <stdint.h>
#include <stdarg.h>
#include "node-i.h"


enum AffNodeType_e
aff_node_type(const struct AffNode_s *tn)
{
    if (tn == 0)
	return affNodeInvalid;
    return tn->type;
}
