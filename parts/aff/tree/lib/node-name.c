#include <stdint.h>
#include <stdarg.h>
#include "node-i.h"

const struct AffSymbol_s *
aff_node_name(const struct AffNode_s *tn)
{
    if (tn == 0)
	return 0;
    return tn->key.name;
}
