#include <stdint.h>
#include <stdarg.h>
#include "node-i.h"

struct AffNode_s *
aff_node_cd(struct AffTree_s *tree,
	    struct AffSTable_s *stable,
	    struct AffNode_s *n,
	    int create,
	    ...)
{
    struct AffNode_s *res;
    va_list va;
    
    va_start(va, create);
    res = aff_node_cdv(tree, stable, n, create, va);
    va_end(va);

    return res;
}
