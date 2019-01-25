#include <stdint.h>
#include <stdarg.h>
#include "node-i.h"

struct AffNode_s *
aff_node_cdv(struct AffTree_s *tree,
	     struct AffSTable_s *stable,
	     struct AffNode_s *n,
	     int create,
	     va_list va)
{
    const char *name;

    for (;;) {
	name = va_arg(va, const char *);
	if (name == 0)
	    return n;
	n = aff_node_chdir(tree, stable, n, create, name);
    }
}
