#include <stdint.h>
#include <stdarg.h>
#include "treap.h"
#include "stable.h"
#include "node-i.h"
#include "tree-i.h"

struct AffNode_s *
aff_tree_root(const struct AffTree_s *tt)
{
    if (tt == 0)
	return 0;
    return (struct AffNode_s *)&tt->root;
}
