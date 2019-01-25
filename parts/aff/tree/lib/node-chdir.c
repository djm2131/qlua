#include <stdint.h>
#include <stdarg.h>
#include "stable.h"
#include "node.h"
#include "tree.h"
#include "node-i.h"

struct AffNode_s *
aff_node_chdir(struct AffTree_s *tree,
	       struct AffSTable_s *stable,
	       struct AffNode_s *n,
	       int create,
	       const char *p)
{
    const struct AffSymbol_s *sym;
    struct AffNode_s *ch;

    if (tree == 0 || stable == 0 || n == 0)
	return 0;
    if (p == 0)
	return n;

    sym = aff_stable_lookup(stable, p);
    if (sym == 0 && create) {
	sym = aff_stable_insert(stable, p);
    }
    if (sym == 0)
	return 0;
    
    ch = aff_tree_lookup(tree, n, sym);
    if (ch == 0 && create) {
	ch = aff_tree_insert(tree, n, sym);
    }
    return ch;
}
