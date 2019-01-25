#include <stdint.h>
#include <string.h>
#include <stdarg.h>
#include "stable.h"
#include "node-i.h"
#include "tree-i.h"

struct AffNode_s *
aff_tree_lookup(const struct AffTree_s      *tt,
		const struct AffNode_s      *parent,
		const struct AffSymbol_s    *name)
{
    struct AffNode_s *n;
    struct Key_s k;

    if (tt == 0 || parent == 0 || name == 0)
	return 0;

    k.parent = parent;
    k.name = name;

    /* must agree with lookup() */
    for (n = tt->tr_root; n;) {
	int cmp = memcmp(&k, &n->key, sizeof (struct Key_s));
	if (cmp == 0)
	    break;
	if (cmp < 0) {
	    n = n->left;
	} else {
	    n = n->right;
	}
    }
    return n;
}
