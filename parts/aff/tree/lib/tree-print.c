#include <stdint.h>
#include <stdio.h>
#include <stdarg.h>
#include "treap.h"
#include "stable.h"
#include "node-i.h"
#include "tree-i.h"

static void
print_node(struct AffNode_s *node, void *arg)
{
    printf(" %16p %016llx: [%16p %16p %16p %32s] ",
	   node, (long long)node->id,
	   node->children, node->next,
	   node->key.parent, aff_symbol_name(node->key.name));
    switch (node->type) {
    case affNodeInvalid:
	printf("unvalid node\n");
	break;
    case affNodeVoid:
	printf("void\n");
	break;
    case affNodeChar:
	printf("char[%d] %016lld\n", node->size, (long long)node->offset);
	break;
    case affNodeInt:
	printf("int[%d] %016lld\n", node->size, (long long)node->offset);
	break;
    case affNodeDouble:
	printf("double[%d] %016lld\n", node->size, (long long)node->offset);
	break;
    case affNodeComplex:
	printf("complex[%d] %016lld\n", node->size, (long long)node->offset);
	break;
    default:
	printf("unknown type %d\n", node->type);
	break;
    }
}

void
aff_tree_print(struct AffTree_s *tt)
{
    if (tt == 0) {
	printf("NULL Tree table\n");
	return;
    }
    printf("Tree table at %p, size=%lld, file-size=%lld:\n",
	   tt, (long long)tt->size, (long long)tt->file_size);
    aff_tree_foreach(tt, print_node, 0);
    printf("Tree table end\n");
}
