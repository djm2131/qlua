#include <stdint.h>
#include <stdlib.h>
#include <stdarg.h>
#include "stable.h"
#include "alloc.h"
#include "node-i.h"
#include "tree-i.h"

static struct AffTree_s *
alloc_tree(uint64_t size)
{
    size_t s = (size_t)(sizeof (struct AffTree_s)
                        + (size - 1) * sizeof (struct AffNode_s));
    if (size != (1 + ((s - sizeof (struct AffTree_s))
                      / sizeof (struct AffNode_s))))
        return 0;

    return aff_realloc(NULL, s);
}


struct AffTree_s *
aff_tree_init(struct AffSTable_s *stable, uint64_t size)
{
    struct AffTree_s *tt;

    if (size == 0) size = BLOCK_SIZE;
    if (size < 2) size = 2;

    tt = alloc_tree(size);
    if (tt == 0) {
        size = BLOCK_SIZE;
        tt = alloc_tree(size);
    }

    if (tt == 0)
        return 0;

    tt->tr_state = RSTEP;
    tt->tr_root = 0;
    tt->size = 1;
    tt->file_size = 0;
    tt->last_block = &tt->block;
    tt->root.type = affNodeVoid;
    tt->root.key.parent = &tt->root;
    tt->root.key.name = aff_stable_insert(stable, "");
    tt->root.id = 0;
    tt->root.size = 0;
    tt->root.offset = 0;
    tt->root.next = 0;
    tt->root.children = 0;
    aff_tree_iblock(&tt->block, 1, size);
    return tt;
}
