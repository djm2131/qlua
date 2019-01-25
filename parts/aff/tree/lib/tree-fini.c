#include <stdint.h>
#include <stdlib.h>
#include <stdarg.h>
#include "stable.h"
#include "alloc.h"
#include "node-i.h"
#include "tree-i.h"

void *
aff_tree_fini(struct AffTree_s *tt)
{
    struct Block_s *bl;

    if (tt == 0)
        return 0;
    for (bl = tt->block.next; bl;) {
        struct Block_s *n = bl->next;
        aff_realloc(bl, 0);
        bl = n;
    }
    aff_realloc(tt, 0);

    return 0;
}
