#include <stdint.h>
#include <stdlib.h>
#include "alloc.h"
#include "stable-i.h"

void *
aff_stable_fini(struct AffSTable_s *st)
{
    struct Block_s *bl;
    int i;

    if (st == 0)
        return 0;
    for (i = 0; i < st->block.used; i++)
        aff_realloc((void *)(st->block.symbol[i].name), 0);
    for (bl = st->block.next; bl;) {
        struct Block_s *n = bl->next;
        for (i = 0; i < bl->used; i++)
            aff_realloc((void *)(bl->symbol[i].name), 0);
        aff_realloc(bl, 0);
        bl = n;
    }
    aff_realloc(st, 0);

    return 0;
}
