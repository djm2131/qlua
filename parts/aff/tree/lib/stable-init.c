#include <stdint.h>
#include <stdlib.h>
#include "alloc.h"
#include "stable-i.h"

static struct AffSTable_s *
alloc_stable(uint64_t size)
{
    size_t s = (size_t)(sizeof (struct AffSTable_s)
                        + (size - 1) * sizeof (struct AffSymbol_s));
    if (size != (1 + ((s - sizeof (struct AffSTable_s))
                      / sizeof (struct AffSymbol_s))))
        return 0;

    return aff_realloc(NULL, s);
}

struct AffSTable_s *
aff_stable_init(uint64_t size)
{
    struct AffSTable_s *st;

    if (size == 0) size = BLOCK_SIZE;
    if (size < 2) size = 2;

    st = alloc_stable(size);
    if (st == 0) {
        size = BLOCK_SIZE;
        st = alloc_stable(size);
    }

    if (st == 0)
        return 0;

    st->tr_state = RINIT;
    st->tr_root = 0;
    st->size = 0;
    st->last_block = &st->block;
    aff_stable_iblock(&st->block, 0, size);

    return st;
}
