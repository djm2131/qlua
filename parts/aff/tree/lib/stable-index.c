#include <stdint.h>
#include "stable-i.h"

const struct AffSymbol_s *
aff_stable_index(const struct AffSTable_s *st, uint32_t index)
{
    const struct Block_s *b;

    if (st == 0)
	return 0;
    for (b = &st->block; b; b = b->next) {
	if (index < b->start + b->used)
	    return &b->symbol[index - b->start];
    }
    return 0;
}
