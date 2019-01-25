#include <stdint.h>
#include "stable-i.h"

void
aff_stable_foreach(const struct AffSTable_s *st,
		   void (*proc)(const struct AffSymbol_s *sym, void *arg),
		   void *arg)
{
    const struct Block_s *b;
    uint32_t i;
    
    if (st == 0)
	return;

    for (b = &st->block; b; b = b->next) {
	for (i = 0; i < b->used; i++) {
	    proc(&b->symbol[i], arg);
	}
    }
}
