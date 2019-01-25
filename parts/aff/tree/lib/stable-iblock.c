#include <stdint.h>
#include "stable-i.h"

void
aff_stable_iblock(struct Block_s *block, int start, int size)
{
    block->next = 0;
    block->start = start;
    block->used = 0;
    block->size = size;
}
