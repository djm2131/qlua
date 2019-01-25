#include <stdint.h>
#include <stdarg.h>
#include "stable.h"
#include "node-i.h"
#include "tree-i.h"

void
aff_tree_iblock(struct Block_s *block, int start, int size)
{
    block->next = 0;
    block->start = start;
    block->used = 0;
    block->size = size;
}
