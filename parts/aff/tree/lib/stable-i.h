#ifndef MARK_ccee59e4_4306_46b9_a6f0_df315a0233fe
#define MARK_ccee59e4_4306_46b9_a6f0_df315a0233fe
#include "stable.h"
#include "treap-i.h"

#ifdef AFF_DEBUG
#define BLOCK_SIZE 4 /* The minimal size for 10% overhead scaling */
#else
#define BLOCK_SIZE 1024
#endif /* defined(AFF_DEBUG) */

struct AffSymbol_s {
    struct AffSymbol_s  *left;
    struct AffSymbol_s  *right;
    uint32_t             hash;
    const char          *name;
    uint32_t             id;
};

struct Block_s {
    struct Block_s     *next;
    uint32_t            start;
    uint32_t            used;
    uint32_t            size;
    struct AffSymbol_s  symbol[1]; /* must be the last. More data follow */
};

struct AffSTable_s {
    struct AffSymbol_s *tr_root;
    uint32_t            tr_state;
    uint32_t            size;
    struct Block_s     *last_block;
    struct Block_s      block; /* must be the last. */
};

void aff_stable_iblock(struct Block_s *block, int start, int size);


#endif /* !defined(MARK_ccee59e4_4306_46b9_a6f0_df315a0233fe) */

