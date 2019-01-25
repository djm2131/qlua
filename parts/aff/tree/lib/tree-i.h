#ifndef MARK_9bd92b39_c568_4256_867b_2febdcaed2d2
#define MARK_9bd92b39_c568_4256_867b_2febdcaed2d2
#include "tree.h"
#include "treap-i.h"

#ifdef AFF_DEBUG
#define BLOCK_SIZE 4 /* The minimal size for 10% overhead scaling */
#else
#define BLOCK_SIZE 1024
#endif /* defined(AFF_DEBUG) */

struct Block_s {
    struct Block_s       *next;
    uint64_t              start;
    int                   used;
    int                   size;
    struct AffNode_s      node[1]; /* more data follows. Must be the last. */
};

struct AffTree_s {
    struct AffNode_s     *tr_root;
    uint32_t              tr_state;
    uint64_t              size;
    uint64_t              file_size;
    struct AffNode_s      root;
    struct Block_s       *last_block;    
    struct Block_s        block; /* This must be the last element */
};

void aff_tree_iblock(struct Block_s *block, int start, int size);

#endif /* !defined(MARK_9bd92b39_c568_4256_867b_2febdcaed2d2) */
