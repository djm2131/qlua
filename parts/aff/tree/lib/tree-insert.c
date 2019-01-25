#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include "stable.h"
#include "alloc.h"
#include "node-i.h"
#include "tree-i.h"

struct AffNode_s *
aff_tree_insert(struct AffTree_s         *tt,
                struct AffNode_s         *parent,
                const struct AffSymbol_s *name)
{
    struct Block_s *b;
    struct AffNode_s *n;
    struct Key_s k;
    int len = 0;

    if (tt == 0 || parent == 0 || name == 0)
        return 0;

    k.parent = parent;
    k.name = name;
    /* must agree with lookup() */
    for (n = tt->tr_root; n;) {
        int cmp = memcmp(&k, &n->key, sizeof (struct Key_s));
        if (cmp == 0)
            break;
        if (cmp < 0) {
            n = n->left;
        } else {
            n = n->right;
        }
    }
    if (n)
        return 0;

    if (tt->last_block->used == tt->last_block->size) {
        int size = (tt->last_block->size * 3) / 2;

        if (size <= tt->last_block->size)
            size = tt->last_block->size + 1;

        b = aff_realloc(NULL, sizeof (struct Block_s)
                        + size * sizeof (struct AffNode_s));
        if (b == 0)
            return 0;
        aff_tree_iblock(b, tt->size, size);
        tt->last_block->next = b;
        tt->last_block = b;
    }
    b = tt->last_block;
    n = &b->node[b->used];
    n->key = k;
    n->type = affNodeVoid;
    n->size = 0;
    n->offset = 0;
    n->id = tt->size;
    n->next = parent->children;
    parent->children = n;
    n->children = 0;
    /* must agree with lookup() */
    {
        uint32_t x;
        struct AffNode_s  *p;
        struct AffNode_s **l;
        struct AffNode_s **r;
        struct AffNode_s **w;
        x = n->hash = tt->tr_state += RSTEP;
        n->left = n->right = 0;
        for (p = tt->tr_root, w = &tt->tr_root; p && p->hash < x; p = *w) {
            int cmp = memcmp(&k, &p->key, sizeof (struct Key_s));
            if (cmp < 0) {
                w = &p->left;
            } else {
                w = &p->right;
            }
        }
        *w = n;
        l = &n->left;
        r = &n->right;
        while (p) {
            int cmp = memcmp(&k, &p->key, sizeof (struct Key_s));
            if (cmp < 0) {
                *r = p;
                r = &p->left;
                p = *r;
            } else {
                *l = p;
                l = &p->right;
                p = *l;
            }
        }
        *l = *r = 0;
    }
    b->used++;
    tt->size++;
    tt->file_size += len
        + sizeof (uint8_t)  /* type */
        + sizeof (uint32_t) /* nameId */
        + sizeof (uint64_t); /* parentId */

    return n;
}
