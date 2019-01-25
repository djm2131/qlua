#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include "alloc.h"
#include "stable-i.h"

const struct AffSymbol_s *
aff_stable_insert(struct AffSTable_s *st, const char *name)
{
    struct Block_s *b;
    struct AffSymbol_s *sym;
    int len;

    if (st == 0 || name == 0)
        return 0;

    len = strlen(name) + 1;
    /* must agree with stable_lookup() */
    for (sym = st->tr_root; sym;) {
        int cmp = strcmp(name, sym->name);
        if (cmp == 0)
            break;
        if (cmp < 0)
            sym = sym->left;
        else
            sym = sym->right;
    }
    if (sym)
        return sym;

    if (st->last_block->used == st->last_block->size) {
        int size = (st->last_block->size * 3) / 2;

        if (size <= st->last_block->size)
            size = st->last_block->size + 1;

        b = aff_realloc(NULL, sizeof (struct Block_s)
                        + (size - 1) * sizeof (struct AffSymbol_s));
        if (b == 0)
            return 0;
        aff_stable_iblock(b, st->size, size);
        st->last_block->next = b;
        st->last_block = b;
    }
    b = st->last_block;
    sym = &b->symbol[b->used];
    sym->name = aff_realloc(NULL, len);
    if (sym->name == 0)
        return 0;
    strcpy((char *)sym->name, name);
    sym->id = st->size;
    /* must agree with lookup code */
    {
        uint32_t x;
        struct AffSymbol_s *p;
        struct AffSymbol_s **l;
        struct AffSymbol_s **r;
        struct AffSymbol_s **w;

        x = sym->hash = st->tr_state += RSTEP;
        sym->left = sym->right = 0;
        for (p = st->tr_root, w = &st->tr_root; p && p->hash < x; p = *w) {
            int cmp = strcmp(name, p->name);
            if (cmp < 0) {
                w = &p->left;
            } else {
                w = &p->right;
            }
        }
        *w = sym;
        l = &sym->left;
        r = &sym->right;
        while (p) {
            int cmp = strcmp(name, p->name);
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
    st->size++;
    return sym;
}
