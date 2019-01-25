#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include "alloc.h"
#include "treap-i.h"

int
aff_treap_insert(struct AffTreap_s *h, const void *key, int size, void *data)
{
    struct Node_s *q;
    struct Node_s *p;
    struct Node_s **w;
    struct Node_s **l;
    struct Node_s **r;
    uint32_t x;
    
    if (data == 0 || h == 0 || key == 0)
        return 1;
    
    for (p = h->root; p;) {
        int cmp = aff_treap_cmp(key, size, p->key, p->key_size);
        if (cmp == 0) {
            return 2;
        } else if (cmp < 0) {
            p = p->left;
        } else {
            p = p->right;
        }
    }
    q = aff_realloc(NULL, sizeof (struct Node_s));
    if (q == 0)
        return 1;
    q->hash = x = h->state += RSTEP; /* fibonaci pseudo-randoms */
    q->key = key;
    q->key_size = size;
    q->value = data;
    q->left = q->right = 0;
    for (p = h->root, w = &h->root; p && p->hash <= x; p = *w) {
        int cmp = aff_treap_cmp(key, size, p->key, p->key_size);
        if (cmp < 0) {
            w = &p->left;
        } else {
            w = &p->right;
        }
    }
    *w = q;
    l = &q->left;
    r = &q->right;
    while (p) {
        int cmp = aff_treap_cmp(key, size, p->key, p->key_size);
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
    return 0;
}
