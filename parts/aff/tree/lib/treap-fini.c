#include <stdint.h>
#include <stdlib.h>
#include "alloc.h"
#include "treap-i.h"

static void
treap_free(struct Node_s *n)
{
    if (n) {
        treap_free(n->left);
        treap_free(n->right);
        aff_realloc(n, 0);
    }
}

void *
aff_treap_fini(struct AffTreap_s *h)
{
    if (h) {
        treap_free(h->root);
        aff_realloc(h, 0);
    }

    return NULL;
}
