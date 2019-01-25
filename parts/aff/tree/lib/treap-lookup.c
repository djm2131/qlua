#include <stdint.h>
#include <string.h>
#include "treap-i.h"

void *
aff_treap_lookup(const struct AffTreap_s *h, const void *key, int size)
{
    struct Node_s *n;

    if (h == 0)
	return 0;

    for (n = h->root; n;) {
	int cmp = aff_treap_cmp(key, size, n->key, n->key_size);

	if (cmp == 0) {
	    return n->value;
	} else if (cmp < 0) {
	    n = n->left;
	} else {
	    n = n->right;
	}
    }
    return 0;
}
