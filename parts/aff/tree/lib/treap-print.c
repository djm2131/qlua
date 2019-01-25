#include <stdio.h>
#include <stdint.h>
#include "treap-i.h"

static void
print_data(const char *ptr, int len)
{
    printf(" %8p \"", ptr);
    for (;len;len--, ptr++) {
	unsigned int v = (*ptr) & 0xff;
	if ((v >= 32) && (v < 127))
	    printf("%c", v);
	else
	    printf("\\x%02x", v);
    }
    printf("\"");
}

static void
print(int l, struct Node_s *n, struct Node_s *r, int (*gs)(const void *))
{
    if (n) {
	print(l + 1, n->left, r, gs);
	printf("%c %3d %8p %08x [%8p %8p]",
	       n == r? '>': ' ', l, n, n->hash, n->left, n->right);
	print_data(n->key, n->key_size);
	print_data(n->value, gs(n->value));
	printf("\n");
	print(l + 1, n->right, r, gs);
    }
}


void
aff_treap_print(struct AffTreap_s *h, int (*get_vsize)(const void *))
{
    if (h == 0) {
	printf("NULL HASH\n");
	return;
    }
    printf("\nh=%p, state=%x, root=%p\n", h, h->state, h->root);
    print(1, h->root, h->root, get_vsize);
    printf("h end\n");
    
}
