#include <stdint.h>
#include <stdlib.h>
#include "alloc.h"
#include "treap-i.h"

struct AffTreap_s *
aff_treap_init(void)
{
    struct AffTreap_s *h = aff_realloc(NULL, sizeof (struct AffTreap_s));
    if (h == 0)
        return 0;

    h->state = RINIT;
    h->root = 0;

    return h;
}
