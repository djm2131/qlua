#include <stdint.h>
#include <string.h>
#include "stable-i.h"

const struct AffSymbol_s *
aff_stable_lookup(const struct AffSTable_s *st, const char *name)
{
    struct AffSymbol_s *sym;

    if (st == 0 || name == 0)
	return 0;

    /* must agree with stable-insert() */
    for (sym = st->tr_root; sym; ) {
	int cmp = strcmp(name, sym->name);
	if (cmp == 0)
	    break;
	if (cmp < 0)
	    sym = sym->left;
	else
	    sym = sym->right;
    }
    return sym;
}
