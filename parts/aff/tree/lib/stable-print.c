#include <stdint.h>
#include <stdio.h>
#include "stable-i.h"

static void
print_string(const struct AffSymbol_s *sym, void *arg)
{
    printf(" %08x: %s\n", aff_symbol_id(sym), aff_symbol_name(sym));
}

void
aff_stable_print(const struct AffSTable_s *st)
{
    if (st == 0) {
	printf("NULL String table\n");
	return;
    }
    printf("String table at %p, size %d:\n", st, st->size);
    aff_stable_foreach(st, print_string, 0);
    printf("String table end\n");
}
