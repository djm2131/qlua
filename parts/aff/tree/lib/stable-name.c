#include <stdint.h>
#include "stable-i.h"

const char *
aff_symbol_name(const struct AffSymbol_s *sym)
{
    if (sym == 0)
	return 0;
    return sym->name;
}
