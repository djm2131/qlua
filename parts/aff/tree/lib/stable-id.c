#include <stdint.h>
#include "stable-i.h"

uint32_t
aff_symbol_id(const struct AffSymbol_s *sym)
{
    if (sym == 0)
	return 0xffffffff;
    return sym->id;
}
