#include <stdint.h>
#include <stdarg.h>
#include "node.h"
#include "coding.h"

uint8_t *
aff_decode_type(enum AffNodeType_e *type, uint8_t *buf, uint32_t size)
{
    if (buf == 0 || size < 1)
	return 0;
    switch (buf[0]) {
    case 1: *type = affNodeVoid; break;
    case 2: *type = affNodeChar; break;
    case 3: *type = affNodeInt; break;
    case 4: *type = affNodeDouble; break;
    case 5: *type = affNodeComplex; break;
    default:
	return 0;
    }
    return buf + 1;
}
