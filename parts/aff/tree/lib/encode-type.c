#include <stdint.h>
#include <stdarg.h>
#include "node.h"
#include "coding.h"

uint8_t *
aff_encode_type(uint8_t *buf, uint32_t size, enum AffNodeType_e type)
{
    if (buf == 0 || size < 1)
	return 0;
    switch (type) {
    case affNodeVoid:    buf[0] = 1; break;
    case affNodeChar:    buf[0] = 2; break;
    case affNodeInt:     buf[0] = 3; break;
    case affNodeDouble:  buf[0] = 4; break;
    case affNodeComplex: buf[0] = 5; break;
    default:
	return 0;
    }
    return buf + 1;
}
