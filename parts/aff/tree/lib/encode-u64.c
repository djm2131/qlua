#include <stdint.h>
#include <stdarg.h>
#include "node.h"
#include "coding.h"

uint8_t *
aff_encode_u64(uint8_t *buf, uint32_t size, uint64_t data)
{
    int i;

    if (buf == 0 || size < 8)
	return 0;

    for (i = 0; i < 8; i++, data >>= 8) {
	buf[7-i] = (uint8_t)(data & 0xff);
    }
    return buf + 8;
}
