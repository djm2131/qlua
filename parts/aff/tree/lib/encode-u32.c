#include <stdint.h>
#include <stdarg.h>
#include "node.h"
#include "coding.h"

uint8_t *
aff_encode_u32(uint8_t *buf, uint32_t size, uint32_t data)
{
    int i;

    if (buf == 0 || size < 4)
	return 0;

    for (i = 0; i < 4; i++, data >>= 8) {
	buf[3-i] = (uint8_t)(data & 0xff);
    }
    return buf + 4;
}
