#include <stdint.h>
#include <string.h>
#include <stdio.h>
#include <stdarg.h>
#include "node.h"
#include "md5.h"
#include "io.h"
#include "aff-i.h"

#define BUFFER_SIZE 8192

int
aff_reader_check(struct AffReader_s *aff)
{
    struct AffMD5_s md5;
    uint8_t buffer[BUFFER_SIZE];
    uint8_t sum[16];
    uint64_t size;

    if (aff == 0 || aff->error)
	return 1;

    if (aff->file == 0) {
	aff->error = "AFF file is closed";
	aff->fatal_error = 1;
	return 1;
    }
    if (aff_file_setpos(aff->file, aff->data_hdr.start) != 0) {
	aff->error = "data positioning error";
	aff->fatal_error = 1;
	return 1;
    }
    aff_md5_init(&md5);
    for (size = 0; size < aff->data_hdr.size; ) {
	uint64_t tail = aff->data_hdr.size - size;
	uint32_t block = tail > BUFFER_SIZE? BUFFER_SIZE: (uint32_t)tail;
	if (fread(buffer, block, 1, aff->file) != 1) {
	    aff->error = "data reading error";
	    aff->fatal_error = 1;
	    return 1;
	}
	aff_md5_update(&md5, buffer, block);
	size += block;
    }
    aff_md5_final(sum, &md5);
    if (memcmp(sum, aff->data_hdr.md5, 16) != 0) {
	aff->error = "data checksum error";
	aff->fatal_error = 1;
	return 1;
    }
    return 0;
}
