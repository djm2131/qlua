#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <stdarg.h>
#include "node.h"
#include "md5.h"
#include "io.h"
#include "coding.h"
#include "aff-i.h"

int
aff_node_get_double(struct AffReader_s *aff,
		    struct AffNode_s *n,
		    double *d,
		    uint32_t s)
{
    uint8_t buf[sizeof (double)];
    uint32_t i;
    uint32_t size;
    uint64_t offset;

    if (aff == 0 || aff->error)
	return 1;
    if (aff->file == 0) {
	aff->error = "NULL file in aff_get_node_double()";
	aff->fatal_error = 1;
	return 1;
    }
    if (n == 0) {
	aff->error = "NULL node in aff_get_node_double()";
	return 1;
    }
    if (d == 0) {
	aff->error = "NULL data in aff_get_node_double()";
	return 1;
    }
    if (aff_node_type(n) != affNodeDouble) {
	aff->error = "Reading a double[] from wrong node";
	return 1;
    }
    if (s == 0) {
	return 0;
    }
    size = aff_node_size(n);
    offset = aff_node_offset(n);
    if (aff_file_setpos(aff->file, offset) != 0) {
	aff->error = "AFF positioning error";
	return 1;
    }
    if (size > s)
	size = s;
    for (i = 0; i < size; i++) {
	if (fread(buf, sizeof (buf), 1, aff->file) != 1) {
	    aff->error = strerror(errno);
	    return 1;
	}
	if (aff_decode_double(&d[i], buf, sizeof(buf)) == 0) {
	    aff->error = "double decoding error";
	    return 1;
	}
    }
    return 0;
}
