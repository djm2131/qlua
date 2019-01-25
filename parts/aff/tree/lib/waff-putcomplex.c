#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <complex.h>
#include <stdarg.h>
#include "node.h"
#include "md5.h"
#include "coding.h"
#include "aff-i.h"

int
aff_node_put_complex(struct AffWriter_s *aff,
		    struct AffNode_s *n,
		    const double _Complex *d,
		    uint32_t s)
{
    uint32_t i;
    uint8_t buf[sizeof (double _Complex)];
    uint32_t size;
    uint8_t *ptr;

    if (aff == 0 || aff->error)
	return 1;
    if (n == 0) {
	aff->error = "NULL node in aff_node_put_complex()";
	return 1;
    }
    if (d == 0) {
	aff->error = "NULL data in aff_node_put_complex()";
	return 1;
    }
    if (aff_node_type(n) != affNodeVoid) {
	aff->error = "Changing the type of the node";
	return 1;
    }
    aff_node_assign(n, affNodeComplex, s, aff->position);
    for (i = 0; i < s; i++) {
	size = sizeof (buf);
	ptr = aff_encode_double(buf, sizeof(buf), creal(d[i]));
	ptr = aff_encode_double(ptr, sizeof(buf) - (ptr - buf), cimag(d[i]));
	if (ptr == 0) {
	    aff->error = "complex encoder failed";
	    aff->fatal_error = 1;
	    return 1;
	}
	if (fwrite(buf, ptr - buf, 1, aff->file) != 1) {
	    aff->error = strerror(errno);
	    aff->fatal_error = 1;
	    return 1;
	}
	aff_md5_update(&aff->data_hdr.md5, buf, ptr - buf);
	aff->data_hdr.size += ptr - buf;
	aff->position += ptr - buf;
    }
    aff->data_hdr.records++;
    return 0;
}
