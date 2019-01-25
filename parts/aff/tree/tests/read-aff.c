#include <stdint.h>
#include <stdlib.h>
#include <stdarg.h>
#include <stdio.h>
#include <string.h>
#include <complex.h>
#include <math.h>
#include "stable.h"
#include "node.h"
#include "aff.h"

static const char *
type_name(enum AffNodeType_e t)
{
    static char buffer[100];

    switch (t) {
    case affNodeVoid:    return "v";
    case affNodeChar:    return "s";
    case affNodeInt:     return "i";
    case affNodeDouble:  return "d";
    case affNodeComplex: return "c";
    default:
	sprintf(buffer, "[t%d]", t);
	return buffer;
    }
}

static void
print_name(const char *name)
{
    if (name == 0) {
	printf("NULL\n");
	return;
    }
    printf("\"");
    for (;*name; name++) {
	unsigned char v = *name;
	if (v < 32 || v > 126 || v == '\\' || v == '\"')
	    printf("\\x%02x", v);
	else
	    printf("%c", v);
    }
    printf("\"\n");
}

static void
process_node(struct AffNode_s *n, void *arg)
{
    struct AffReader_s *f = arg;
    uint64_t n_id = aff_node_id(n);
    const char *n_name = aff_symbol_name(aff_node_name(n));
    enum AffNodeType_e n_type = aff_node_type(n);
    uint64_t n_parent = aff_node_id(aff_node_parent(n));
    uint32_t n_size = aff_node_size(n);

    printf("%016llx [%6d] %6s %016llx ",
	   (long long)n_parent, n_size, type_name(n_type), (long long)n_id);
    print_name(n_name);
    switch (n_type) {
    default:
	break;
    case affNodeChar: {
	char *t = malloc(n_size);
	uint32_t i;
	if (t == 0)
	    goto error;
	aff_node_get_char(f, n, t, n_size);
	printf("  String: \"");
	for (i = 0; i < n_size; i++) {
	    char v = t[i];
	    if (v < 32 || v > 126 || v == '\\' || v == '\"')
		printf("\\x%02x", v);
	    else
		printf("%c", v);
	}
	printf("\"\n");
	free(t);
	break;
    }
    case affNodeInt: {
	uint32_t *t = malloc(n_size * sizeof (uint32_t));
	uint32_t i;
	if (t == 0)
	    goto error;
	aff_node_get_int(f, n, t, n_size);
	for (i = 0; i < n_size; i++)
	    printf("   i[%5d]: %14d (%08x)\n", i, t[i], t[i]);
	free(t);
	break;
    }
    case affNodeDouble: {
	double *t = malloc(n_size * sizeof (double));
	uint32_t i;
	if (t == 0)
	    goto error;
	aff_node_get_double(f, n, t, n_size);
	for (i = 0; i < n_size; i++)
	    printf("   d[%5d]: %25.16e\n", i, t[i]);
	free(t);
	break;
    }
    case affNodeComplex: {
	double _Complex *t = malloc(n_size * sizeof (double _Complex));
	uint32_t i;
	if (t == 0)
	    goto error;
	aff_node_get_complex(f, n, t, n_size);
	for (i = 0; i < n_size; i++)
	    printf("   c[%5d]: %25.16e %25.16e\n", i, creal(t[i]), cimag(t[i]));
	free(t);
	break;
    }
    }
    aff_node_foreach(n, process_node, f);
    return;
error:
    fprintf(stderr, "*** not enough memory\n");
    exit(1);
}


static void
do_read(const char *file_name)
{
    struct AffNode_s *n;
    struct AffReader_s *f;
    const char *status;

    printf("\ntrying to read %s\n", file_name);
    f = aff_reader(file_name);
    status = aff_reader_errstr(f);
    if (status) {
	printf("reader open error: %s\n", status);
	aff_reader_close(f);
	return;
    }
    n = aff_reader_root(f);
    process_node(n, f);
    status = aff_reader_errstr(f);
    if (status) {
	printf("processing error: %s\n", status);
    }
    aff_reader_close(f);
}

int
main(int argc, char *argv[])
{
    int i;
    if (argc < 2) {
	fprintf(stderr, "usage: read-aff input ...\n");
	return 1;
    }
    for (i = 1; i < argc; i++) {
	do_read(argv[i]);
    }

    return 0;
}

