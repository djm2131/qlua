#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "lhpc-aff.h"
#include "util.h"

struct arg {
    struct AffReader_s *r;
    const char *name;
    int result;
};

static int verbose = 0;

static void
check_node(struct AffNode_s *node, void *ptr)
{
    const char *name;
    struct arg *arg = ptr;
    const struct AffSymbol_s *sym;
    enum AffNodeType_e type;
    uint32_t size;

    if (node == 0 || arg->result) {
        fprintf(stderr, "lhpc-aff: error processing %s: %s\n",
                arg->name, aff_reader_errstr(arg->r));
        arg->result = 1;
        return;
    }
    if (aff_node_parent(node) == NULL) {
        fprintf(stderr, "lhpc-aff: error processing %s: missing parent\n",
                arg->name);
        arg->result = 1;
        return;
    }
    if (aff_node_id(node) == (uint64_t)-1) {
        fprintf(stderr, "lhpc-aff: error processing %s: unexpected node id\n",
                arg->name);
        arg->result = 1;
        return;
    }
    sym = aff_node_name(node);
    if (sym == 0) {
        fprintf(stderr, "lhpc-aff: error processing %s: NULL node name\n",
                arg->name);
        arg->result = 1;
        return;
    }
    if (aff_symbol_id(sym) == (uint32_t)-1) {
        fprintf(stderr, "lhpc-aff: error processing %s: missing symbol ID\n",
                arg->name);
        arg->result = 1;
        return;
    }
    name = aff_symbol_name(sym);
    if (name == 0) {
        fprintf(stderr, "lhpc-aff: error processing %s: missing symbol name\n",
                arg->name);
        arg->result = 1;
        return;
    }
    if (aff_reader_namecheck(arg->r, name)) {
        fprintf(stderr, "lhpc-aff: error processing %s: bad node name %s\n",
                arg->name, name);
        arg->result = 1;
        return;
    }
#if 0 /* XXX */
    switch (arg->r->version) {
    case 2: status = aff_name_check2(name); break;
    case 2: status = aff_name_check2(name); break;
    default:
        fprintf(stderr, "lhpc-aff: INTERNAL ERROR: AffReader->version = %d\n", arg->r->version);
        arg->result = 1;
        return;
    }
    if (status != 0) {
        fprintf(stderr, "lhpc-aff: error processing %s: bad node name %s\n",
                arg->name, name);
        arg->result = 1;
        return;
    }
#endif /* XXX */
    type = aff_node_type(node);
    size = aff_node_size(node);
    switch (type) {
    case affNodeVoid:
        break;
    case affNodeChar: {
        char *ptr = malloc(sizeof (char) * size);
        if (ptr == 0) {
            fprintf(stderr, "lhpc-aff: not enough memory to check %s\n",
                    arg->name);
            arg->result = 2;
            return;
        }
        if (aff_node_get_char(arg->r, node, ptr, size)) {
            fprintf(stderr, "lhpc-aff: error processing %s:"
                    " char[%d] data access failed\n",
                    arg->name, size);
            arg->result = 1;
        }
        free(ptr);
        break;
    }
    case affNodeInt: {
        uint32_t *ptr = malloc(sizeof (uint32_t) * size);
        if (ptr == 0) {
            fprintf(stderr, "lhpc-aff: not enough memory to check %s\n",
                    arg->name);
            arg->result = 2;
            return;
        }
        if (aff_node_get_int(arg->r, node, ptr, size)) {
            fprintf(stderr, "lhpc-aff: error processing %s:"
                    " int[%d] data access failed\n",
                    arg->name, size);
            arg->result = 1;
        }
        free(ptr);
        break;
    }
    case affNodeDouble: {
        double *ptr = malloc(sizeof (double) * size);
        if (ptr == 0) {
            fprintf(stderr, "lhpc-aff: not enough memory to check %s\n",
                    arg->name);
            arg->result = 2;
            return;
        }
        if (aff_node_get_double(arg->r, node, ptr, size)) {
            fprintf(stderr, "lhpc-aff: error processing %s:"
                    " double[%d] data access failed\n",
                    arg->name, size);
            arg->result = 1;
        }
        free(ptr);
        break;
    }
    case affNodeComplex: {
        double _Complex *ptr = malloc(sizeof (double _Complex) * size);
        if (ptr == 0) {
            fprintf(stderr, "lhpc-aff: not enough memory to check %s\n",
                    arg->name);
            arg->result = 2;
            return;
        }
        if (aff_node_get_complex(arg->r, node, ptr, size)) {
            fprintf(stderr, "lhpc-aff: error processing %s:"
                    " complex[%d] data access failed\n",
                    arg->name, size);
            arg->result = 1;
        }
        free(ptr);
        break;
    }
    default:
        fprintf(stderr, "lhpc-aff: processing %s: unknown node type %d\n",
                arg->name, (int)type);
    }
    aff_node_foreach(node, check_node, arg);
}

static int
check_file(const char *name)
{
    struct arg arg;
    struct AffReader_s *r = aff_reader(name);
    const char *status = aff_reader_errstr(r);
    int ret = 1;

    if (status != 0) {
        fprintf(stderr, "lhpc-aff: failed on %s: %s\n", name, status);
        goto end;
    }
    if (verbose) {
        printf("lhpc-aff: file %s opened\n", name);
    }
    if (aff_reader_check(r)) {
        fprintf(stderr, "lhpc-aff: data checksum failed on %s\n", name);
        goto end;
    }
    arg.r = r;
    arg.name = name;
    arg.result = 0;
    aff_node_foreach(aff_reader_root(r), check_node, &arg);
    ret = arg.result;
    if (verbose && ret == 0) {
        printf("lhpc-aff: done processing %s\n", name);
    }
end:
    aff_reader_close(r);
    return ret;
}

void
h_check(void)
{
    printf("lhpc-aff check [-v] file ...\n"
      "\tChecks integrity of each file. If an error is found, the program\n"
      "\tprints a message on the stderr and exits with a non-zero status.\n"
      "\tIf -v is given, details of the checking are printed on stdout.\n");
}

int
x_check(int argc, char *argv[])
{
    if (argc < 1) {
    error:
        fprintf(stderr, "usage: lhpc-aff check [-v] file...\n");
        return 1;
    }
    if (strcmp(argv[0], "-v") == 0) {
        verbose = 1;
        if (argc == 1)
            goto error;
        argc--;
        argv++;
    }
    for (; argc; argc--, argv++) {
        if (check_file(argv[0])) {
            fprintf(stderr, "lhpc-aff: checking file %s failed\n", argv[0]);
            return 1;
        }
    }
    return 0;
}
