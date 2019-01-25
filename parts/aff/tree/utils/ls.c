#include <stdio.h>
#include <complex.h>
#include <stdlib.h>
#include <string.h>
#include "lhpc-aff.h"
#include "util.h"
#include "common.h"

static int long_format = 0;
static int recursive = 0;
static int count_subnodes = 0; 
static int directory_only = 0; 

struct arg {
    struct AffReader_s *r;
    const char *fname;
    const char *kpath;
    struct AffNode_s *root;
};

struct node_count_arg {
    struct AffReader *r;
    size_t cnt;
};

static void
node_count(struct AffNode_s *node, void *arg_)
{
    struct node_count_arg *arg = (struct node_count_arg *)arg_;
    arg->cnt++;
    aff_node_foreach(node, node_count, arg_);
}
#if 0
static char *
xstrdup(const char *n)
{
    char *p = malloc(strlen(n) + 1);
    if (p == 0) {
	fprintf(stderr, "lhpc-aff: non enough memory\n");
	exit(1);
    }
    strcpy(p, n);
    return p;
}
#endif
static char *
type_name(enum AffNodeType_e type)
{
    switch (type) {
    case affNodeVoid: return "void";
    case affNodeChar: return "char";
    case affNodeInt: return "int";
    case affNodeDouble: return "double";
    case affNodeComplex: return "complex";
    default:
	fprintf(stderr, "lhpc-aff: Unknown node type %d\n", type);
	exit(1);
    }
}

static void
short_list(struct AffNode_s *node, void *ptr)
{
    const char *name = aff_symbol_name(aff_node_name(node));
    uint32_t size = aff_node_size(node);
    enum AffNodeType_e type = aff_node_type(node);
    char buffer[128];

    sprintf(buffer, "%s[%d]", type_name(type), size);
    printf("  %-15s %s\n", buffer, name);
}

enum affNodeTypeMask {
    affNodeVoidMask = 0x1,
    affNodeCharMask = 0x2,
    affNodeIntMask  = 0x4,
    affNodeDoubleMask   = 0x8,
    affNodeComplexMask  = 0x10
};
#define aff_ls_mask_all (affNodeVoidMask | affNodeCharMask |\
                        affNodeIntMask | affNodeDoubleMask |\
                        affNodeComplexMask)
enum affNodeTypeMask aff_ls_mask = aff_ls_mask_all;

static void
do_node(struct AffNode_s *node, void *ptr)
{
    struct arg *arg = ptr;
    enum AffNodeType_e type = aff_node_type(node);
    uint32_t size = aff_node_size(node);
    enum affNodeTypeMask node_mask;

    if (node == arg->root)
	printf("/");
    else {
	print_path(arg->root, node);
    }
    switch(type)
    {
    case affNodeVoid:   node_mask = affNodeVoidMask;    break;
    case affNodeChar:   node_mask = affNodeCharMask;    break;
    case affNodeInt:    node_mask = affNodeIntMask;     break;
    case affNodeDouble: node_mask = affNodeDoubleMask;  break;
    case affNodeComplex:node_mask = affNodeComplexMask; break;
    default:
        fprintf(stderr, "lhpc-aff: Internal error: uknown node type %d\n",
                type);
        exit(1);
    }
    printf(":  %s[%d]", type_name(type), size);
    if (count_subnodes && !recursive)
    {
        struct node_count_arg cnt_arg;
        cnt_arg.cnt = 0;
        aff_node_foreach(node, node_count, &cnt_arg);
        printf("    %lld", (long long int)cnt_arg.cnt);
    }
    printf("\n");
    if (long_format && (node_mask & aff_ls_mask) )
    {
	switch (type) {
	case affNodeVoid:
	    break;
	case affNodeChar: {
	    char *ptr = malloc(sizeof (char) * size);
	    int i;
	    if (ptr == 0) {
		fprintf(stderr, "lhpc-aff: not enough memory\n");
		exit(1);
	    }
	    if (aff_node_get_char(arg->r, node, ptr, size)) {
		fprintf(stderr, "lhpc-aff: error getting data\n");
		free(ptr);
		exit(1);
	    }
	    printf("    \"");
	    for (i = 0; i < size; i++) {
		unsigned char p = ptr[i];
		if (p < 32 || p >= 127 || p == '\"' || p == '\\')
		    printf("\\x%02x", p);
		else
		    printf("%c", p);
	    }
	    printf("\"\n");
	    free(ptr);
	} break;
	case affNodeInt: {
	    uint32_t *ptr = malloc(sizeof (uint32_t) * size);
	    int i;
	    if (ptr == 0) {
		fprintf(stderr, "lhpc-aff: not enough memory\n");
		exit(1);
	    }
	    if (aff_node_get_int(arg->r, node, ptr, size)) {
		fprintf(stderr, "lhpc-aff: error getting data\n");
		free(ptr);
		exit(1);
	    }
	    for (i = 0; i < size; i++)
		printf("    %5d %11d\n", i, ptr[i]);
	    free(ptr);
	    break;
	}
	case affNodeDouble: {
	    double *ptr = malloc(sizeof (double) * size);
	    int i;
	    if (ptr == 0) {
		fprintf(stderr, "lhpc-aff: not enough memory\n");
		exit(1);
	    }
	    if (aff_node_get_double(arg->r, node, ptr, size)) {
		fprintf(stderr, "lhpc-aff: error getting data\n");
		free(ptr);
		exit(1);
	    }
	    for (i = 0; i < size; i++)
		printf("    %5d %24.16e\n", i, ptr[i]);
	    free(ptr);
	    break;
	}
	case affNodeComplex: {
	    double _Complex *ptr = malloc(sizeof (double _Complex) * size);
	    int i;
	    if (ptr == 0) {
		fprintf(stderr, "lhpc-aff: not enough memory\n");
		exit(1);
	    }
	    if (aff_node_get_complex(arg->r, node, ptr, size)) {
		fprintf(stderr, "lhpc-aff: error getting data\n");
		free(ptr);
		exit(1);
	    }
	    for (i = 0; i < size; i++)
		printf("    %5d %24.16e %24.16e\n", i, creal(ptr[i]), cimag(ptr[i]));
	    free(ptr);
	    break;
	}
	default:
	    fprintf(stderr, "lhpc-aff: Internal error: uknown node type %d\n",
		    type);
	    exit(1);
	}
    }
    if (!directory_only)
        aff_node_foreach(node, short_list, arg);
    if (recursive)
	aff_node_foreach(node, do_node, arg);
}

/* We are single threaded now, hence it's ok to use strtok() */
static int
do_ls(struct AffReader_s *r, const char *name, const char *kp)
{
//    char *pp = xstrdup(kp);
//    char *p = pp;
    struct AffNode_s *root = aff_reader_root(r);
    struct AffNode_s *node;
    struct arg arg;
    
    node = lookup_path(r, root, kp);
    if (NULL == node) {
        fprintf(stderr, "lhpc-aff: %s[%s]: cannot read node\n", name, kp);
        return 0;   // FIXME print error and proceed
    }
//    for (node = root, p = strtok(p, "/"); p; p = strtok(NULL, "/")) {
//	node = aff_reader_chdir(r, node, p);
//	if (node == 0) {
//	    fprintf(stderr, "lhpc-aff: error accesing %s:%s at %s\n",
//		    name, kp, p);
//	    free(pp);
//	    return 0;   // FIXME print error and proceed
//	}
//    }
//    free(pp);
    arg.r = r;
    arg.fname = name;
    arg.kpath = kp;
    arg.root = root;
    do_node(node, &arg);
    return (aff_reader_errstr(r) != 0);
}

static int 
do_ls_keylist(struct AffReader_s *r, const char *name, const char *list_fname)
{
    FILE *list = NULL;
    if(0 == strcmp(list_fname, "-")) {
        list = stdin;
        if (ferror(list)) {
            fprintf(stderr, "%s: bad stdin stream\n", __func__);
            return 1;
        }
    }
    else {
        if (NULL == (list = fopen(list_fname, "r"))) {
            fprintf(stderr, "%s: cannot open %s\n", __func__, list_fname);
            return 1;
        }
    }
    char buf[16384], *fargv[1];
    int num;
    while (NULL != fgets(buf, sizeof(buf), list)) {
        if ('\n' != buf[strlen(buf)-1]) {
            fprintf(stderr, "%s: line too long, skipping\n", __func__);
            while (NULL != fgets(buf, sizeof(buf), list))
                if ('\n' == buf[strlen(buf)-1])
                    break;
            continue;
        }
        num = split_farg( buf, 1, fargv );
        if (num < 0) {
            fprintf(stderr, "%s: unexpected result of split_farg; exiting\n",
                    __func__);
            goto errclean_r;
        }
        if (num == 0)
            continue;
        if (do_ls(r, name, fargv[0])) {
            fprintf(stderr, "%s: [%s]: %s\n", __func__, fargv[0],
                    aff_reader_errstr(r));
            goto errclean_r;
        }
    }
    
    fclose(list);
    return 0;
    
errclean_r:
    fclose(list);
    return 1;
}

void
h_ls(void)
{
    printf("lphc-aff ls [-klsR] [-aAvVcCiIdDxX] [-f <pre-list>] [-F <post-list>]\n"
           "\t\t<aff-file> [<key-path>] ...\n"
	   "List the contents of AFF file starting at each key-path.\n"
	   "Key path components are separated by /, as in UNIX paths.\n"
	   "\t-l\tshow data in each component\n"
	   "\t-R\tdescent recursively\n"
           "\t-d\tdo not print subkeys, only the given key\n"
           "\t-s\tprint the total number of subnodes\n"
           "\t-a(A)\t(do not) print data on nodes of all types; -a is default\n"
           "\t-v(V)\t(do not) print data on void nodes\n"
           "\t-c(C)\t(do not) print data on char nodes\n"
           "\t-i(I)\t(do not) print data on int nodes\n"
           "\t-d(D)\t(do not) print data on double nodes\n"
           "\t-x(X)\t(do not) print data on complex nodes\n"
           "\t\tamong [aAvVcCiIdDxX] options, the last always takes precedence\n"
           "\t-f <pre-list>\n\t\tprint data from key list in file <pre-list> BEFORE\n"
           "\t\tprocessing command line list\n"
           "\t-F <post-list>\n\t\tprint data from key list in file <post-list> AFTER\n"
           "\t\tprocessing command line list\n"
           "\t\tparameter to only one of -f, -F options can be '-' (stdin)\n"
           );
    /* TODO 
       -d node itself, without subnodes
       -s number of subnodes
     */
}




int
x_ls(int argc, char *argv[])
{
    const char *fname;
    struct AffReader_s *r;
    const char *status;
    const char *list1_fname = NULL,
               *list2_fname = NULL;
    int res = 1;

    if (argc < 1) {
    error:
	fprintf(stderr, "usage: lhpc-aff ls [-lR] aff-file key-path ...\n");
	return 1;
    }
    for (;argc; argc--, argv++) {
	char *p;
	if (argv[0][0] != '-')
	    break;
	for (p = &argv[0][1]; *p; p++) {
	    switch (*p) {
	    case 'l': long_format = 1; break;
	    case 'R': recursive = 1; break;
            case 's': count_subnodes = 1; break;
            case 'k': directory_only = 1; break;
	    case 'a': aff_ls_mask = aff_ls_mask_all; break;
	    case 'A': aff_ls_mask = 0; break;
            case 'v': aff_ls_mask = aff_ls_mask | affNodeVoidMask;      break;
            case 'c': aff_ls_mask = aff_ls_mask | affNodeCharMask;      break;
            case 'i': aff_ls_mask = aff_ls_mask | affNodeIntMask;       break;
            case 'd': aff_ls_mask = aff_ls_mask | affNodeDoubleMask;    break;
            case 'x': aff_ls_mask = aff_ls_mask | affNodeComplexMask;   break;
            case 'V': aff_ls_mask = aff_ls_mask & ~affNodeVoidMask;     break;
            case 'C': aff_ls_mask = aff_ls_mask & ~affNodeCharMask;     break;
            case 'I': aff_ls_mask = aff_ls_mask & ~affNodeIntMask;      break;
            case 'D': aff_ls_mask = aff_ls_mask & ~affNodeDoubleMask;   break;
            case 'X': aff_ls_mask = aff_ls_mask & ~affNodeComplexMask;  break;
            case 'f':   
                {
                    if( '\0' != *(p+1) || !(--argc) )
                    {
                        fprintf( stderr, "%s: -f must be followed by a file name\n", __func__ );
                        return 1;
                    }
                    list1_fname = *(++argv);
                } break;
            case 'F':   
                {
                    if( '\0' != *(p+1) || !(--argc) )
                    {
                        fprintf( stderr, "%s: -F must be followed by a file name\n", __func__ );
                        return 1;
                    }
                    list2_fname = *(++argv);
                } break;
	    default: goto error;
	    }
	}
    }
	
    if (argc < 1)
	goto error;
    fname = argv[0];
    r = aff_reader(fname);
    if (aff_reader_errstr(r) != 0)
	goto end;
    if (NULL != list1_fname) {
        if (do_ls_keylist(r, fname, list1_fname))
            goto end;
    }
    if (argc == 1 && NULL == list1_fname && NULL ==list2_fname) {
        if (do_ls(r, fname, ""))
            goto end;
    } else {
	for ( argv++; --argc; argv++) {
	    if (do_ls(r, fname, argv[0]))
		goto end;
	}
    }
    if (NULL != list2_fname) {
        if (do_ls_keylist(r, fname, list2_fname))
            goto end;
    }

end:
    status = aff_reader_errstr(r);
    aff_reader_close(r);
    if (status != 0) {
	fprintf(stderr, "lhpc-aff: error opening %s\n", argv[0]);
    } else {
	res = 0;
    }
    return res;
}
