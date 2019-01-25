#include <stdio.h>
#include <stdlib.h>
#include <float.h>
#include <assert.h>
#include <math.h>
#include <string.h>
#include <unistd.h>
#include <complex.h>
#include <errno.h>

#include "lhpc-aff.h"
#include <sys/types.h>
#include <sys/stat.h>

#include "util.h"
#include "common.h"

int 
        diff_recursive = 0,
        print_diff_data = 0,
        ignore_missing = 0,
        quiet_report = 0;

double  diff_prec = 1. * DBL_EPSILON; // rounding error


/* compare first common number of elements (if data size is different) 
 * complex and double are compared element by element, 
 * with fractional precision `diff_prec'
 */
static int 
diff_complex(char *buf, size_t buf_size,
             struct AffReader_s *rA, struct AffNode_s *nodeA,
             struct AffReader_s *rB, struct AffNode_s *nodeB)
{
    assert(affNodeComplex == aff_node_type(nodeA) && 
            affNodeComplex == aff_node_type(nodeB));
    size_t  sizeA = aff_node_size(nodeA),
            sizeB = aff_node_size(nodeB);
            
    double _Complex *ptrA = (double _Complex *)malloc(sizeA * sizeof(double _Complex));
    if (NULL == ptrA) {
        fprintf(stderr, "%s: not enough memory\n", __func__);
        return 1;
    }
    if (aff_node_get_complex(rA, nodeA, ptrA, sizeA)) {
        fprintf(stderr, "%s: %s\n", __func__, aff_reader_errstr(rA));
        goto clearerr_A;
    }
    double _Complex *ptrB = (double _Complex *)malloc(sizeB *sizeof(double _Complex));
    if (NULL == ptrB) {
        fprintf(stderr, "%s: not enough memory\n", __func__);
        goto clearerr_A;
    }
    if (aff_node_get_complex(rB, nodeB, ptrB, sizeB)) {
        fprintf(stderr, "%s: %s\n", __func__, aff_reader_errstr(rB));
        goto clearerr_BA;
    }
    size_t size = (sizeA <= sizeB ? sizeA : sizeB);
    double *ptr_diff = (double *)malloc(size * sizeof(double));
    if (NULL == ptr_diff) {
        fprintf(stderr, "%s: not enough memory\n", __func__);
        goto clearerr_BA;
    }
    
    int is_diff = 0;
    for (size_t t = 0 ; t < size; t++) {
        ptr_diff[t] = 0.;
        if (isnan(creal(ptrA[t])) || isinf(creal(ptrA[t])) ||
                isnan(cimag(ptrA[t])) || isinf(cimag(ptrA[t]))) {
            if (isnan(creal(ptrA[t])) != isnan(creal(ptrB[t])) || 
                    isinf(creal(ptrA[t])) != isinf(creal(ptrB[t])) ||
                    isnan(cimag(ptrA[t])) != isnan(cimag(ptrB[t])) || 
                    isinf(cimag(ptrA[t])) != isinf(cimag(ptrB[t])) ) {
                is_diff = 1;
                ptr_diff[t] = -1.;
            }
            continue;
        }
        
        double numer = cabs(ptrA[t] - ptrB[t]);
        double denom = cabs(ptrA[t] + ptrB[t]);
        if (0. == denom) {
            if (0. != numer) {
                ptr_diff[t] = -1.;
                is_diff = 1;
            }
        } else {
            ptr_diff[t] = 2 * numer / denom;
            if (diff_prec < ptr_diff[t])
                is_diff = 1;
        }
    }
    if (is_diff && print_diff_data) {
        int max_len = buf_size;
        char *c = buf;
        int written_len;
        for (size_t t = 0 ; t < size && 0 < max_len; t++) {
            if (isnan(ptr_diff[t]) || isinf(ptr_diff[t]) || 
                    diff_prec < ptr_diff[t] || ptr_diff[t] < 0.) {
                snprintf(c, max_len, "%ld\t(%13.7e,%13.7e)\t(%13.7e,%13.7e)\t%13.7e\n%n",
                         (long int)t, creal(ptrA[t]), cimag(ptrA[t]), 
                         creal(ptrB[t]), cimag(ptrB[t]), ptr_diff[t], &written_len);
                c += written_len, max_len -= written_len;
            }
        }
        written_len = strlen(buf);
        if (0 < written_len && '\n' != buf[written_len-1])
            buf[written_len] = '\n';
    }
    free(ptr_diff);
    free(ptrA);
    free(ptrB);
    return (0 != is_diff);
    
clearerr_BA:
    if (NULL != ptrB)
        free(ptrB);
clearerr_A:
    if (NULL != ptrA)
        free(ptrA);
    return -1;
}

static int
diff_double(char *buf, size_t buf_size,
            struct AffReader_s *rA, struct AffNode_s *nodeA,
            struct AffReader_s *rB, struct AffNode_s *nodeB)
{
    assert(affNodeDouble == aff_node_type(nodeA) && 
            affNodeDouble == aff_node_type(nodeB));
    size_t  sizeA = aff_node_size(nodeA),
            sizeB = aff_node_size(nodeB);
    double *ptrA = (double *)malloc(sizeA * sizeof(double));
    if (NULL == ptrA) {
        fprintf(stderr, "%s: not enough memory\n", __func__);
        return 1;
    }
    if (aff_node_get_double(rA, nodeA, ptrA, sizeA)) {
        fprintf(stderr, "%s: %s\n", __func__, aff_reader_errstr(rA));
        goto clearerr_A;
    }
    double *ptrB = (double *)malloc(sizeB *sizeof(double));
    if (NULL == ptrB) {
        fprintf(stderr, "%s: not enough memory\n", __func__);
        goto clearerr_A;
    }
    if (aff_node_get_double(rB, nodeB, ptrB, sizeB)) {
        fprintf(stderr, "%s: %s\n", __func__, aff_reader_errstr(rB));
        goto clearerr_BA;
    }
    size_t size = (sizeA <= sizeB ? sizeA : sizeB);
    double *ptr_diff = (double *)malloc(size * sizeof(double));
    if (NULL == ptr_diff) {
        fprintf(stderr, "%s: not enough memory\n", __func__);
        goto clearerr_BA;
    }
    
    int is_diff = 0;
    for (size_t t = 0 ; t < size; t++) {
        ptr_diff[t] = 0.;
        if (isnan(ptrA[t]) || isinf(ptrA[t])) {
            if (isnan(ptrA[t]) != isnan(ptrB[t]) || 
                    isinf(ptrA[t]) != isinf(ptrB[t]) ) {
                is_diff = 1;
                ptr_diff[t] = -1.;
            }
            continue;
        }

        double numer = fabs(ptrA[t] - ptrB[t]);
        double denom = fabs(ptrA[t] + ptrB[t]);
        if (0. == denom) {
            if (0. != numer) {
                ptr_diff[t] = -1.;
                is_diff = 1;
            }
        } else {
            ptr_diff[t] = 2 * numer / denom;
            if (diff_prec < ptr_diff[t])
                is_diff = 1;
        }
    }
    if (is_diff && print_diff_data) {
        int max_len = buf_size;
        char *c = buf;
        int written_len;
        for (size_t t = 0 ; t < size && 0 < max_len; t++) {
            if (isnan(ptr_diff[t]) || isinf(ptr_diff[t]) || 
                    diff_prec < ptr_diff[t] || ptr_diff[t] < 0.) {
                snprintf(c, max_len, "%ld\t%13.7e\t%13.7e\t%13.7e\n%n",
                         (long int)t, ptrA[t], ptrB[t], ptr_diff[t], &written_len);
                c += written_len, max_len -= written_len;
            }
        }
        written_len = strlen(buf);
        if (0 < written_len && '\n' != buf[written_len-1])
            buf[written_len] = '\n';
    }
    free(ptr_diff);
    free(ptrA);
    free(ptrB);
    return (0 != is_diff);
    
clearerr_BA:
    if (NULL != ptrB)
        free(ptrB);
clearerr_A:
    if (NULL != ptrA)
        free(ptrA);
    return -1;
}

static int
diff_int(char *buf, size_t buf_size,
            struct AffReader_s *rA, struct AffNode_s *nodeA,
            struct AffReader_s *rB, struct AffNode_s *nodeB)
{
    assert(affNodeInt == aff_node_type(nodeA) && 
            affNodeInt == aff_node_type(nodeB));
    size_t  sizeA = aff_node_size(nodeA),
            sizeB = aff_node_size(nodeB);
    uint32_t *ptrA = (uint32_t *)malloc(sizeA * sizeof(uint32_t));
    if (NULL == ptrA) {
        fprintf(stderr, "%s: not enough memory\n", __func__);
        return 1;
    }
    if (aff_node_get_int(rA, nodeA, ptrA, sizeA)) {
        fprintf(stderr, "%s: %s\n", __func__, aff_reader_errstr(rA));
        goto clearerr_A;
    }
    uint32_t *ptrB = (uint32_t *)malloc(sizeB *sizeof(uint32_t));
    if (NULL == ptrB) {
        fprintf(stderr, "%s: not enough memory\n", __func__);
        goto clearerr_A;
    }
    if (aff_node_get_int(rB, nodeB, ptrB, sizeB)) {
        fprintf(stderr, "%s: %s\n", __func__, aff_reader_errstr(rB));
        goto clearerr_BA;
    }
    int is_diff = 0;
    size_t size = (sizeA <= sizeB ? sizeA : sizeB);
    for (size_t t = 0 ; t < size; t++) {
        if (ptrA[t] != ptrB[t])
            is_diff = 1;
    }
    if (is_diff && print_diff_data) {
        int max_len = buf_size;
        char *c = buf;
        int written_len;
        for (size_t t = 0 ; t < size && 0 < max_len; t++) {
            if (ptrA[t] != ptrB[t]) {
                snprintf(c, max_len, "%-6ld\t%14ld\t%14ld\n%n",
                         (long int)t, (long int)ptrA[t], (long int)ptrB[t], &written_len);
                c += written_len, max_len -= written_len;
            }
        }
        written_len = strlen(buf);
        if (0 < written_len && '\n' != buf[written_len-1])
            buf[written_len] = '\n';
    }
    free(ptrA);
    free(ptrB);
    return (0 != is_diff);
    
clearerr_BA:
    if (NULL != ptrB)
        free(ptrB);
clearerr_A:
    if (NULL != ptrA)
        free(ptrA);
    return -1;
}

static int
diff_char(char *buf, size_t buf_size,
            struct AffReader_s *rA, struct AffNode_s *nodeA,
            struct AffReader_s *rB, struct AffNode_s *nodeB)
{
    assert(affNodeChar == aff_node_type(nodeA) && 
            affNodeChar == aff_node_type(nodeB));
    size_t  sizeA = aff_node_size(nodeA),
            sizeB = aff_node_size(nodeB);
    int max_len = buf_size;
    char *c = buf;
    int written_len = 0;
    char *ptrA = (char *)malloc(sizeA * sizeof(char));
    if (NULL == ptrA) {
        fprintf(stderr, "%s: not enough memory\n", __func__);
        return 1;
    }
    if (aff_node_get_char(rA, nodeA, ptrA, sizeA)) {
        fprintf(stderr, "%s: %s\n", __func__, aff_reader_errstr(rA));
        goto clearerr_A;
    }
    char *ptrB = (char *)malloc(sizeB *sizeof(char));
    if (NULL == ptrB) {
        fprintf(stderr, "%s: not enough memory\n", __func__);
        goto clearerr_A;
    }
    if (aff_node_get_char(rB, nodeB, ptrB, sizeB)) {
        fprintf(stderr, "%s: %s\n", __func__, aff_reader_errstr(rB));
        goto clearerr_BA;
    }
    
    int is_diff = 0;
    size_t size = (sizeA <= sizeB ? sizeA : sizeB);
    for (size_t t = 0 ; t < size; t++) {
        if (ptrA[t] != ptrB[t])
            is_diff = 1;
    }
    if ((sizeA != sizeB || is_diff) && print_diff_data) {
        snprintf(c, max_len, "\t\"%n", &written_len);
        c += written_len, max_len -= written_len;
        for (size_t i = 0 ; i < sizeA && 0 < max_len; i++) {
            unsigned char p = ptrA[i];
            if (p < 32 || 127 < p || '\"' == p || '\\' == p)
                snprintf(c, max_len, "\\x%02x%n", p, &written_len);
            else
                snprintf(c, max_len, "%c%n", p, &written_len);
            c += written_len, max_len -= written_len;
        }
        snprintf(c, max_len, "\"\n\t\"%n", &written_len);
        c += written_len, max_len -= written_len;
        for (size_t i = 0 ; i < sizeB && 0 < max_len ; i++) {
            unsigned char p = ptrB[i];
            if (p < 32 || 127 < p || '\"' == p || '\\' == p)
                snprintf(c, max_len, "\\x%02x%n", p, &written_len);
            else
                snprintf(c, max_len, "%c%n", p, &written_len);
            c += written_len, max_len -= written_len;
        }
        snprintf(c, max_len, "\"\n%n", &written_len);
        c += written_len, max_len -= written_len;

        written_len = strlen(buf);
        if (0 < written_len && '\n' != buf[written_len-1])
            buf[written_len] = '\n';
    }
    
    free(ptrA);
    free(ptrB);
    return (0 != is_diff);
    
clearerr_BA:
    if (NULL != ptrB)
        free(ptrB);
clearerr_A:
    if (NULL != ptrA)
        free(ptrA);
    return -1;
}

static const char *
node_type_str(enum AffNodeType_e type) {
    switch (type) {
    case affNodeInvalid:    return "invalid";
    case affNodeChar:       return "char";
    case affNodeInt:        return "int";
    case affNodeDouble:     return "double";
    case affNodeComplex:    return "complex";
    case affNodeVoid:       return "void";
    }
    return NULL;
}

/* compare data in `nodeA' and `nodeB' */
static int 
diff_node_data(struct AffReader_s *rA, struct AffNode_s *nodeA, const char *fileA_name,
               struct AffReader_s *rB, struct AffNode_s *nodeB, const char *fileB_name)
{
    // compare type and size
    int is_diff = 0;
    if (aff_node_type(nodeA) != aff_node_type(nodeB)) {
        if (!quiet_report) {
            print_path(aff_reader_root(rA), nodeA);
            printf("  <=>  ");
            print_path(aff_reader_root(rB), nodeB);
            printf(": different data type: %s != %s\n", 
                   node_type_str(aff_node_type(nodeA)), node_type_str(aff_node_type(nodeB)));
            is_diff = 1;    
        } else 
            return 1;
    }
    if (aff_node_size(nodeA) != aff_node_size(nodeB)) {
        if (!quiet_report) {
            print_path(aff_reader_root(rA), nodeA);
            printf("  <=>  ");
            print_path(aff_reader_root(rB), nodeB);
            printf(": different data size: %ld != %ld\n", 
                   (long int)aff_node_size(nodeA), (long int)aff_node_size(nodeB));
            is_diff = 1;
        } else 
            return 1;
    }
        
    // compare data
    int diff_res = 0;
    char buf[10000];
    switch (aff_node_type(nodeA)) {
    case affNodeInvalid:
        fprintf(stderr, "%s: %s: ", __func__, fileA_name);
        fprint_path(stderr, aff_reader_root(rA), nodeA);
        fprintf(stderr, ": AffNodeInvalid node type\n");
        return -1;
    case affNodeChar:
        diff_res = diff_char(buf, sizeof(buf), rA, nodeA, rB, nodeB);
        break;
    case affNodeInt:
        diff_res = diff_int(buf, sizeof(buf), rA, nodeA, rB, nodeB);
        break;
    case affNodeDouble:
        diff_res = diff_double(buf, sizeof(buf), rA, nodeA, rB, nodeB);
        break;
    case affNodeComplex:
        diff_res = diff_complex(buf, sizeof(buf), rA, nodeA, rB, nodeB);
        break;
    case affNodeVoid:
        break;
    }
    if (diff_res < 0) 
        return diff_res;
    if (0 < diff_res) {
        if (!quiet_report) {
            print_path(aff_reader_root(rA), nodeA);
            printf("  <=>  ");
            print_path(aff_reader_root(rB), nodeB);
            if (aff_node_size(nodeA) == aff_node_size(nodeB))
                printf(":  %s[%d]: different data\n", 
                       node_type_str(aff_node_type(nodeA)), aff_node_size(nodeA));
            else 
                printf(": %s[%d, %d]: different data\n", node_type_str(aff_node_type(nodeA)),
                       aff_node_size(nodeA), aff_node_size(nodeB));
            if (print_diff_data) 
                printf("%s\n", buf);
        }
    }
    return diff_res;
}


static int 
diff_subnodes(struct AffReader_s *rA, struct AffNode_s *nodeA, const char *fileA_name, 
              struct AffReader_s *rB, struct AffNode_s *nodeB, const char *fileB_name);

struct diff_subnodes_arg {
    struct AffReader_s *rA;
    const char *fileA_name;
    struct AffReader_s *rB; 
    struct AffNode_s *nodeB;
    const char *fileB_name;
    
    /* flag: do not check node contents, only presence */
    int only_check_key_present;  

    /* 0 == diff_res: nodes are equal
     * 0 < diff_res : nodes are different    
     * diff_res < 0 : error has occured  */  
    int diff_res;           
};

/* check that there is the subnode in `arg->nodeB' with the same name as `nodeA'
 * and compare (recursively) their contents 
 */
static void
diff_subnodes_func(struct AffNode_s *nodeA, void *arg_) 
{
    struct diff_subnodes_arg *arg = (struct diff_subnodes_arg *)arg_;
    
    // check presence
    struct AffNode_s *nodeB = 
        aff_node_chdir(aff_reader_tree(arg->rB), aff_reader_stable(arg->rB), 
                       arg->nodeB, 0, aff_symbol_name(aff_node_name(nodeA)));
    if (NULL == nodeB) {
        if (ignore_missing)
            return;
        if (!quiet_report) {
            print_path(aff_reader_root(arg->rA), nodeA);
            printf(": only in %s\n", arg->fileA_name);
        }
        arg->diff_res = 1;
        return;
    }
    if (arg->only_check_key_present)
        return;
    
    int diff_res = diff_node_data(arg->rA, nodeA, arg->fileA_name,
                                  arg->rB, nodeB, arg->fileB_name);
    if (diff_res < 0 ||
            ((quiet_report || !diff_recursive) && 0 < diff_res)) {
        arg->diff_res = diff_res;
        return;
    }

    // compare subnodes
    diff_res = diff_subnodes(arg->rA, nodeA, arg->fileA_name, 
                             arg->rB, nodeB, arg->fileB_name);
    if (diff_res)
        arg->diff_res = diff_res;
}

/* compare all subnodes of `nodeA' and `nodeB' */
static int 
diff_subnodes(struct AffReader_s *rA, struct AffNode_s *nodeA, const char *fileA_name, 
              struct AffReader_s *rB, struct AffNode_s *nodeB, const char *fileB_name)
{
    // compare subnodes of `nodeA' to the same subnodes of `nodeB'
    struct diff_subnodes_arg arg;
    arg.rA = rA, arg.fileA_name = fileA_name,
        arg.rB = rB, arg.fileB_name = fileB_name,
        arg.nodeB = nodeB;
    arg.only_check_key_present = 0;
    arg.diff_res = 0;
    aff_node_foreach(nodeA, diff_subnodes_func, &arg);
    if (arg.diff_res < 0 ||
            (quiet_report && 0 < arg.diff_res))
        return arg.diff_res;
    
    if (ignore_missing)
        return arg.diff_res;

    // check for subnodes of `nodeB' absent in `nodeA'
    arg.rA = rB, arg.fileA_name = fileB_name,
        arg.rB = rA, arg.fileB_name = fileA_name,
        arg.nodeB = nodeA;
    arg.only_check_key_present = 1;
    aff_node_foreach(nodeB, diff_subnodes_func, &arg);
    return arg.diff_res;
}

static int
diff_key(struct AffReader_s *rA, const char *keyA, const char *fileA_name,
         struct AffReader_s *rB, const char *keyB, const char *fileB_name)
{
    if (NULL == rA || NULL == keyA || NULL == fileA_name ||
            NULL == rB || NULL == keyB || NULL == fileB_name)
        return -1;
    struct AffNode_s *nodeA = aff_reader_chpath(rA, aff_reader_root(rA), keyA);
    if (NULL == nodeA || NULL != aff_reader_errstr(rA)) {
        fprintf(stderr, "%s: %s[%s]: %s\n", 
                __func__, fileA_name, keyA, aff_reader_errstr(rA));
#if 1
        return -1;
#else   // TODO: use the code below when clearerr is implemented
        if (aff_reader_clearerr(rA))
            return -1;
        else 
            return 1;
#endif
    }
    struct AffNode_s *nodeB = aff_reader_chpath(rB, aff_reader_root(rB), keyB);
    if (NULL == nodeB || NULL != aff_reader_errstr(rA)) {
        fprintf(stderr, "%s: %s[%s]: %s\n", 
                __func__, fileB_name, keyB, aff_reader_errstr(rB));
#if 1
        return -1;
#else   // TODO use the code below when clearerr is implemented        
        if (aff_reader_clearerr(rB))
            return -1;
        else
            return 1;
#endif
    }
    
    int diff_res = 0;
    if (aff_reader_root(rA) != nodeA && aff_reader_root(rB) != nodeB)
        diff_res = diff_node_data(rA, nodeA, fileA_name,
                                  rB, nodeB, fileB_name);
    else if (aff_reader_root(rA) == nodeA && aff_reader_root(rB) != nodeB)
        fprintf(stderr, "%s: %s[%s]: root node, no data\n",
                __func__, fileA_name, keyA);
    else if (aff_reader_root(rA) != nodeA && aff_reader_root(rB) == nodeB)
        fprintf(stderr, "%s: %s[%s]: root node, no data\n",
                __func__, fileB_name, keyB);
    
    if (diff_res < 0 || (quiet_report && 0 < diff_res))
        return diff_res;
    if (diff_recursive) {
        int aux = diff_subnodes(rA, nodeA, fileA_name, 
                                rB, nodeB, fileB_name);
        if (aux < 0 || (quiet_report && 0 < aux))
            return aux;
        if (aux)
            diff_res = aux;
    }
    
    return diff_res;
}

static int
diff_key_list(struct AffReader_s *rA, const char *fileA_name,
              struct AffReader_s *rB, const char *fileB_name,
              const char *list_name)
{
    if (NULL == rA || NULL == fileA_name ||
            NULL == rB || NULL == fileB_name ||
            NULL == list_name)
        return -1;
    FILE *list;
    if (!strcmp(list_name, "-")) {
        list = stdin;
        if (ferror(list)) {
            fprintf(stderr, "%s: bad stdin stream\n", __func__);
            return -1;
        }
    } else {
        if (NULL == (list = fopen(list_name, "r"))) {
            fprintf(stderr, "%s: %s: %s\n", __func__, list_name, strerror(errno));
            return -1;
        }
    }
    char buf[32768];
    char *fargv[2];
    int diff_res = 0;
    while (NULL != fgets(buf, sizeof(buf), list)) {
        if ('\n' != buf[strlen(buf)-1]) {
            fprintf(stderr, "%s: %s: line too long, skipping\n", __func__, list_name);
            while (NULL != fgets(buf, sizeof(buf), list)) {
                if ('\n' == buf[strlen(buf) - 1])
                    break;
            }
            continue;
        }
        int num = split_farg(buf, 2, fargv);
        if (num < 0) {
            fprintf(stderr, "%s: error in split_farg\n", __func__);
            goto errclean_f;
        }
        if (num == 0)
            continue;
        if (num < 2) {
            fprintf(stderr, "%s: syntax error: expect 2 keypaths, only %d given\n", 
                    __func__, num);
            goto errclean_f;
        }
        
        int aux = diff_key(rA, fargv[0], fileA_name,
                           rB, fargv[1], fileB_name);
        if (aux < 0 || (quiet_report && 0 < aux))
            return aux;
        if (aux)
            diff_res = aux;
    }
    fclose(list);
    return diff_res;

errclean_f:
    fclose(list);
    return -1;
}

int 
x_diff(int argc, char *argv[])
{
    const char 
            *fileA_name = NULL,
            *fileB_name = NULL,
            *list1_name = NULL,
            *list2_name = NULL;
            
    for (; argc ; --argc, ++argv) {
        if ('-' != argv[0][0])
            break;
        for (char *p = argv[0] + 1 ; '\0' != *p ; ++p) {
            switch (*p) {
            case 'd':   print_diff_data = 1;    break;
            case 'N':   ignore_missing = 1;     break;
            case 'R':   diff_recursive = 1;     break;
            case 'q':   quiet_report = 1;       break;
            case 'p':   
                    if( '\0' != *(p+1) || !(--argc) ) {
                        fprintf( stderr, "%s: -p must be followed by a number\n", __func__ );
                        return -1;
                    }
                    diff_prec = strtod(*(++argv), NULL);
                    break;
            case 'f':   
                    if( '\0' != *(p+1) || !(--argc) ) {
                        fprintf( stderr, "%s: -f must be followed by a file name\n", __func__ );
                        return -1;
                    }
                    list1_name = *(++argv);
                    break;
            case 'F':
                    if( '\0' != *(p+1) || !(--argc) ) {
                        fprintf( stderr, "%s: -F must be followed by a file name\n", __func__ );
                        return -1;
                    }
                    list2_name = *(++argv);
                    break;
            default:
                fprintf( stderr, "%s: unknown option -%c\n", __func__, *p );
                return -1;
            }
        }
    }

    if (argc < 2) {
        fprintf(stderr, "%s: expect two file names\n", __func__);
        return 1;
    } else {
        fileA_name = *(argv++),
        fileB_name = *(argv++);
        argc -= 2;
    }
    if (argc % 2) {
        fprintf(stderr, "%s: unpaired key paths\n", __func__);
        return -1;
    }
    if (list1_name != NULL && !strcmp(list1_name, "-") &&
            list2_name != NULL && !strcmp(list2_name, "-")) {
        fprintf(stderr, "%s: both -f,-F lists cannot be '-' (stdin)\n", __func__);
        return -1;
    }

    struct AffReader_s *rA = aff_reader(fileA_name),
                       *rB = aff_reader(fileB_name);
    if (rA == NULL || rB == NULL) {
        fprintf(stderr, "%s: not enough memory\n", __func__);
        goto clearerr_r;
    }
    if (NULL != aff_reader_errstr(rA)) {
        fprintf(stderr, "%s: %s: %s\n", __func__, fileA_name, aff_reader_errstr(rA));
        goto clearerr_r;
    }
    if (NULL != aff_reader_errstr(rB)) {
        fprintf(stderr, "%s: %s: %s\n", __func__, fileB_name, aff_reader_errstr(rB));
        goto clearerr_r;
    }
    
    int diff_res = 0;
    if (0 == argc && 
            NULL == list1_name &&
            NULL == list2_name) {
        // compare recursively, from root
        diff_recursive = 1;
        diff_res = diff_subnodes(rA, aff_reader_root(rA), fileA_name,
                                 rB, aff_reader_root(rB), fileB_name);
        goto clearexit;
    }

    if (NULL != list1_name) {
        int aux = diff_key_list(rA, fileA_name, rB, fileB_name, list1_name);
        if (aux < 0 || 
                (quiet_report && 0 < aux))
            goto clearexit;
        if (aux)
            diff_res = aux;
    }
    for (; argc ; argc -= 2) {
        int aux = diff_key(rA, argv[0], fileA_name, rB, argv[1], fileB_name);
        if (aux < 0 || 
                (quiet_report && 0 < aux))
            goto clearexit;
        if (aux)
            diff_res = aux;
    }
    if (NULL != list2_name) {
        int aux = diff_key_list(rA, fileA_name, rB, fileB_name, list2_name);
        if (aux < 0 || 
                (quiet_report && 0 < aux))
            goto clearexit;
        if (aux)
            diff_res = aux;
    }

clearexit:
    aff_reader_close(rA);
    aff_reader_close(rB);
    return diff_res;

clearerr_r:
    if (NULL != rA)
        aff_reader_close(rA);
    if (NULL != rB)
        aff_reader_close(rB);
    return -1;    
}


void 
h_diff(void)
{
    printf( "Usage:\n"
            "lhpc-aff diff [-dNR] [-p <rel-prec>] [-f <list>] [-F <list>] <fileA> <fileB>\n"
            "                [<keyA> <keyB>] ...\n"
            "Compare data in the keys in two AFF files <fileA> and <fileB>:\n"
            "<fileA>[<keyA>] <-> <fileB>[<keyB>]\n"
            "If keys are missing, compare files recursively starting from the root nodes.\n"
            "If the data in two nodes has different size, only the first common number of\n"
            "elements is compared."
            "Options:\n"
            "    -d          print data difference for keys that have different data\n"
            "    -N          ignore absent keys\n"
            "    -R          compare keys recursively\n"
            "    -p          for `double' and `complex' data nodes, use <rel-prec>\n"
            "                as fractional difference threshold\n"
            "    -f <pre-list>\n"
            "                compare keys in file <pre-list> BEFORE processing command line list\n"
            "    -F <post-list>\n"
            "                compare keys in file <pre-list> AFTER processing command line list\n"
            "                each line of <list> is a separate diff instruction,\n"
            "                and it must have two key paths, in order:\n"
            "                <keyA> <keyB>\n"
            "                parameter to only one of -f, -F options can be '-' (stdin).\n"
          );
}
