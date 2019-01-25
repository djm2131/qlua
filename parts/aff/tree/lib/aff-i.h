#ifndef MARK_6f9650e1_45e8_4695_81a9_8c3672baee90
#define MARK_6f9650e1_45e8_4695_81a9_8c3672baee90
#include "aff.h"
#include <stdio.h>

enum {
    AFF_SIG_ID_SIZE   =  21, /* These shalt be so for the ages of ages */
    AFF_SIG_OFF_DBITS =  21, 
    AFF_SIG_OFF_RADIX =  22,
    AFF_SIG_OFF_MANT  =  23,
    AFF_SIG_OFF_EXP   =  24,
    AFF_SIG_OFF_SIZE  =  28,
    AFF_SIG_SIZE      =  32,
    AFF_HEADER_SIZE1  = 144, /* the full header size V1 */
    AFF_HEADER_SIZE2  = 168  /* the full header size V2 and V3 */
};

#define AFF_SIG1       "LHPC AFF version 1.0"
#define AFF_SIG2       "LHPC AFF version 2.0"
#define AFF_SIG3       "LHPC AFF version 3.0"

struct WSection_s {
    uint64_t         start;
    uint64_t         size;
    uint64_t         records;
    struct AffMD5_s  md5;
};

struct AffWriter_s {
    struct AffMD5_s      header_md5;
    int                  header_size;
    const char          *error;
    int                  version;
    int                  fatal_error;

    FILE                *file;
    uint64_t             position;

    struct AffSTable_s  *stable;
    struct AffTree_s    *tree;

    struct WSection_s    stable_hdr;
    struct WSection_s    tree_hdr;
    struct WSection_s    data_hdr;
};

struct RSection_s {
    uint64_t         start;
    uint64_t         size;
    uint64_t         records;
    uint8_t          md5[16];
};

struct AffReader_s {
    const char          *error;
    int                  version;
    int                  fatal_error;

    FILE                *file;
    uint64_t             position;

    struct AffSTable_s  *stable;
    struct AffTree_s    *tree;

    struct RSection_s    stable_hdr;
    struct RSection_s    tree_hdr;
    struct RSection_s    data_hdr;
};

extern uint8_t aff_signature1[];
extern uint8_t aff_signature2[];
extern uint8_t aff_signature3[];

char *aff_strsep(char *str, char **end, char delim);

#endif /* !defined(MARK_6f9650e1_45e8_4695_81a9_8c3672baee90) */
