#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <float.h>
#include <errno.h>
#include "md5.h"
#include "stable.h"
#include "tree.h"
#include "alloc.h"
#include "aff-i.h"

struct AffWriter_s *
aff_writer(const char *file_name)
{
    uint8_t dummy_header[AFF_HEADER_SIZE2];
    struct AffWriter_s *aff = aff_realloc(NULL, sizeof (struct AffWriter_s));

    if (aff == 0)
        return 0;

    memset(aff, 0, sizeof (struct AffWriter_s));
    remove(file_name);
    aff->file = fopen(file_name, "wb");
    if (aff->file == 0) {
        aff->error = strerror(errno);
        return aff;
    }

    aff->stable = aff_stable_init(0);
    if (aff->stable == 0) {
        aff->error = "Not enough memory for stable in aff_writer()";
        aff->fatal_error = 1;
        goto error;
    }

    aff->tree = aff_tree_init(aff->stable, 0);
    if (aff->tree == 0) {
        aff->error = "Not enough memory for tree in aff_writer()";
        aff->fatal_error = 1;
        goto error;
    }

    memset(dummy_header, 0, AFF_HEADER_SIZE2);
    if (fwrite(dummy_header, AFF_HEADER_SIZE2, 1, aff->file) != 1) {
        aff->error = "Error writing dummy header in aff_writer()";
        aff->fatal_error = 1;
        goto error;
    }
    
    if (FLT_RADIX != 2 || sizeof(double) > sizeof (uint64_t)) {
        aff->error = "Unsupported double format in aff_writer()";
        aff->fatal_error = 1;
        goto error;
    }

    aff->position = AFF_HEADER_SIZE2;
    aff_md5_init(&aff->data_hdr.md5);
    aff->data_hdr.size = 0;
    aff->data_hdr.records = 0;
    aff->data_hdr.start = AFF_HEADER_SIZE2;
    aff->version = 2; /* upgrade to 3 only if a post version 2 key is created */

    return aff;

error:
    if (aff->file)
        fclose(aff->file);
    aff->file = 0;
    if (aff->tree)
        aff_tree_fini(aff->tree);
    aff->tree = 0;
    if (aff->stable)
        aff_stable_fini(aff->stable);
    aff->stable = 0;

    return aff;
}
