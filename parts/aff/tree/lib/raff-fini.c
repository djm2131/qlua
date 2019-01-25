#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include "stable.h"
#include "tree.h"
#include "md5.h"
#include "alloc.h"
#include "aff-i.h"

void
aff_reader_close(struct AffReader_s *aff)
{
    if (aff == 0)
        return;
    if (aff->file)
        fclose(aff->file);
    if (aff->tree)
        aff_tree_fini(aff->tree);
    if (aff->stable)
        aff_stable_fini(aff->stable);
    aff_realloc(aff, 0);
}
