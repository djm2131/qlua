#include <stdlib.h>
#include "alloc.h"

/* default AFF allocator */

void *
aff_realloc(void *ptr, size_t size)
{
    if (size > 0) {
        return realloc(ptr, size);
    }
    if (ptr) {
        free(ptr);
    }
    return NULL;
}
