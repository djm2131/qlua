#include <stdint.h>
#include <stdarg.h>
#include "aff.h"

const char *
aff_version(void)
{
    return 
#ifdef AFF_DEBUG
        "[DEBUG]: "
#endif
        AFF_VERSION
        ;
}
