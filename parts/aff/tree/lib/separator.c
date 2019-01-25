#include <stdint.h>
#include "md5.h"
#include "aff-i.h"

char *
aff_strsep(char *str, char **end, char delim)
{
    char *s;

    if( str == NULL ) {
	*end = NULL;
	return NULL;
    }

    for (s = str ; (*s != delim) && (*s != '\0'); s++)
	;

    *end = (*s == 0)? NULL: (s + 1);
    *s = 0;

    return str;
}
