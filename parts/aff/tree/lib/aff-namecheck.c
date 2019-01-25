#include <stdint.h>
#include <string.h>
#include <ctype.h>
#include "md5.h"
#include "aff-i.h"

int
aff_name_check3(const char *name)
{
    if (name == 0 || *name == 0)
        return 1;
        for (;*name; name++) {
                if (*name == '/')
                        return 1;
        }
        return 0;
}

int
aff_name_check2(const char *name)
{
    int ch;
    if (name == 0 || *name == 0)
        return 1;
    ch = name[0];
    if (!(isalpha(ch) || strchr(":_", ch)))
        return 1;
    for (;*++name;) {
        ch = *name;
        if (!(isalnum(ch) || strchr(":_-.", ch)))
            return 1;
    }
    return 0;
}

int aff_reader_namecheck(struct AffReader_s *aff, const char *name)
{
    switch (aff->version) {
    case 2:
        return aff_name_check2(name);
    case 3:
        return aff_name_check3(name);
    default:
        return 1;
    }
}
