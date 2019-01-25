#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include "node.h"
#include "md5.h"
#include "alloc.h"
#include "aff-i.h"

struct AffNode_s *
aff_reader_chpath(struct AffReader_s *aff,
                  struct AffNode_s *dir,
                  const char *path)
{
    struct AffNode_s *res;
    char *str;
    char *ptr;
    char *end;

    if (aff == 0 || aff->error)
        return 0;

    if (path == 0) {
        aff->error = "NULL path in aff_reader_chpath()";
        return 0;
    }
    if (path[0] == '/') {
        res = aff_reader_root(aff);
    } else {
        if (dir == 0) {
            aff->error = "NULL dir in aff_reader_chpath()";
            return 0;
        }
        res = dir;
    }
    str = aff_realloc(NULL, strlen(path) + 1);
    if (str == 0) {
        aff->error = "Not enough memory in aff_reader_chpath()";
        return 0;
    }
    strcpy(str, path);
    for (end = str;;) {
        ptr = aff_strsep(end, &end, '/');
        if (ptr == NULL)
            break;
        if (ptr[0] == 0)
            continue;
        res = aff_reader_chdir(aff, res, ptr);
        if (res == 0) {
            aff->error = "No such node in aff_reader_chdir()";
            break;
        }
    }
    
    aff_realloc(str, 0);
    return res;
}
