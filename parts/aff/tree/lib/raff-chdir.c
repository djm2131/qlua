#include <stdint.h>
#include <stdio.h>
#include <stdarg.h>
#include "node.h"
#include "md5.h"
#include "aff-i.h"

struct AffNode_s *
aff_reader_chdir(struct AffReader_s *aff,
		 struct AffNode_s *dir,
		 const char *name)
{
    struct AffNode_s *res;

    if (aff == 0 || aff->error)
	return 0;
    if (dir == 0) {
	aff->error = "NULL dir in aff_reader_chdir()";
	return 0;
    }
    if (name == 0) {
	aff->error = "NULL name in aff_reader_chdir()";
	return 0;
    }
    res = aff_node_chdir(aff->tree, aff->stable, dir, 0, name);
    if (res == 0) {
	aff->error = "aff_reader_chdir() failed";
    }
    return res;
}
