#include <stdio.h>
#include <string.h>
#include "lhpc-aff.h"
#include "util.h"

struct op {
    const char  *name;
    int        (*run)(int argc, char *argv[]);
    void       (*help)(void);
    const char  *info;
};

#define PROC(n,x,h,t) { n, x, h, t},
struct op op[] = {
#include "procs.def"
    {0, 0, 0, 0}
};
#undef PROC

int
x_help(int argc, char *argv[])
{
    if (argc == 0) {
	int i;
	printf("Usage: lhpc-aff cmd [arg]...\n"
	       "\nAvailable commands:\n");
	for (i = 0; op[i].name; i++)
	    printf("\t%-10s%s\n", op[i].name, op[i].info);
	printf("\nSay lhpc-aff help <cmd> for more details\n");
    } else {
	int i;
	for (i = 0; op[i].name; i++) {
	    if (strcmp(op[i].name, argv[0]) == 0) {
		op[i].help();
		return 0;
	    }
	}
	printf("lhpc-aff: Uknown commnd %s\n", argv[0]);
    }
    return 0;
}

void
h_help(void)
{
    printf("lhpc-aff help [cmd]\n"
	"\tprints help about cmd, if none is given, lists available commands\n"
	"\tand exits.\n");
}

int
x_version(int argc, char *argv[])
{
    printf("%s\n", aff_version());
    return 0;
}

void
h_version(void)
{
    printf("lhpc-aff version\n"
	   "\tprints the version of lhpc-aff library it is linked with.\n");
}

int
main(int argc, char *argv[])
{
    int i;

    if (argc < 2) {
	x_help(0, NULL);
	return 1;
    }
    for (i = 0; op[i].name; i++) {
	if (strcmp(op[i].name, argv[1]) == 0)
	    return op[i].run(argc - 2, argv + 2);
    }
    fprintf(stderr, "lhpc-aff: uknown command %s\n", argv[1]);
    return 1;
}
