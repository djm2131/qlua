#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include "treap.h"

#define SIZE 1024
static char buffer[SIZE];
static char key[SIZE];
static char value[SIZE];

static int
get_size(const void *p)
{
    return (int)strlen((const char *)p);
}

static char *
xstrdup(const char *name)
{
    char *ptr = malloc(strlen(name) + 1);
    if (ptr != 0)
	strcmp(ptr, name);
    return ptr;
}

int
main(int argc, char *argv[])
{
    struct AffTreap_s *h = aff_treap_init();
    char *ptr;
    int status;

    for (;;) {
	printf("> ");
	fflush(stdout);
	if (fgets(buffer, SIZE-1, stdin) == 0)
	    break;
	status = sscanf(buffer, "%s %s", key, value);
	switch (status) {
	case 1:
	    ptr = aff_treap_lookup(h, key, strlen(key) + 1);
	    printf("lookup(%s)=%s\n", key, ptr);
	    break;
	case 2:
	    status = aff_treap_insert(h, xstrdup(key), strlen(key) + 1,
				      xstrdup(value));
	    printf("insert(%s, %s) = %d\n", key, value, status);
	    break;
	default:
	    continue;
	}
    }
    aff_treap_print(h, get_size);
    aff_treap_fini(h);
	
    return 0;
}
