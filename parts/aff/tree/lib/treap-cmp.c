#include <stdint.h>
#include <string.h>
#include "treap-i.h"

int
aff_treap_cmp(const void *a_ptr, unsigned int a_size,
	      const void *b_ptr, unsigned int b_size)
{
    if (a_size < b_size)
	return -1;
    if (a_size > b_size)
	return +1;
    return memcmp(a_ptr, b_ptr, a_size);
}
	     
