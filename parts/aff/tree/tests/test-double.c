#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <stdarg.h>
#include "node.h"
#include "coding.h"

int
main(int argc, char *argv[])
{
    int i;
    double in, out;
    uint8_t b[sizeof (double)];

    for (i = 1; i < argc; i++) {
	in = strtod(argv[i], NULL);
	if (aff_encode_double(b, sizeof (b), in) == 0 ||
	    aff_decode_double(&out, b, sizeof (b)) == 0) {
	    printf("codec error\n");
	    return 1;
	}
	printf("%28.14a : %02x%02x%02x%02x%02x%02x%02x%02x : %28.14a\n",
	       in, b[0], b[1], b[2], b[3], b[4], b[5], b[6], b[7], out);
	    
    }
    return 0;
}
