#include <stdio.h>
#include <stdlib.h>
#include <clover.h>

int
main(int argc, char *argv[])
{
    int size;
    int width;
    int f_size;
    int v_size;

    if (argc != 3) {
        fprintf(stderr, "Usage: check-vfsize size width\n");
        return 1;
    }
    size = atoi(argv[1]);
    width = atoi(argv[2]);

    f_size = qx(sizeof_fermion)(size);
    v_size = qx(sizeof_vfermion)(size, width);

    printf("%d %d %d %d  %d %d\n",
           size, width, v_size, f_size, v_size / f_size, v_size % f_size);



    return 0;
}
