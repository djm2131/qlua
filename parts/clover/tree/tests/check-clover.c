#include "clover-test.h"

int lattice[NDIM];

int
main(int argc, char *argv[])
{
    int fpos[NDIM];
    int d, a, b, ri;
    int status = 1;
    int mu;
    QDP_ColorMatrix *U[NDIM];
    QDP_ColorMatrix *CL[(NDIM * (NDIM - 1)) / 2];

    /* start QDP */
    QDP_initialize(&argc, &argv);

    if (argc != 1 + 2 * NDIM + 4) {
        printf0("ERROR: usage: %s Lx ... x ... d a b r/i\n", argv[0]);
        goto end;
    }

    for (mu = 0; mu < NDIM; mu++)
        lattice[mu] = atoi(argv[1 + mu]);

    for (mu = 0; mu < NDIM; mu++)
        fpos[mu] = atoi(argv[1 + NDIM + mu]);
    d = atoi(argv[1 + 2 * NDIM]);
    a = atoi(argv[1 + 2 * NDIM + 1]);
    b = atoi(argv[1 + 2 * NDIM + 2]);
    ri = atoi(argv[1 + 2 * NDIM + 3]);
    
    /* set lattice size and create layout */
    QDP_set_latsize(NDIM, lattice);
    QDP_create_layout();

    create_Mvector(U, NELEMS(U));
    create_Mvector(CL, NELEMS(CL));

    point_gauge(U, fpos, d, a, b, ri);
    clover(CL, U);

    dump_gauge("check-clover.U", U, NELEMS(U), 1);
    dump_gauge("check-clover.CL", CL, NELEMS(CL), 0);

    destroy_Mvector(U, NELEMS(U));
    destroy_Mvector(CL, NELEMS(CL));
    
    status = 0;
end:
    /* shutdown QDP */
    QDP_finalize();
        
    return status;
}
