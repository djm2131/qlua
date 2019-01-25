#include "clover-test.h"

int lattice[NDIM];

int
main(int argc, char *argv[])
{
    int fpos[NDIM];
    int c, d, ri;
    int gamma;
    int status = 1;
    int mu;
    QDP_DiracFermion *f;
    QDP_DiracFermion *g;

    /* start QDP */
    QDP_initialize(&argc, &argv);

    if (argc != 1 + 2 * NDIM + 4) {
        printf0("ERROR: usage: %s Lx ... x ... c d r/i gamma\n", argv[0]);
        goto end;
    }

    for (mu = 0; mu < NDIM; mu++)
        lattice[mu] = atoi(argv[1 + mu]);

    for (mu = 0; mu < NDIM; mu++)
        fpos[mu] = atoi(argv[1 + NDIM + mu]);
    c = atoi(argv[1 + 2 * NDIM]);
    d = atoi(argv[1 + 2 * NDIM + 1]);
    ri = atoi(argv[1 + 2 * NDIM + 2]);
    gamma = atoi(argv[1 + 2 * NDIM + 3]);
    
    /* set lattice size and create layout */
    QDP_set_latsize(NDIM, lattice);
    QDP_create_layout();
        
    f = QDP_create_D();
    g = QDP_create_D();
    point_fermion(f, fpos, c, d, ri);
    dump_fermion("check-gamma-f", f);
    QDP_D_eq_gamma_times_D(g, f, gamma, QDP_all);
    dump_fermion("check-gamma-g", g);

    QDP_destroy_D(g);
    QDP_destroy_D(f);
    
    status = 0;
end:
    /* shutdown QDP */
    QDP_finalize();
        
    return status;
}
