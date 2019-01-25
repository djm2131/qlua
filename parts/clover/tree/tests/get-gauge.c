#include "clover-test.h"

int lattice[NDIM];

int
main(int argc, char *argv[])
{
    int status = 1;
    int mu;
    const char *g_name;
    QDP_ColorMatrix *U[NDIM];
    QLA_Real plaq;

    /* start QDP */
    QDP_initialize(&argc, &argv);

    if (argc != 1 + NDIM + 1) {
        printf0("ERROR: usage: %s Lx ... gauge-file\n", argv[0]);
        goto end;
    }

    for (mu = 0; mu < NDIM; mu++)
        lattice[mu] = atoi(argv[1 + mu]);
    g_name = argv[1 + NDIM];
    
    /* set lattice size and create layout */
    QDP_set_latsize(NDIM, lattice);
    QDP_create_layout();
        
    /* allocate the gauge field */
    create_Mvector(U, NELEMS(U));
    
    /* read gauge field */
    if (read_gauge(U, g_name) != 0) {
        printf0("ERROR: read_gauge(%s)\n", g_name);
        goto end;
    }
        
    /* Compute plaquette */
    plaq = plaquette(U);
        
    /* delete the gauge field */
    destroy_Mvector(U, NELEMS(U));
        
    /* Display the value */
    printf0("plaquette{%s} = %g\n", argv[1],
            plaq / (QDP_volume() * QDP_Nc * NDIM * (NDIM - 1) / 2 ));
    
    status = 0;
end:
    /* shutdown QDP */
    QDP_finalize();
        
    return status;
}
