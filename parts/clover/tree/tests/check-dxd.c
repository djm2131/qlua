#include <assert.h>
#include "clover-test.h"
#include "../port/qop-clover.h"

static int lattice[NDIM];
static int network[NDIM];
static int node[NDIM];
static int self;
static int primary;

static QDP_ColorMatrix *U[NDIM];
static QDP_ColorMatrix *C[(NDIM * (NDIM - 1)) / 2];
static QDP_DiracFermion *F[4];

static void
get_vector(int v[], int def, int dim, const int d[])
{
    int i;

    for (i = 0; i < dim && i < NDIM; i++)
        v[i] = d[i];
    for (;i < NDIM; i++)
        v[i] = def;
}

static void
sublattice(int lo[], int hi[], const int node[], void *env)
{
    int i;

    for (i = 0; i < NDIM; i++) {
        lo[i] = (lattice[i] * node[i]) / network[i];
        hi[i] = (lattice[i] * (node[i] + 1)) / network[i];
    }
}

static double
u_reader(int dir, const int pos[NDIM], int a, int b, int re_im, void *e)
{
    QLA_Real xx;
    int n = QDP_node_number(pos);
    int i = QDP_index(pos);
    QLA_ColorMatrix *m = QDP_expose_M(U[dir]);

    assert(n == self);
    if (re_im == 0) {
        QLA_r_eq_Re_c(xx, QLA_elem_M(m[i], a, b));
    } else {
        QLA_r_eq_Im_c(xx, QLA_elem_M(m[i], a, b));
    }

    QDP_reset_M(U[dir]);
    return xx;
}

static double
c_reader(int mu, int nu, const int pos[NDIM], int a, int b, int re_im, void *e)
{
    int xm, xn, d;
    
    for (d = 0, xm = 0; xm < NDIM; xm++) {
        for (xn = xm + 1; xn < NDIM; xn++, d++) {
            if ((xm == mu) && (xn == nu))
                goto found;
        }
    }
    return 0.0;
    
found:
    {
        QLA_Real xx;
        QLA_ColorMatrix *m = QDP_expose_M(C[d]);
        int n = QDP_node_number(pos);
        int i = QDP_index(pos);

        assert(n == self);
        if (re_im == 0) {
            QLA_r_eq_Re_c(xx, QLA_elem_M(m[i], a, b));
        } else {
            QLA_r_eq_Im_c(xx, QLA_elem_M(m[i], a, b));
        }
        QDP_reset_M(C[d]);
        return xx;
    }
}

static double
f_reader(const int pos[NDIM], int c, int d, int re_im, void *env)
{
    QLA_Real xx;
    int n = QDP_node_number(pos);
    int i = QDP_index(pos);
    QDP_DiracFermion *f = (QDP_DiracFermion *)env;
    QLA_DiracFermion *df = QDP_expose_D(f);

    assert(n == self);
    if (re_im == 0) {
        QLA_r_eq_Re_c(xx, QLA_elem_D(df[i], c, d));
    } else {
        QLA_r_eq_Im_c(xx, QLA_elem_D(df[i], c, d));
    }
    QDP_reset_D(f);
    return xx;
}

static void
f_writer(const int pos[4], int c, int d, int re_im, double v, void *env)
{
    int n = QDP_node_number(pos);
    int i = QDP_index(pos);
    QDP_DiracFermion *f = (QDP_DiracFermion *)env;
    QLA_DiracFermion *df = QDP_expose_D(f);

    assert(n == self);
    if (re_im == 0) {
        QLA_real(QLA_elem_D(df[i], c, d)) = v;
    } else {
        QLA_imag(QLA_elem_D(df[i], c, d)) = v;
    }
    QDP_reset_D(f);
}

static void
icoord(QLA_Int *dest, int index)
{
    *dest = index;
}

static void
show_dot(const char *name, QDP_DiracFermion *a, QDP_DiracFermion *b)
{
    QLA_Complex v;

    QDP_c_eq_D_dot_D(&v, a, b, QDP_all);
    printf0(" <%s> = %30.20e %+30.20e\n", name, QLA_real(v), QLA_imag(v));
}

int
main(int argc, char *argv[])
{
    int status = 1;
    int mu, i;
    struct QOP_CLOVER_State *clover_state;
    QDP_Int *I_seed;
    int i_seed;
    QDP_RandomState *state;
    QLA_Real plaq;
    QLA_Real n[NELEMS(F)];
    struct QOP_CLOVER_Gauge *c_g;
    struct QOP_CLOVER_Fermion *c_f[NELEMS(F)];
    double kappa;
    double c_sw;

    /* start QDP */
    QDP_initialize(&argc, &argv);

    if (argc != 1 + NDIM + 3) {
        printf0("ERROR: usage: %s Lx ... seed kappa c_sw\n", argv[0]);
        goto end;
    }

    for (mu = 0; mu < NDIM; mu++) {
        lattice[mu] = atoi(argv[1 + mu]);
    }
    i_seed = atoi(argv[1 + NDIM]);
    kappa = atof(argv[2 + NDIM]);
    c_sw = atof(argv[3 + NDIM]);
    
    /* set lattice size and create layout */
    QDP_set_latsize(NDIM, lattice);
    QDP_create_layout();

    primary = QMP_is_primary_node();
    self = QMP_get_node_number();
    get_vector(network, 1, QMP_get_logical_number_of_dimensions(),
               QMP_get_logical_dimensions());
    get_vector(node, 0, QMP_get_logical_number_of_dimensions(),
               QMP_get_logical_coordinates());
        
    printf0("network: ");
    for (i = 0; i < NDIM; i++)
        printf0(" %d", network[i]);
    printf0("\n");

    printf0("node: ");
    for (i = 0; i < NDIM; i++)
        printf0(" %d", node[i]);
    printf0("\n");

    printf0("kappa: %20.15f\n", kappa);
    printf0("c_sw:  %20.15f\n", c_sw);

    /* allocate the gauge field */
    create_Mvector(U, NELEMS(U));
    create_Mvector(C, NELEMS(C));
    create_Dvector(F, NELEMS(F));
    I_seed = QDP_create_I();
    QDP_I_eq_funci(I_seed, icoord, QDP_all);
    state = QDP_create_S();
    QDP_S_eq_seed_i_I(state, i_seed, I_seed, QDP_all);
    
    for (mu = 0; mu < NELEMS(U); mu++) {
        QDP_M_eq_gaussian_S(U[mu], state, QDP_all);
    }
    
    for (i = 0; i < NELEMS(F); i++) {
        QDP_D_eq_gaussian_S(F[i], state, QDP_all);
    }

    /* build the clovers */
    clover(C, U);

    /* initialize CLOVER */
    if (QOP_CLOVER_init(&clover_state, lattice, network, node, primary,
                        sublattice, NULL)) {
        printf0("CLOVER_init() failed\n");
        goto end;
    }

    if (QOP_CLOVER_import_fermion(&c_f[0], clover_state, f_reader, F[0])) {
        printf0("CLOVER_import_fermion(0) failed\n");
        goto end;
    }

    if (QOP_CLOVER_import_fermion(&c_f[1], clover_state, f_reader, F[1])) {
        printf0("CLOVER_import_fermion(1) failed\n");
        goto end;
    }

    if (QOP_CLOVER_allocate_fermion(&c_f[2], clover_state)) {
        printf0("CLOVER_allocate_fermion(2) failed\n");
        goto end;
    }

    if (QOP_CLOVER_allocate_fermion(&c_f[3], clover_state)) {
        printf0("CLOVER_allocate_fermion(3) failed\n");
        goto end;
    }

    if (QOP_CLOVER_import_gauge(&c_g, clover_state, kappa, c_sw,
                                u_reader, c_reader, NULL)) {
        printf("CLOVER_import_gauge() failed\n");
        goto end;
    }

    QOP_CLOVER_D_operator(c_f[2], c_g, c_f[0]);
    QOP_CLOVER_export_fermion(f_writer, F[2], c_f[2]);

    QOP_CLOVER_D_operator_conjugated(c_f[3], c_g, c_f[1]);
    QOP_CLOVER_export_fermion(f_writer, F[3], c_f[3]);
    
    /* free CLOVER */
    QOP_CLOVER_free_gauge(&c_g);
    for (i = 0; i < NELEMS(c_f); i++)
        QOP_CLOVER_free_fermion(&c_f[i]);

    QOP_CLOVER_fini(&clover_state);

    /* Compute plaquette */
    plaq = plaquette(U);

    /* field norms */
    for (i = 0; i < NELEMS(F); i++)
        QDP_r_eq_norm2_D(&n[i], F[i], QDP_all);
        


    /* Display the values */
    printf0("plaquette = %g\n",
            plaq / (QDP_volume() * QDP_Nc * NDIM * (NDIM - 1) / 2 ));
    for (i = 0; i < NELEMS(F); i++)
        printf0(" |f|^2 [%d] = %20.10e\n", i, (double)(n[i]));

    /* Compute and display <f[1] f[2]> */
    show_dot("1|D0", F[1], F[2]);
    /* Compute and display <f[3] f[0]> */
    show_dot("X1|0", F[3], F[0]);

    QDP_destroy_S(state);
    QDP_destroy_I(I_seed);
    destroy_Mvector(U, NELEMS(U));
    destroy_Mvector(C, NELEMS(C));
    destroy_Dvector(F, NELEMS(F));

    status = 0;
end:
    /* shutdown QDP */
    printf0("end\n");
    QDP_finalize();
        
    return status;
}
