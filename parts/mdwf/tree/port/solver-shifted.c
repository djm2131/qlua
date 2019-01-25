#include <mdwf.h>

/* Solve
 *   (M^\dagget M + s[n]) v_psi[n] = eta
 *     for n = 0 ... count - 1
 *     and
 *   M^\dagger M psi = eta
 *
 *
 */ 

#define MAX_OPTIONS (Q(LOG_CG_RESIDUAL)     | \
                     Q(LOG_TRUE_RESIDUAL)   | \
                     Q(FINAL_CG_RESIDUAL))

int
QX(MxM_SCG)(struct QX(VectorFermion)       *v_psi,
            struct QX(HalfFermion)         *psi,
            int                             *out_iterations,
            double                          *out_epsilon,
            const struct Q(Parameters)      *params,
            const double                     shift[],
            const struct QX(Gauge)          *gauge,
            const struct QX(HalfFermion)    *eta,
            int                              max_iterations,
            double                           min_epsilon,
            unsigned int                     options)
{
    DECLARE_STATE;
    int count = 0;
    long long flops = 0;
    long long sent = 0;
    long long received = 0;
    void *sptr = 0;
    size_t sptr_size = 0;
    double *v = 0;
    double *w = 0;
    double *adn = 0;
    double *bdd = 0;
    void *pptr = 0;
    size_t pptr_size = 0;
    void *temps = 0;
    struct SUn *U = 0;
    int status = 1;
    struct VectorFermion *vpi_x = 0;
    struct Fermion *rho_x = 0;
    struct Fermion *pi_x = 0;
    struct Fermion *zeta_x = 0;
    struct Fermion *t0_x = 0;
    struct Fermion *t1_x = 0;
    struct Fermion *t2_x = 0;
    struct Fermion *t0_y = 0;
    double rhs_norm = 0;

    /* check arguments */
    CHECK_ARG0(v_psi);
    CHECK_ARGn(psi, "MxM_SCG");
    CHECK_ARGn(params, "MxM_SCG");
    CHECK_POINTER(shift, "MxM_SCG");
    CHECK_ARGn(gauge, "MxM_SCG");
    CHECK_ARGn(eta, "MxM_SCG");
    count = v_psi->count;

    /* setup communication */
    if (q(setup_comm)(state, sizeof (REAL))) {
        return q(set_error)(state, 0, "MxM_SCG(): communication setup failed");
    }

    sptr_size = count * 4 * sizeof (double);
    sptr = q(malloc)(state, sptr_size);
    if (sptr == 0) {
        return q(set_error)(state, 0, "MxM_SCG(): not enough memory");
    }
    adn = sptr;
    bdd = adn + count;
    v = bdd + count;
    w = v + count;

    /* allocate locals */
    pptr = qx(allocate_xy_)(state, &pptr_size, &temps,
                           0, /* header */
                           6 + count, /* cb_x */
                           1); /* cb_y */
    if (pptr == 0) {
        q(free)(state, sptr, sptr_size);
        return q(set_error)(state, 0, "MxM_CG(): not enough memory");
    }
    U = gauge->data;
    t0_x   = temps;
    t1_x   = temps = qx(step_cb_x)(state, temps);
    t2_x   = temps = qx(step_cb_x)(state, temps);
    rho_x  = temps = qx(step_cb_x)(state, temps);
    pi_x   = temps = qx(step_cb_x)(state, temps);
    zeta_x = temps = qx(step_cb_x)(state, temps);
    t0_y   = temps = qx(step_cb_x)(state, temps);
    vpi_x  = temps = qx(step_cb_y)(state, temps);

    /* clear bits we do not understand */
    options = options & MAX_OPTIONS;

    BEGIN_TIMING(state);
    /* compute the norm of the RHS */
    flops += qx(omp_f_norm)(state, &rhs_norm, state->lat_x->full_size, state->Ls, eta->cb_x);
    QMP_sum_double(&rhs_norm);
    if (options) {
        qx(zprint)(state, "MxM SCG", "rhs norm %e normalized epsilon %e",
                   rhs_norm, min_epsilon * rhs_norm);
    }
    
    /* solve */
    status = qx(scg_solver)(v_psi->cb_x, psi->cb_x, count, "MxM SCG",
                            out_iterations, out_epsilon,
                            state, params, shift, U, eta->cb_x, 
                            max_iterations, min_epsilon * rhs_norm, options,
                            &flops, &sent, &received,
                            v, w, adn, bdd,
                            rho_x, vpi_x, pi_x, zeta_x,
                            t0_x, t1_x, t2_x, t0_y);

    END_TIMING(state, flops, sent, received);

    /* handle zero mode properly */
    if (status > 1)
        goto end;

    /* output final residuals if desired */
    if (options) {
        qx(zprint)(state, "MxM SCG", "status %d, total iterations %d",
                  status, *out_iterations);
    }
    if (options & (Q(FINAL_CG_RESIDUAL) | Q(LOG_CG_RESIDUAL))) {
        double norm = rhs_norm == 0? 1: rhs_norm;

        qx(zprint)(state, "MxM SCG",
                   "zero shift residual %e normalized %e",
                   *out_epsilon, *out_epsilon / norm);
    }
    if (rhs_norm != 0.0)
        *out_epsilon = *out_epsilon / rhs_norm;

end:
    /* free memory */
    q(free)(state, pptr, pptr_size);
    q(free)(state, sptr, sptr_size);
    if (status != 0) {
        q(set_error)(state, 0, "MxM_SCG() solver failed to converge");
    }
    return status;
}
