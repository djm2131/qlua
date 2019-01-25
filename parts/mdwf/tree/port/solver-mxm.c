#include <mdwf.h>

/* Solve
 *   M^\dagget M psi = eta
 *
 * with psi as an initial guess
 *
 */ 

#define MAX_OPTIONS (Q(LOG_CG_RESIDUAL)     | \
                     Q(LOG_TRUE_RESIDUAL)   | \
                     Q(FINAL_CG_RESIDUAL))

int
QX(MxM_CG)(struct QX(HalfFermion)          *psi,             /* in/out */
           int                             *out_iterations,
           double                          *out_epsilon,
           const struct Q(Parameters)      *params,
           const struct QX(Gauge)          *gauge,
           const struct QX(HalfFermion)    *eta,
           int                              max_iterations,
           double                           min_epsilon,
           unsigned int                     options)
{
    DECLARE_STATE;
    long long flops = 0;
    long long sent = 0;
    long long received = 0;
    void *ptr = 0;
    size_t ptr_size = 0;
    void *temps = 0;
    struct SUn *U = 0;
    int status = 1;
    struct Fermion *rho_x = 0;
    struct Fermion *pi_x = 0;
    struct Fermion *zeta_x = 0;
    struct Fermion *t0_x = 0;
    struct Fermion *t1_x = 0;
    struct Fermion *t2_x = 0;
    struct Fermion *t0_y = 0;
    double rhs_norm = 0;

    /* check arguments */
    CHECK_ARG0(psi);
    CHECK_ARGn(params, "MxM_CG");
    CHECK_ARGn(gauge, "MxM_CG");
    CHECK_ARGn(eta, "MxM_CG");

    /* setup communication */
    if (q(setup_comm)(state, sizeof (REAL))) {
        return q(set_error)(state, 0, "MxM_CG(): communication setup failed");
    }

    /* allocate locals */
    ptr = qx(allocate_xy_)(state, &ptr_size, &temps,
                          0,  /* header */
                          6,  /* cb_x */
                          1); /* cb_y */
    if (ptr == 0) {
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

    /* clear bits we do not understand */
    options = options & MAX_OPTIONS;

    BEGIN_TIMING(state);
    /* compute the norm of the RHS */
    flops += qx(omp_f_norm)(state, &rhs_norm, state->lat_x->full_size, state->Ls, eta->cb_x);
    QMP_sum_double(&rhs_norm);
    if (options) {
        qx(zprint)(state, "MxM CG", "rhs norm %e normalized epsilon %e",
                   rhs_norm, min_epsilon * rhs_norm);
    }
    
    /* solve */
    status = qx(cg_solver)(psi->cb_x, "MxM CG", out_iterations, out_epsilon,
                           state, params, U,
                           eta->cb_x, NULL,
                           max_iterations, min_epsilon * rhs_norm, options,
                           &flops, &sent, &received,
                           rho_x, pi_x, zeta_x,
                           t0_x, t1_x, t2_x, t0_y);

    END_TIMING(state, flops, sent, received);

    /* handle zero mode properly */
    if (status > 1)
        goto end;

    /* output final residuals if desired */
    if (options) {
        qx(zprint)(state, "MxM CG", "status %d, total iterations %d",
                  status, *out_iterations);
    }
    if (options & (Q(FINAL_CG_RESIDUAL) | Q(LOG_CG_RESIDUAL))) {
        double norm = rhs_norm == 0? 1: rhs_norm;

        qx(zprint)(state, "MxM CG", "solver residual %e normalized %e",
                   *out_epsilon, *out_epsilon / norm);
    }
    if (rhs_norm != 0.0)
        *out_epsilon = *out_epsilon / rhs_norm;

end:
    /* free memory */
    q(free)(state, ptr, ptr_size);
    if (status != 0) {
        q(set_error)(state, 0, "MxM_CG() solver failed to converge");
    }
    return status;
}
