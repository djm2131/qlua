#include <mdwf.h>

/* Solve
 *   D_dw psi = eta
 *
 * with psi_0 as an initial guess
 *
 */ 

#define MAX_OPTIONS (Q(LOG_CG_RESIDUAL)     | \
                     Q(LOG_TRUE_RESIDUAL)   | \
                     Q(FINAL_CG_RESIDUAL)   | \
                     Q(FINAL_DIRAC_RESIDUAL))

int
QX(DDW_CG)(struct QX(Fermion)          *psi,
           int                         *out_iterations,
           double                      *out_epsilon,
           const struct Q(Parameters)  *params,
           const struct QX(Fermion)    *psi_0,
           const struct QX(Gauge)      *gauge,
           const struct QX(Fermion)    *eta,
           int                          max_iterations,
           double                       min_epsilon,
           unsigned int                 options)
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
    struct Fermion *xi_x = 0;
    struct Fermion *chi_x = 0;
    struct Fermion *rho_x = 0;
    struct Fermion *pi_x = 0;
    struct Fermion *zeta_x = 0;
    struct Fermion *t0_x = 0;
    struct Fermion *t1_x = 0;
    struct Fermion *t2_x = 0;
    struct Fermion *t0_y = 0;
    double dirac_residual = 0;
    double rhs_norm = 0;

    /* check arguments */
    CHECK_ARG0(psi);
    CHECK_ARGn(params, "DDW_CG");
    CHECK_ARGn(psi_0, "DDW_CG");
    CHECK_ARGn(gauge, "DDW_CG");
    CHECK_ARGn(eta, "DDW_CG");

    /* setup communication */
    if (q(setup_comm)(state, sizeof (REAL))) {
        return q(set_error)(state, 0, "DDW_CG(): communication setup failed");
    }

    /* allocate locals */
    ptr = qx(allocate_xy_)(state, &ptr_size, &temps,
                          0,  /* header */
                          8,  /* cb_x */
                          1); /* cb_y */
    if (ptr == 0) {
        return q(set_error)(state, 0, "DDW_CG(): not enough memory");
    }
    U = gauge->data;
    t0_x  = temps;
    t1_x  = temps = qx(step_cb_x)(state, temps);
    t2_x  = temps = qx(step_cb_x)(state, temps);
    xi_x  = temps = qx(step_cb_x)(state, temps);
    chi_x = temps = qx(step_cb_x)(state, temps);
    rho_x = temps = qx(step_cb_x)(state, temps);
    pi_x  = temps = qx(step_cb_x)(state, temps);
    zeta_x= temps = qx(step_cb_x)(state, temps);
    t0_y  = temps = qx(step_cb_x)(state, temps);

    /* clear bits we do not understand */
    options = options & MAX_OPTIONS;

    BEGIN_TIMING(state);
    /* compute the norm of the RHS */
    flops += qx(op_norm2)(&rhs_norm, eta, state);
    if (options) {
        qx(zprint)(state, "DDW CG", "rhs norm %e normalized epsilon %e",
                   rhs_norm, min_epsilon * rhs_norm);
    }
    
    /* precondition */
    qx(cg_precondition)(xi_x, chi_x, state, params,
                        U, psi_0->cb_x, eta->cb_x, eta->cb_y,
                        &flops, &sent, &received,
                        t0_x, t1_x, t0_y);

    /* solve */
    status = qx(cg_solver)(xi_x, "DDW CG", out_iterations, out_epsilon,
                           state, params, U,
                           chi_x, NULL,
                           max_iterations, min_epsilon * rhs_norm, options,
                           &flops, &sent, &received,
                           rho_x, pi_x, zeta_x,
                           t0_x, t1_x, t2_x, t0_y);
    /* handle zero mode properly */
    if (status > 1) {
        END_TIMING(state, flops, sent, received);
        goto end;
    }

    /* inflate */
    qx(cg_inflate)(psi->cb_x, psi->cb_y,
                   state, params, U, eta->cb_y, xi_x,
                   &flops, &sent, &received,
                   t0_x, t0_y);
    
    if (options & Q(FINAL_DIRAC_RESIDUAL)) {
        dirac_residual = qx(cg_dirac_error)(psi->cb_x, psi->cb_y,
                                            state, params, U,
                                            eta->cb_x, eta->cb_y,
                                            &flops, &sent, &received,
                                            t0_x, t1_x, t0_y);
    }

    END_TIMING(state, flops, sent, received);

    /* output final residuals if desired */
    if (options) {
        qx(zprint)(state, "DDW CG", "status %d, total iterations %d",
                  status, *out_iterations);
    }
    if (options & (Q(FINAL_CG_RESIDUAL) | Q(LOG_CG_RESIDUAL))) {
        double norm = rhs_norm == 0? 1: rhs_norm;

        qx(zprint)(state, "DDW CG", "solver residual %e normalized %e",
                   *out_epsilon, *out_epsilon / norm);
    }
    if (options & Q(FINAL_DIRAC_RESIDUAL)) {
        double norm = rhs_norm == 0? 1: rhs_norm;

        qx(zprint)(state, "DDW CG", "Dirac residual %e normalized %e",
                   dirac_residual, dirac_residual / norm);
    }
    if (rhs_norm != 0.0)
        *out_epsilon = *out_epsilon / rhs_norm;

end:
    /* free memory */
    q(free)(state, ptr, ptr_size);
    if (status != 0) {
        q(set_error)(state, 0, "DDW_CG() solver failed to converge");
    }
    return status;
}
